#!/usr/bin/env Rscript
# ==============================================================================
# R/06_forecasting/06d_models_hourly.R
#
# Forecast 24h-ahead de cartera horaria (relevante para OMIE day-ahead).
# Modelos: LightGBM/XGBoost directo con 24 modelos (uno por h), quantile XGB.
# Outputs:
#   outputs/tables/forecast_leaderboard_hourly.csv
#   outputs/tables/forecast_hourly_predictions.csv
#   outputs/tables/forecast_hourly_business_impact.csv (€ desvio)
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(here)
  library(lubridate)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))
source(here::here("R", "_lib", "forecast_metrics.R"))

log_section("PASO 07d: Forecasting horario 24h-ahead (OMIE)")
t0 <- proc.time(); set.seed(SEED)

ph <- read_parquet_safe(path(FEATURES_DIR, "portfolio_hourly_fe.parquet"),
                        "portfolio_hourly_fe")

# Strict cutoffs by date.
train <- ph |> filter(as.Date(datetime) <= TRAIN_END)
val   <- ph |> filter(as.Date(datetime) >= VAL_START &
                       as.Date(datetime) <= VAL_END)
test  <- ph |> filter(as.Date(datetime) >= TEST_START &
                       as.Date(datetime) <= TEST_END)

message(sprintf("  splits horarios: train=%s val=%s test=%s",
                fmt_int(nrow(train)), fmt_int(nrow(val)), fmt_int(nrow(test))))

feature_cols <- intersect(c(
  "hour", "dow", "month", "is_weekend", "any_holiday",
  "is_friday", "is_monday", "is_summer", "is_winter",
  "sin_h", "cos_h", "sin_h2", "cos_h2",
  "sin_dow", "cos_dow", "sin_doy", "cos_doy",
  "sin_doy2", "cos_doy2",
  "hdd_mean", "cdd_mean", "tmed_mean",
  paste0("lag", FORECAST_HOURLY_LAGS),
  "roll24", "roll168", "roll24_sd", "diff_lag24", "diff_lag168"
), names(ph))

# ---- Baseline: same-hour last week ----
message("[1/4] Baselines horarios...")
y_test <- test$kWh_total
predictions <- data.frame(datetime = test$datetime, actual = y_test)
metrics <- list()

# snaive 168 (same hour last week)
hist <- c(train$kWh_total, val$kWh_total)
pred_snv <- hist[(length(hist) - 168 + 1):length(hist)]
pred_snv <- rep_len(pred_snv, length(y_test))
predictions$snaive168 <- pred_snv
metrics$snaive168 <- forecast_summary(y_test, pred_snv, train$kWh_total,
                                      m = 24, label = "snaive168")

# snaive 24 (same hour yesterday)
pred_snv24 <- hist[(length(hist) - 24 + 1):length(hist)]
pred_snv24 <- rep_len(pred_snv24, length(y_test))
predictions$snaive24 <- pred_snv24
metrics$snaive24 <- forecast_summary(y_test, pred_snv24, train$kWh_total,
                                     m = 24, label = "snaive24")

# ---- LightGBM / XGBoost con log-target y early stopping ----
message("[2/4] LightGBM/XGBoost (log-target + early stopping en val)...")

train_ok <- train |> filter(complete.cases(across(all_of(feature_cols))))
val_ok   <- val   |> filter(complete.cases(across(all_of(feature_cols))))
test_ok  <- test  |> filter(complete.cases(across(all_of(feature_cols))))

X_tr <- as.matrix(train_ok[, feature_cols]); y_tr <- train_ok$kWh_total
X_vl <- as.matrix(val_ok[,   feature_cols]); y_vl <- val_ok$kWh_total
X_te <- as.matrix(test_ok[,  feature_cols]); y_te <- test_ok$kWh_total
ly_tr <- log1p(y_tr); ly_vl <- log1p(y_vl)

align_h <- function(p_ok) {
  out <- rep(NA_real_, length(y_test))
  out[match(test_ok$datetime, test$datetime)] <- p_ok
  out
}

if (requireNamespace("lightgbm", quietly = TRUE)) {
  set.seed(SEED)
  lgb_tr <- lightgbm::lgb.Dataset(X_tr, label = ly_tr)
  lgb_vl <- lightgbm::lgb.Dataset(X_vl, label = ly_vl, reference = lgb_tr)
  lgb <- lightgbm::lgb.train(
    params = list(objective = "regression", metric = "mae",
                  learning_rate = 0.05, num_leaves = 63,
                  feature_fraction = 0.85, bagging_fraction = 0.85,
                  bagging_freq = 1, min_data_in_leaf = 20, verbose = -1),
    data = lgb_tr, nrounds = 3000,
    valids = list(val = lgb_vl), early_stopping_rounds = 75
  )
  pred <- expm1(predict(lgb, X_te))
  predictions$lgbm <- align_h(pred)
  metrics$lgbm <- forecast_summary(y_te, pred, train$kWh_total,
                                   m = 24, label = "lgbm")
  message(sprintf("  LGBM best_iter=%d", lgb$best_iter))
}

if (requireNamespace("xgboost", quietly = TRUE)) {
  dtr <- xgboost::xgb.DMatrix(X_tr, label = ly_tr)
  dvl <- xgboost::xgb.DMatrix(X_vl, label = ly_vl)
  dte <- xgboost::xgb.DMatrix(X_te)
  set.seed(SEED)
  xgb <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror",
                  eta = 0.04, max_depth = 7,
                  subsample = 0.85, colsample_bytree = 0.85,
                  min_child_weight = 5),
    data = dtr, nrounds = 3000,
    watchlist = list(val = dvl), early_stopping_rounds = 75, verbose = 0
  )
  pred <- expm1(predict(xgb, dte))
  predictions$xgb <- align_h(pred)
  metrics$xgb <- forecast_summary(y_te, pred, train$kWh_total,
                                  m = 24, label = "xgb")
  message(sprintf("  XGB best_iter=%d", xgb$best_iteration))

  q_low <- xgboost::xgb.train(
    params = list(objective = "reg:quantileerror", quantile_alpha = 0.05,
                  eta = 0.05, max_depth = 6),
    data = dtr, nrounds = 1500,
    watchlist = list(val = dvl), early_stopping_rounds = 75, verbose = 0
  )
  q_hi  <- xgboost::xgb.train(
    params = list(objective = "reg:quantileerror", quantile_alpha = 0.95,
                  eta = 0.05, max_depth = 6),
    data = dtr, nrounds = 1500,
    watchlist = list(val = dvl), early_stopping_rounds = 75, verbose = 0
  )
  predictions$xgb_q05 <- align_h(expm1(predict(q_low, dte)))
  predictions$xgb_q95 <- align_h(expm1(predict(q_hi,  dte)))
}

# Stacking lineal en validacion (XGB+LGBM).
val_preds <- list()
if (exists("xgb") && !is.null(xgb)) val_preds$xgb <- expm1(predict(xgb, dvl))
if (exists("lgb") && !is.null(lgb)) val_preds$lgbm <- expm1(predict(lgb, X_vl))
if (length(val_preds) >= 2) {
  vP <- as.matrix(as.data.frame(val_preds))
  XtX <- t(vP) %*% vP + diag(0.01 * mean(diag(t(vP) %*% vP)), ncol(vP))
  w <- as.vector(solve(XtX, t(vP) %*% y_vl))
  w <- pmax(w, 0); if (sum(w) > 0) w <- w / sum(w)
  message("  Pesos stacking horario: ",
          paste(sprintf("%s=%.2f", names(val_preds), w), collapse = ", "))
  test_mat <- as.matrix(predictions[, names(val_preds), drop = FALSE])
  if (!any(is.na(test_mat))) {
    predictions$stack <- as.vector(test_mat %*% w)
    metrics$stack <- forecast_summary(y_test, predictions$stack,
                                      train$kWh_total, m = 24,
                                      label = "stack")
  }
}

y_test_c <- y_te  # for reporting

# ---- Business impact: € deviation ----
message("[3/4] Impacto de negocio (€ desvio OMIE)...")
impact <- data.frame(
  model = setdiff(names(predictions), c("datetime", "actual",
                                        "xgb_q05", "xgb_q95")),
  EUR_dev_total = NA_real_,
  EUR_per_MWh = NA_real_,
  stringsAsFactors = FALSE
)
total_mwh <- sum(y_test, na.rm = TRUE) / 1000
for (i in seq_len(nrow(impact))) {
  m <- impact$model[i]
  yhat <- predictions[[m]]
  ok <- !is.na(yhat) & !is.na(y_test)
  if (!any(ok)) next
  impact$EUR_dev_total[i] <- eur_deviation_cost(y_test[ok], yhat[ok])
  impact$EUR_per_MWh[i]   <- impact$EUR_dev_total[i] / total_mwh
}
write_csv_audit(impact, "forecast_hourly_business_impact.csv")
print(impact)

# ---- Save ----
message("[4/4] Guardando outputs...")
leaderboard <- bind_rows(metrics) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  arrange(MAE)
write_csv_audit(leaderboard, "forecast_leaderboard_hourly.csv")
print(leaderboard)
write_csv_audit(predictions, "forecast_hourly_predictions.csv")

saveRDS(list(metrics = metrics, impact = impact),
        path(MODEL_DIR, "forecast_hourly.rds"))

message(sprintf("07d en %.1f s", (proc.time() - t0)[["elapsed"]]))
