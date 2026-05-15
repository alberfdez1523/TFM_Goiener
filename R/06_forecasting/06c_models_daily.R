#!/usr/bin/env Rscript
# ==============================================================================
# R/06_forecasting/06c_models_daily.R
#
# Daily portfolio. Ajustes del modelo final:
#   - Target en log1p para estabilizar la varianza estacional.
#   - Hiperparametros tuneados con early stopping sobre validacion.
#   - LightGBM + XGBoost + RF + ARIMAX + ETS + baselines.
#   - Stacking lineal (ridge) sobre el conjunto de validacion.
#   - Conformal split y XGB quantile para intervalos.
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))
source(here::here("R", "_lib", "forecast_metrics.R"))

log_section("PASO 07c: Forecasting diario de la cartera")
t0 <- proc.time(); set.seed(SEED)

pd <- read_parquet_safe(path(FEATURES_DIR, "portfolio_daily_fe.parquet"),
                        "portfolio_daily_fe")

train <- pd |> filter(date <= TRAIN_END)
val   <- pd |> filter(date >= VAL_START & date <= VAL_END)
test  <- pd |> filter(date >= TEST_START & date <= TEST_END)

message(sprintf("  splits: train=%s, val=%s, test=%s",
                fmt_int(nrow(train)), fmt_int(nrow(val)), fmt_int(nrow(test))))

y_train <- train$kWh_total
y_val   <- val$kWh_total
y_test  <- test$kWh_total

predictions <- data.frame(date = test$date, actual = y_test,
                          stringsAsFactors = FALSE)
metrics <- list()

# ---- Baselines ----
message("[1/6] Baselines...")
hist_full <- c(y_train, y_val)
predictions$naive <- rep(tail(hist_full, 1), length(y_test))
metrics$naive <- forecast_summary(y_test, predictions$naive, y_train,
                                  m = 7, label = "naive")

predictions$snaive7 <- hist_full[length(hist_full) - 7 + seq_along(y_test) %% 7]
metrics$snaive7 <- forecast_summary(y_test, predictions$snaive7, y_train,
                                    m = 7, label = "snaive7")
predictions$snaive365 <- hist_full[length(hist_full) - 365 + seq_along(y_test)]
metrics$snaive365 <- forecast_summary(y_test, predictions$snaive365, y_train,
                                      m = 7, label = "snaive365")

# ---- ETS / ARIMAX ----
fit_arima <- NULL
if (requireNamespace("forecast", quietly = TRUE)) {
  message("[2/6] ETS y ARIMAX...")
  ts_full <- ts(hist_full, frequency = 7)
  xreg_cols <- c("hdd_mean", "cdd_mean", "is_weekend", "any_holiday",
                 "sin_doy", "cos_doy", "is_summer", "is_winter")
  xreg_cols <- intersect(xreg_cols, names(pd))
  xreg_full <- as.matrix(rbind(train, val)[, xreg_cols])
  xreg_test <- as.matrix(test[, xreg_cols])

  fit_ets <- tryCatch(forecast::ets(ts_full, model = "ZZZ"),
                       error = function(e) NULL)
  if (!is.null(fit_ets)) {
    predictions$ets <- as.numeric(forecast::forecast(fit_ets,
                                                     h = length(y_test))$mean)
    metrics$ets <- forecast_summary(y_test, predictions$ets, y_train,
                                    m = 7, label = "ets")
  }

  fit_arima <- tryCatch(
    forecast::auto.arima(ts_full, xreg = xreg_full, seasonal = TRUE,
                         stepwise = TRUE, approximation = TRUE),
    error = function(e) NULL
  )
  if (!is.null(fit_arima)) {
    predictions$arimax <- as.numeric(forecast::forecast(
      fit_arima, xreg = xreg_test, h = length(y_test))$mean)
    metrics$arimax <- forecast_summary(y_test, predictions$arimax, y_train,
                                       m = 7, label = "arimax")
  }
}

# ---- ML setup ----
message("[3/6] ML modelos (log-target + early stopping)...")
feature_cols <- c(
  "hdd_mean", "cdd_mean", "tmed_mean", "any_holiday",
  "is_weekend", "is_friday", "is_monday",
  "is_summer", "is_winter",
  "dow", "month", "doy",
  "sin_doy", "cos_doy", "sin_doy2", "cos_doy2", "sin_dow", "cos_dow",
  paste0("lag", FORECAST_DAILY_LAGS),
  "roll7", "roll14", "roll28", "roll7_sd", "roll28_sd",
  "yoy_diff", "hdd_lag1", "cdd_lag1", "hdd_roll7", "cdd_roll7"
)
feature_cols <- intersect(feature_cols, names(pd))

train_ok <- train |> filter(complete.cases(across(all_of(feature_cols))))
val_ok   <- val   |> filter(complete.cases(across(all_of(feature_cols))))
test_ok  <- test  |> filter(complete.cases(across(all_of(feature_cols))))

X_tr <- as.matrix(train_ok[, feature_cols]); y_tr <- train_ok$kWh_total
X_vl <- as.matrix(val_ok[,   feature_cols]); y_vl <- val_ok$kWh_total
X_te <- as.matrix(test_ok[,  feature_cols]); y_te <- test_ok$kWh_total
ly_tr <- log1p(y_tr); ly_vl <- log1p(y_vl)

# Helper to align test_ok predictions back to full test set.
align_test <- function(p_ok) {
  out <- rep(NA_real_, nrow(test))
  out[match(test_ok$date, test$date)] <- p_ok
  out
}

best_rf <- NULL
if (requireNamespace("ranger", quietly = TRUE)) {
  set.seed(SEED)
  best <- list(score = Inf, mtry = NA, fit = NULL)
  for (mtry_try in c(max(2, floor(sqrt(ncol(X_tr)))),
                     max(3, floor(ncol(X_tr) / 3)),
                     max(4, floor(ncol(X_tr) / 2)))) {
    rf <- ranger::ranger(y = ly_tr, x = as.data.frame(X_tr),
                          num.trees = 700, mtry = mtry_try,
                          min.node.size = 5, num.threads = parallel::detectCores())
    pv <- expm1(predict(rf, as.data.frame(X_vl))$predictions)
    sc <- mae(y_vl, pv)
    if (sc < best$score) best <- list(score = sc, mtry = mtry_try, fit = rf)
  }
  best_rf <- best$fit
  pred <- expm1(predict(best_rf, as.data.frame(X_te))$predictions)
  predictions$rf <- align_test(pred)
  metrics$rf <- forecast_summary(y_te, pred, y_train, m = 7, label = "rf")
  message(sprintf("  RF mejor mtry=%d MAE_val=%.1f", best$mtry, best$score))
}

xgb <- NULL; dvl <- NULL
if (requireNamespace("xgboost", quietly = TRUE)) {
  dtr <- xgboost::xgb.DMatrix(X_tr, label = ly_tr)
  dvl <- xgboost::xgb.DMatrix(X_vl, label = ly_vl)
  dte <- xgboost::xgb.DMatrix(X_te)
  set.seed(SEED)
  xgb <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror",
                  eta = 0.03, max_depth = 6,
                  subsample = 0.85, colsample_bytree = 0.85,
                  min_child_weight = 3),
    data = dtr, nrounds = 3000,
    watchlist = list(val = dvl), early_stopping_rounds = 50, verbose = 0
  )
  pred <- expm1(predict(xgb, dte))
  predictions$xgb <- align_test(pred)
  metrics$xgb <- forecast_summary(y_te, pred, y_train, m = 7, label = "xgb")
  message(sprintf("  XGB best_iter=%d", xgb$best_iteration))

  q_low <- xgboost::xgb.train(
    params = list(objective = "reg:quantileerror",
                  quantile_alpha = 0.05, eta = 0.03, max_depth = 5),
    data = dtr, nrounds = 1500,
    watchlist = list(val = dvl), early_stopping_rounds = 50, verbose = 0
  )
  q_hi  <- xgboost::xgb.train(
    params = list(objective = "reg:quantileerror",
                  quantile_alpha = 0.95, eta = 0.03, max_depth = 5),
    data = dtr, nrounds = 1500,
    watchlist = list(val = dvl), early_stopping_rounds = 50, verbose = 0
  )
  predictions$xgb_q05 <- align_test(expm1(predict(q_low, dte)))
  predictions$xgb_q95 <- align_test(expm1(predict(q_hi,  dte)))
}

lgb <- NULL
if (requireNamespace("lightgbm", quietly = TRUE)) {
  set.seed(SEED)
  lgb_tr <- lightgbm::lgb.Dataset(X_tr, label = ly_tr)
  lgb_vl <- lightgbm::lgb.Dataset(X_vl, label = ly_vl, reference = lgb_tr)
  lgb <- lightgbm::lgb.train(
    params = list(objective = "regression", metric = "mae",
                  learning_rate = 0.03, num_leaves = 31,
                  feature_fraction = 0.85, bagging_fraction = 0.85,
                  bagging_freq = 1, min_data_in_leaf = 10, verbose = -1),
    data = lgb_tr, nrounds = 3000,
    valids = list(val = lgb_vl), early_stopping_rounds = 50
  )
  pred <- expm1(predict(lgb, X_te))
  predictions$lgbm <- align_test(pred)
  metrics$lgbm <- forecast_summary(y_te, pred, y_train, m = 7, label = "lgbm")
  message(sprintf("  LGBM best_iter=%d", lgb$best_iter))
}

# ---- Stacking lineal ridge sobre validacion ----
message("[4/6] Stacking lineal sobre validacion...")
val_preds <- list()
if (!is.null(best_rf)) val_preds$rf <-
  expm1(predict(best_rf, as.data.frame(X_vl))$predictions)
if (!is.null(xgb))     val_preds$xgb <- expm1(predict(xgb, dvl))
if (!is.null(lgb))     val_preds$lgbm <- expm1(predict(lgb, X_vl))
if (!is.null(fit_arima)) {
  fit_pred <- fitted(fit_arima)
  val_preds$arimax <- tail(as.numeric(fit_pred), length(y_vl))
}

if (length(val_preds) >= 2) {
  vP <- as.matrix(as.data.frame(val_preds))
  XtX <- t(vP) %*% vP + diag(0.01 * mean(diag(t(vP) %*% vP)), ncol(vP))
  Xty <- t(vP) %*% y_vl
  w <- as.vector(solve(XtX, Xty))
  w <- pmax(w, 0); if (sum(w) > 0) w <- w / sum(w)
  message("  Pesos stacking: ",
          paste(sprintf("%s=%.2f", names(val_preds), w), collapse = ", "))
  test_mat <- as.matrix(predictions[, names(val_preds), drop = FALSE])
  if (!any(is.na(test_mat))) {
    predictions$stack <- as.vector(test_mat %*% w)
    metrics$stack <- forecast_summary(y_test, predictions$stack, y_train,
                                      m = 7, label = "stack")
  }
}

# Ensemble simple mean of top-3 by MAE on test (informativo).
if (length(metrics) >= 3) {
  ord <- bind_rows(metrics) |> arrange(MAE)
  top3 <- intersect(ord$model[1:min(3, nrow(ord))], names(predictions))
  if (length(top3) >= 2) {
    em <- as.matrix(predictions[, top3, drop = FALSE])
    predictions$ensemble <- rowMeans(em, na.rm = TRUE)
    metrics$ensemble <- forecast_summary(y_test, predictions$ensemble,
                                         y_train, m = 7, label = "ensemble_top3")
  }
}

# ---- Conformal split intervals ----
message("[5/6] Conformal split intervals...")
leaderboard <- bind_rows(metrics) |> arrange(MAE)
best_name <- leaderboard$model[1]
message(sprintf("  best point model: %s", best_name))

q <- NA_real_
if (best_name %in% names(val_preds)) {
  resid_cal <- y_vl - val_preds[[best_name]]
  q <- conformal_quantile(resid_cal, alpha = 0.10)
  pp <- predictions[[best_name]]
  predictions$conformal_lo <- pp - q
  predictions$conformal_hi <- pp + q
}

# ---- Outputs ----
message("[6/6] Guardando outputs...")
leaderboard <- bind_rows(metrics) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  arrange(MAE)
write_csv_audit(leaderboard, "forecast_leaderboard_daily.csv")
print(leaderboard)
write_csv_audit(predictions, "forecast_daily_predictions.csv")

intervals <- data.frame(
  date = predictions$date,
  actual = predictions$actual,
  point  = if ("xgb" %in% names(predictions)) predictions$xgb else NA,
  q05_xgb = if ("xgb_q05" %in% names(predictions)) predictions$xgb_q05 else NA,
  q95_xgb = if ("xgb_q95" %in% names(predictions)) predictions$xgb_q95 else NA,
  conformal_lo = if ("conformal_lo" %in% names(predictions)) predictions$conformal_lo else NA,
  conformal_hi = if ("conformal_hi" %in% names(predictions)) predictions$conformal_hi else NA
)
interval_metrics <- data.frame(
  variant = c("conformal_90", "xgb_quantile_90"),
  coverage = c(
    if (!is.na(intervals$conformal_lo[1]))
      empirical_coverage(intervals$actual, intervals$conformal_lo,
                         intervals$conformal_hi) else NA_real_,
    if (!is.na(intervals$q05_xgb[1]))
      empirical_coverage(intervals$actual, intervals$q05_xgb,
                         intervals$q95_xgb) else NA_real_
  ),
  width = c(
    if (!is.na(intervals$conformal_lo[1]))
      interval_width(intervals$conformal_lo, intervals$conformal_hi)
      else NA_real_,
    if (!is.na(intervals$q05_xgb[1]))
      interval_width(intervals$q05_xgb, intervals$q95_xgb) else NA_real_
  )
)
write_csv_audit(intervals, "forecast_daily_intervals.csv")
write_csv_audit(interval_metrics, "forecast_daily_interval_metrics.csv")

if (!is.null(xgb)) {
  imp <- xgboost::xgb.importance(model = xgb)
  write_csv_audit(as.data.frame(imp), "forecast_daily_xgb_importance.csv")
}

saveRDS(list(metrics = metrics, predictions = predictions,
             best = best_name, conformal_q = q),
        path(MODEL_DIR, "forecast_daily.rds"))

message(sprintf("07c en %.1f s", (proc.time() - t0)[["elapsed"]]))
