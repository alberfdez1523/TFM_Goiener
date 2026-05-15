#!/usr/bin/env Rscript
# ==============================================================================
# R/06_forecasting/06e_models_cluster.R
#
# Forecast diario por cluster (top-down vs bottom-up).
# Modelos: ETS por cluster + XGBoost por cluster. Reconciliacion bottom-up
# y comparacion contra forecast portfolio agregado.
# Outputs:
#   outputs/tables/forecast_leaderboard_cluster.csv
#   outputs/tables/forecast_cluster_predictions.csv
#   outputs/tables/forecast_reconciliation.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))
source(here::here("R", "_lib", "forecast_metrics.R"))

log_section("PASO 07e: Forecasting por cluster + reconciliacion")
t0 <- proc.time(); set.seed(SEED)

cd <- read_parquet_safe(path(FEATURES_DIR, "cluster_daily_fe.parquet"),
                        "cluster_daily_fe")
pd <- read_parquet_safe(path(FEATURES_DIR, "portfolio_daily_fe.parquet"),
                        "portfolio_daily_fe")

clusters <- sort(unique(cd$cluster))
metrics_list <- list()
preds_list <- list()

feature_cols <- intersect(c(
  "hdd_mean", "cdd_mean", "is_weekend", "dow", "month", "doy",
  "sin_doy", "cos_doy", "sin_dow", "cos_dow",
  paste0("lag", FORECAST_DAILY_LAGS), "roll7", "roll28"
), names(cd))

for (cl in clusters) {
  sub <- cd |> filter(cluster == cl) |> arrange(date)
  train <- sub |> filter(date <= TRAIN_END)
  val   <- sub |> filter(date >= VAL_START & date <= VAL_END)
  test  <- sub |> filter(date >= TEST_START & date <= TEST_END)
  if (nrow(test) < 30) next

  trainv <- bind_rows(train, val) |>
    filter(complete.cases(across(all_of(feature_cols))))
  if (nrow(trainv) < 100) next
  X_tv <- as.matrix(trainv[, feature_cols])
  y_tv <- trainv$kWh_total
  X_test <- as.matrix(test[, feature_cols])
  y_test <- test$kWh_total

  if (requireNamespace("xgboost", quietly = TRUE)) {
    dtv <- xgboost::xgb.DMatrix(X_tv, label = y_tv)
    dtest <- xgboost::xgb.DMatrix(X_test)
    set.seed(SEED + as.integer(cl))
    xgb <- xgboost::xgb.train(
      params = list(objective = "reg:squarederror",
                    eta = 0.05, max_depth = 5,
                    subsample = 0.85, colsample_bytree = 0.85),
      data = dtv, nrounds = 500, verbose = 0
    )
    pred <- predict(xgb, dtest)
    metrics_list[[as.character(cl)]] <- forecast_summary(
      y_test, pred, train$kWh_total, m = 7,
      label = sprintf("xgb_cluster_%d", cl)
    ) |> mutate(cluster = cl)
    preds_list[[as.character(cl)]] <- data.frame(
      date = test$date, cluster = cl, actual = y_test, pred = pred
    )
  }
}

if (length(metrics_list) == 0) {
  warning("Sin clusters validos para forecasting.")
  message(sprintf("07e en %.1f s (sin output)", (proc.time() - t0)[["elapsed"]]))
  quit(save = "no")
}

leaderboard <- bind_rows(metrics_list) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  arrange(cluster)
write_csv_audit(leaderboard, "forecast_leaderboard_cluster.csv")
print(leaderboard)

preds_all <- bind_rows(preds_list)
write_csv_audit(preds_all, "forecast_cluster_predictions.csv")

# Bottom-up reconciliation vs top-down portfolio.
bu <- preds_all |> group_by(date) |>
  summarise(pred_bu = sum(pred), actual_sum = sum(actual), .groups = "drop")

# Top-down: use forecast_daily.rds best predictions if exists.
td_pred_file <- path(MODEL_DIR, "forecast_daily.rds")
td_df <- NULL
if (file_exists(td_pred_file)) {
  td <- readRDS(td_pred_file)
  td_df <- td$predictions |>
    select(date, actual_total = actual,
           pred_td = !!sym(td$best))
}

recon <- bu
if (!is.null(td_df)) recon <- recon |> left_join(td_df, by = "date")

recon_metrics <- data.frame(
  variant = c("bottom_up", if (!is.null(td_df)) "top_down" else NULL),
  MAE = c(mae(recon$actual_sum, recon$pred_bu),
          if (!is.null(td_df)) mae(recon$actual_total, recon$pred_td) else NULL),
  WAPE = c(wape(recon$actual_sum, recon$pred_bu),
           if (!is.null(td_df)) wape(recon$actual_total, recon$pred_td) else NULL),
  EUR_dev = c(eur_deviation_cost(recon$actual_sum, recon$pred_bu),
              if (!is.null(td_df)) eur_deviation_cost(recon$actual_total,
                                                       recon$pred_td) else NULL)
) |> mutate(across(where(is.numeric), \(x) round(x, 3)))

write_csv_audit(recon_metrics, "forecast_reconciliation.csv")
print(recon_metrics)

message(sprintf("07e en %.1f s", (proc.time() - t0)[["elapsed"]]))
