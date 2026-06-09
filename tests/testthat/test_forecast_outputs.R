test_that("forecast diario mejora el naive semanal", {
  leaderboard <- read_csv_if_exists(path(TABLE_DIR, "forecast_leaderboard_daily.csv"))
  xgb <- leaderboard[leaderboard$model == "xgb", , drop = FALSE]
  naive <- leaderboard[leaderboard$model == "snaive7", , drop = FALSE]

  expect_equal(nrow(xgb), 1)
  expect_equal(nrow(naive), 1)
  expect_lt(xgb$WAPE, naive$WAPE)
  expect_lt(xgb$MASE, 1)
})

test_that("intervalo agregado mantiene cobertura minima", {
  intervals <- read_csv_if_exists(path(TABLE_DIR, "forecast_daily_interval_metrics.csv"))
  conformal <- intervals[intervals$variant == "conformal_90", , drop = FALSE]

  expect_equal(nrow(conformal), 1)
  expect_true("coverage" %in% names(conformal))
  expect_gte(conformal$coverage[1], FORECAST_INTERVAL_MIN_COVERAGE)
})

test_that("predicciones principales no estan vacias", {
  daily <- read_csv_if_exists(path(TABLE_DIR, "forecast_daily_predictions.csv"))
  hourly <- read_csv_if_exists(path(TABLE_DIR, "forecast_hourly_predictions.csv"))
  cluster <- read_csv_if_exists(path(TABLE_DIR, "forecast_cluster_predictions.csv"))

  expect_true(all(c("date", "actual", "xgb") %in% names(daily)))
  expect_true(all(c("datetime", "actual", "xgb") %in% names(hourly)))
  expect_true(all(c("date", "cluster", "actual", "pred") %in% names(cluster)))
  expect_gt(nrow(daily), 100)
  expect_gt(nrow(hourly), 1000)
  expect_gt(nrow(cluster), 100)
  expect_false(all(is.na(daily$xgb)))
  expect_false(all(is.na(hourly$xgb)))
  expect_false(all(is.na(cluster$pred)))
})

test_that("forecast excluye cola final invalida por cobertura", {
  audit <- read_csv_if_exists(path(TABLE_DIR, "forecast_target_coverage_audit.csv"))
  daily <- read_csv_if_exists(path(TABLE_DIR, "forecast_daily_predictions.csv"))
  hourly <- read_csv_if_exists(path(TABLE_DIR, "forecast_hourly_predictions.csv"))
  cluster <- read_csv_if_exists(path(TABLE_DIR, "forecast_cluster_predictions.csv"))

  invalid_dates <- as.Date(audit$date[!audit$retained_for_forecast])
  expect_true(as.Date("2024-01-25") %in% invalid_dates)

  daily_dates <- as.Date(daily$date)
  hourly_dates <- as.Date(substr(hourly$datetime, 1, 10))
  cluster_dates <- as.Date(cluster$date)

  expect_false(any(daily_dates %in% invalid_dates))
  expect_false(any(hourly_dates %in% invalid_dates))
  expect_false(any(cluster_dates %in% invalid_dates))

  expect_equal(max(daily_dates), as.Date("2024-01-24"))
  expect_equal(max(hourly_dates), as.Date("2024-01-24"))
  expect_equal(max(cluster_dates), as.Date("2024-01-24"))
})
