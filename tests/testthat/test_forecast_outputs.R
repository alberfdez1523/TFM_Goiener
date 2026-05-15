test_that("forecast agregado mejora el naive semanal", {
  metrics <- read_csv_if_exists(path(TABLE_DIR, "forecast_metrics.csv"))
  xgb <- metrics[metrics$model_key == "xgb", , drop = FALSE]
  naive <- metrics[metrics$model_key == "naive7", , drop = FALSE]

  expect_equal(nrow(xgb), 1)
  expect_equal(nrow(naive), 1)
  expect_lt(xgb$WAPE, naive$WAPE)
  expect_lt(xgb$MASE, 1)
})

test_that("intervalo agregado mantiene cobertura minima", {
  intervals <- read_csv_if_exists(path(TABLE_DIR, "forecast_interval_metrics.csv"))
  expect_true("empirical_coverage" %in% names(intervals))
  expect_gte(intervals$empirical_coverage[1], FORECAST_INTERVAL_MIN_COVERAGE)
})

test_that("predicciones principales no estan vacias", {
  predictions <- read_csv_if_exists(path(TABLE_DIR, "forecast_predictions.csv"))
  expect_true(all(c("date", "actual", "pred_xgb") %in% names(predictions)))
  expect_gt(nrow(predictions), 100)
  expect_false(all(is.na(predictions$pred_xgb)))
})

