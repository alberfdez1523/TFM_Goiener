test_that("intervalos por cluster tienen calibracion y alertas trazables", {
  calibration <- read_csv_if_exists(path(TABLE_DIR, "forecast_cluster_interval_calibration.csv"))
  alerts <- read_csv_if_exists(path(TABLE_DIR, "forecast_interval_alerts.csv"))
  cluster_prob <- read_csv_if_exists(path(TABLE_DIR, "forecast_cluster_probabilistic_metrics.csv"))

  expect_true(all(c(
    "cluster", "calibration_group", "n_calibration", "qhat",
    "safety_factor", "coverage_val", "coverage_test"
  ) %in% names(calibration)))
  expect_true(all(c("scope", "empirical_coverage", "alert_level") %in% names(alerts)) ||
                nrow(alerts) == 0)

  cluster_level <- cluster_prob
  if (all(c("season", "month") %in% names(cluster_prob))) {
    cluster_level <- cluster_prob[is.na(cluster_prob$season) & is.na(cluster_prob$month), , drop = FALSE]
  }
  expect_gt(nrow(cluster_level), 0)
  expect_gte(min(cluster_level$empirical_coverage, na.rm = TRUE), 85)
})
