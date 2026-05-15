test_that("forecast operativo existe y mejora el naive semanal", {
  weather_modes <- read_csv_if_exists(path(TABLE_DIR, "forecast_weather_mode_metrics.csv"))

  expect_true(all(c("expost_weather", "operational_weather", "baseline_no_weather") %in%
                    weather_modes$weather_mode))

  operational <- weather_modes[weather_modes$weather_mode == "operational_weather", , drop = FALSE]
  naive <- weather_modes[weather_modes$model_key == "naive7", , drop = FALSE]

  expect_equal(nrow(operational), 1)
  expect_equal(nrow(naive), 1)
  expect_lt(operational$WAPE, naive$WAPE)
})
