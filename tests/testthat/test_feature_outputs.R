suppressWarnings(suppressPackageStartupMessages(library(arrow)))

test_that("user_features contiene las features esperadas para clustering", {
  skip_if_not(file_exists(USER_FEATURES_PARQUET), "Falta user_features.parquet")
  features <- arrow::read_parquet(USER_FEATURES_PARQUET)

  expect_true(all(sprintf("norm_h%02d", 0:23) %in% names(features)))
  expect_true(all(c(
    "ratio_night_day", "ratio_weekend_weekday", "cv_daily",
    "seasonal_amplitude_norm", "zero_day_rate", "low_day_rate",
    "max_month_share", "monthly_entropy", "peak_share",
    "valley_share", "is_residential_strict",
    "morning_kWh_share", "afternoon_kWh_share", "evening_kWh_share",
    "holiday_ratio", "bridge_ratio", "weekday_weekend_peak_shift",
    "low_consumption_spell_rate", "possible_intermittent_home",
    "proxy_autoconsumption_second_home", "ccaa", "density_bucket",
    "climate_zone", "goiener_core_region"
  ) %in% names(features)))
  expect_gt(nrow(features), 1000)
})
