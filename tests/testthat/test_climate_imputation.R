suppressWarnings(suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
}))

test_that("capa climatica imputada completa el rango diario del alcance", {
  skip_if_not(file_exists(DAILY_CLIMATE_IMPUTED_PARQUET),
              "Falta daily_climate_imputed.parquet")

  climate <- arrow::read_parquet(DAILY_CLIMATE_IMPUTED_PARQUET)
  climate$cod_provincia <- normalize_province_code(climate$cod_provincia)
  expected_days <- length(seq(
    as.Date(sprintf("%d-01-01", YEAR_MIN)),
    as.Date(sprintf("%d-12-31", YEAR_MAX)),
    by = "day"
  ))

  expect_setequal(sort(unique(climate$cod_provincia)), sort(FOCUS_PROVINCES))

  station_coverage <- climate |>
    count(cod_provincia, indicativo, name = "n_days")
  expect_true(all(station_coverage$n_days == expected_days))

  critical_cols <- c("tmed", "tmax", "tmin", "hdd", "cdd")
  expect_true(all(critical_cols %in% names(climate)))
  expect_false(any(is.na(climate[, critical_cols])))
})

test_that("capa climatica imputada conserva trazabilidad de imputacion", {
  skip_if_not(file_exists(DAILY_CLIMATE_IMPUTED_PARQUET),
              "Falta daily_climate_imputed.parquet")
  climate <- arrow::read_parquet(DAILY_CLIMATE_IMPUTED_PARQUET)

  flag_cols <- c(
    "climate_row_imputed", "climate_any_imputed", "n_climate_values_imputed",
    "tmed_imputed", "tmax_imputed", "tmin_imputed", "prec_imputed",
    "hrMedia_imputed", "sol_imputed", "velmedia_imputed"
  )
  expect_true(all(flag_cols %in% names(climate)))
  expect_true(any(climate$climate_any_imputed))
  expect_gte(max(climate$n_climate_values_imputed, na.rm = TRUE), 1)
})

test_that("auditorias de imputacion climatica quedan exportadas", {
  summary <- read_csv_if_exists(CLIMATE_IMPUTATION_SUMMARY_CSV)
  by_date <- read_csv_if_exists(CLIMATE_IMPUTATION_BY_DATE_CSV)

  expect_true(all(c(
    "cod_provincia", "indicativo", "variable", "n_missing_before",
    "n_imputed", "n_missing_after", "longest_gap_days", "method", "status"
  ) %in% names(summary)))
  expect_true(all(summary$n_missing_after == 0))
  expect_true(any(summary$n_imputed > 0))

  imputed_provinces <- normalize_province_code(summary$cod_provincia[summary$n_imputed > 0])
  expect_true(any(imputed_provinces %in% c("20", "31")))

  expect_true(all(c(
    "cod_provincia", "fecha", "variables_imputed", "climate_row_imputed",
    "climate_any_imputed", "n_climate_values_imputed"
  ) %in% names(by_date)))
  expect_gt(nrow(by_date), 0)
})
