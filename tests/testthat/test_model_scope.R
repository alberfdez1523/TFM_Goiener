suppressWarnings(suppressPackageStartupMessages(library(arrow)))

test_that("auditoria de alcance top 5 es coherente", {
  audit <- read_csv_if_exists(MODEL_SCOPE_AUDIT_CSV)
  audit$cod_provincia <- normalize_province_code(audit$cod_provincia)
  audit$in_model_scope <- as.logical(audit$in_model_scope)

  expect_setequal(audit$cod_provincia[audit$in_model_scope], FOCUS_PROVINCES)
  expect_equal(sum(audit$in_model_scope), length(FOCUS_PROVINCES))
  expect_gt(unique(audit$scope_pct_total)[1], 85)
  expect_gt(unique(audit$scope_user_total)[1], 25000)
  expect_gt(unique(audit$excluded_users)[1], 0)
})

test_that("clima y datasets de modelado solo contienen provincias top 5", {
  skip_if_not(file_exists(DAILY_CLIMATE_PARQUET), "Falta daily_climate.parquet")
  climate <- arrow::read_parquet(DAILY_CLIMATE_PARQUET)
  expect_setequal(sort(unique(climate$cod_provincia)), sort(FOCUS_PROVINCES))

  skip_if_not(file_exists(DAILY_CLIMATE_IMPUTED_PARQUET),
              "Falta daily_climate_imputed.parquet")
  climate_imputed <- arrow::read_parquet(DAILY_CLIMATE_IMPUTED_PARQUET)
  climate_imputed$cod_provincia <- normalize_province_code(climate_imputed$cod_provincia)
  expect_setequal(sort(unique(climate_imputed$cod_provincia)), sort(FOCUS_PROVINCES))

  skip_if_not(file_exists(DAILY_WITH_CLIMATE), "Falta daily_with_climate.parquet")
  daily_model <- arrow::read_parquet(DAILY_WITH_CLIMATE)
  expect_true(all(daily_model$cod_provincia %in% FOCUS_PROVINCES))
  expect_true(all(c("climate_any_imputed", "n_climate_values_imputed") %in%
                    names(daily_model)))
  expect_false(any(is.na(daily_model$tmed)))

  skip_if_not(file.exists(USER_FEATURES_PARQUET), "Falta user_features.parquet")
  features <- arrow::read_parquet(USER_FEATURES_PARQUET)
  expect_true(all(features$cod_provincia %in% FOCUS_PROVINCES))

  skip_if_not(file.exists(USER_CLUSTERS_PARQUET), "Falta user_clusters.parquet")
  clusters <- arrow::read_parquet(USER_CLUSTERS_PARQUET)
  expect_true(all(clusters$cod_provincia %in% FOCUS_PROVINCES))
})
