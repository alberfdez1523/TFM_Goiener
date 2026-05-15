test_that("clustering genera perfiles y balance diagnostico", {
  profiles <- read_csv_if_exists(path(TABLE_DIR, "cluster_profiles.csv"))
  balance <- read_csv_if_exists(path(TABLE_DIR, "cluster_balance_diagnostics.csv"))

  expect_true(all(c("cluster", "n_usuarios", "pct_usuarios", "median_daily_kWh") %in% names(profiles)))
  expect_true(all(c("cluster", "pct_stage_b", "max_pct_constraint") %in% names(balance)))
  expect_true(any(profiles$cluster == 0))

  stage_b <- balance[isTRUE(balance$is_stage_b) | balance$is_stage_b == "TRUE", , drop = FALSE]
  stage_b <- stage_b[!is.na(stage_b$pct_stage_b), , drop = FALSE]
  expect_gt(nrow(stage_b), 0)
  expect_lte(max(stage_b$pct_stage_b, na.rm = TRUE), CLUSTER_MAX_PCT_PER_CLUSTER + 1e-6)
})

test_that("tabla de validacion de clustering conserva los criterios de seleccion", {
  validation <- read_csv_if_exists(path(TABLE_DIR, "cluster_validation.csv"))
  expect_true(all(c(
    "algo", "k", "silhouette_avg", "balance_entropy",
    "min_cluster_pct", "max_cluster_pct", "passes_size_constraints",
    "selected"
  ) %in% names(validation)))
  expect_equal(sum(validation$selected %in% TRUE), 1)
})

