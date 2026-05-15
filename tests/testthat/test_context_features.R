test_that("contexto provincial es agregado y tiene claves unicas", {
  skip_if_not(file_exists(PROVINCE_CONTEXT_CSV), "Falta province_context.csv")
  context <- read.csv(PROVINCE_CONTEXT_CSV, stringsAsFactors = FALSE)

  expect_true(all(c(
    "cod_provincia", "ccaa", "coastal_flag", "density_bucket",
    "climate_zone", "goiener_core_region"
  ) %in% names(context)))
  expect_equal(nrow(context), length(unique(context$cod_provincia)))

  forbidden <- c("cups", "user_id", "dni", "nombre", "direccion",
                 "renta", "income", "household", "vivienda")
  expect_false(any(tolower(names(context)) %in% forbidden))
})

test_that("auditoria de contexto queda exportada", {
  audit <- read_csv_if_exists(path(TABLE_DIR, "context_feature_audit.csv"))
  expect_true(all(c("feature", "n_users", "pct_missing", "privacy_level") %in% names(audit)))
  expect_true(all(audit$privacy_level == "provincial_aggregate_no_personal_data"))
})
