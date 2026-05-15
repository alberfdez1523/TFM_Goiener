#!/usr/bin/env Rscript
# ==============================================================================
# R/04_features/04b_climate_sensitivity.R
#
# Enriquece user_features.parquet con coeficientes de regresion por usuario
# (beta_HDD, beta_CDD, R2) usando daily_with_climate.parquet. La salida es
# user_features.parquet, consumida por R/05_clustering/.
#
# Idempotente: si user_features.parquet existe y es mas reciente que las
# entradas, no hace nada salvo que se invoque con --force.
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(duckdb)
  library(DBI)
  library(glue)
  library(fs)
  library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 04b: Sensibilidad climatica por usuario")

t0 <- proc.time()
set.seed(SEED)

stopifnot(
  "Falta user_features_base.parquet" = file_exists(USER_FEATURES_BASE_PARQUET),
  "Falta daily_with_climate.parquet" = file_exists(DAILY_WITH_CLIMATE)
)

# 1. Per-user OLS via DuckDB regr_slope / regr_intercept / regr_r2.
con <- connect_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

daily_abs <- path_abs(DAILY_WITH_CLIMATE) |> path_norm()

message("[1/3] Calculando regresiones por usuario (kWh ~ HDD + CDD)...")
betas <- dbGetQuery(con, glue("
  WITH base AS (
    SELECT user_id,
           CAST(daily_kWh AS DOUBLE) AS y,
           CAST(hdd AS DOUBLE) AS hdd,
           CAST(cdd AS DOUBLE) AS cdd
    FROM read_parquet('{daily_abs}')
    WHERE daily_kWh IS NOT NULL AND hdd IS NOT NULL AND cdd IS NOT NULL
  ),
  per_user AS (
    SELECT user_id,
           COUNT(*) AS n_obs_climate,
           AVG(y) AS y_mean,
           VAR_SAMP(y) AS y_var,
           regr_slope(y, hdd) AS beta_hdd_uni,
           regr_slope(y, cdd) AS beta_cdd_uni,
           regr_r2(y, hdd) AS r2_hdd_only,
           regr_r2(y, cdd) AS r2_cdd_only
    FROM base
    GROUP BY user_id
    HAVING COUNT(*) >= 60
  )
  SELECT * FROM per_user
"))

message(sprintf("  Usuarios con regresion climatica valida: %s",
                fmt_int(nrow(betas))))

# 2. Joint OLS via in-memory chunking only for users in scope.
message("[2/3] Calculando regresion conjunta (kWh ~ HDD + CDD) por usuario...")
features_legacy <- read_parquet_safe(USER_FEATURES_BASE_PARQUET, "user_features_base.parquet")
ids_scope <- features_legacy$user_id

# Limit to those in scope.
betas <- betas |> filter(user_id %in% ids_scope)

joint_fit <- function(chunk_ids) {
  ids_sql <- paste0("'", chunk_ids, "'", collapse = ",")
  df <- dbGetQuery(con, glue("
    SELECT user_id, CAST(daily_kWh AS DOUBLE) AS y,
           CAST(hdd AS DOUBLE) AS hdd, CAST(cdd AS DOUBLE) AS cdd
    FROM read_parquet('{daily_abs}')
    WHERE user_id IN ({ids_sql})
      AND daily_kWh IS NOT NULL AND hdd IS NOT NULL AND cdd IS NOT NULL
  "))
  if (nrow(df) == 0) return(NULL)
  out <- df |>
    group_by(user_id) |>
    summarise(
      beta_hdd = tryCatch({
        m <- lm(y ~ hdd + cdd)
        unname(coef(m)["hdd"])
      }, error = function(e) NA_real_),
      beta_cdd = tryCatch({
        m <- lm(y ~ hdd + cdd)
        unname(coef(m)["cdd"])
      }, error = function(e) NA_real_),
      r2_joint = tryCatch({
        m <- lm(y ~ hdd + cdd)
        summary(m)$r.squared
      }, error = function(e) NA_real_),
      .groups = "drop"
    )
  out
}

# Chunking to keep memory low.
chunks <- split(betas$user_id, ceiling(seq_along(betas$user_id) / 2000L))
joint_list <- lapply(seq_along(chunks), function(i) {
  if (i %% 5 == 0) message(sprintf("    chunk %d / %d", i, length(chunks)))
  joint_fit(chunks[[i]])
})
joint_df <- bind_rows(joint_list)

betas_full <- betas |> left_join(joint_df, by = "user_id")

# 3. Merge into user_features and write final table.
message("[3/3] Uniendo con user_features_base.parquet y guardando user_features.parquet...")

# Normalise per-kWh elasticities (relative to mean consumption).
betas_full <- betas_full |>
  mutate(
    beta_hdd_norm = ifelse(is.finite(beta_hdd) & y_mean > 0,
                           beta_hdd / y_mean, NA_real_),
    beta_cdd_norm = ifelse(is.finite(beta_cdd) & y_mean > 0,
                           beta_cdd / y_mean, NA_real_)
  ) |>
  select(user_id, n_obs_climate, beta_hdd, beta_cdd, r2_joint,
         beta_hdd_norm, beta_cdd_norm)

features_final <- features_legacy |>
  left_join(betas_full, by = "user_id")

arrow::write_parquet(features_final, USER_FEATURES_PARQUET)
message(sprintf("  Escrito: %s (%s usuarios, %d columnas)",
                USER_FEATURES_PARQUET,
                fmt_int(nrow(features_final)), ncol(features_final)))

# Summary table for the qmd.
summary_df <- data.frame(
  variable = c("usuarios_con_clima", "beta_hdd_mediana",
               "beta_cdd_mediana", "r2_joint_mediana",
               "beta_hdd_p90", "beta_cdd_p90"),
  value = c(
    sum(!is.na(features_final$beta_hdd)),
    median(features_final$beta_hdd, na.rm = TRUE),
    median(features_final$beta_cdd, na.rm = TRUE),
    median(features_final$r2_joint, na.rm = TRUE),
    quantile(features_final$beta_hdd, 0.90, na.rm = TRUE),
    quantile(features_final$beta_cdd, 0.90, na.rm = TRUE)
  )
)
write_csv_audit(summary_df, "feature_climate_sensitivity_summary.csv")

elapsed <- (proc.time() - t0)[["elapsed"]]
message(sprintf("\nPaso 04b completado en %.1f s.", elapsed))
