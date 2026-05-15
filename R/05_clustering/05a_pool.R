#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05a_pool.R
#
# Embudo residencial estricto + matriz de features para clustering V2.
# Escribe data/parquet/features/cluster_pool.parquet y
# outputs/tables/cluster_pool_audit.csv.
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 05a: Pool residencial y matriz de features")
t0 <- proc.time(); set.seed(SEED)

features <- read_parquet_safe(USER_FEATURES_V2_PARQUET,
                              "user_features_v2.parquet")

is_true <- function(x) { y <- as.logical(x); y[is.na(y)] <- FALSE; y }

mask_residential <- is_true(features$is_residential_strict)
mask_clean       <- !is_true(features$has_sustained_extreme)
mask_coverage    <- !is.na(features$active_days) &
                    features$active_days >= MIN_ACTIVE_DAYS &
                    !is.na(features$mean_daily_kWh) &
                    features$mean_daily_kWh > 0
mask_focus       <- features$cod_provincia %in% FOCUS_PROVINCES
mask_occupied    <- !is.na(features$mean_daily_kWh) &
                    features$mean_daily_kWh >= MIN_DAILY_KWH_CLUSTER

eligible <- features[mask_residential & mask_clean & mask_coverage & mask_focus, ]
pool <- eligible[eligible$mean_daily_kWh >= MIN_DAILY_KWH_CLUSTER, ]
no_habitual <- eligible[eligible$mean_daily_kWh < MIN_DAILY_KWH_CLUSTER, ]

audit <- tibble::tibble(
  paso = c("Total features", "Provincias top-5",
           "Residencial 2.0TD", "Sin outlier sostenido",
           sprintf(">= %d dias activos", MIN_ACTIVE_DAYS),
           sprintf("Vivienda habitual (>= %.1f kWh/dia)", MIN_DAILY_KWH_CLUSTER),
           "Pool clustering V2",
           "Segmento no_habitual (separado)"),
  n_usuarios = c(
    nrow(features),
    sum(mask_focus),
    sum(mask_focus & mask_residential),
    sum(mask_focus & mask_residential & mask_clean),
    sum(mask_focus & mask_residential & mask_clean & mask_coverage),
    sum(mask_focus & mask_residential & mask_clean & mask_coverage & mask_occupied),
    nrow(pool),
    nrow(no_habitual)
  )
) |> mutate(pct_total = round(100 * n_usuarios / nrow(features), 2))

write_csv_audit(audit, "cluster_pool_audit.csv")
print(audit)

arrow::write_parquet(pool, path(FEATURES_DIR, "cluster_pool.parquet"))
arrow::write_parquet(no_habitual,
                     path(FEATURES_DIR, "cluster_no_habitual.parquet"))
message(sprintf("Pool guardado: %s usuarios (vivienda habitual)",
                fmt_int(nrow(pool))))
message(sprintf("Segmento no_habitual: %s usuarios", fmt_int(nrow(no_habitual))))
message(sprintf("05a en %.1f s", (proc.time() - t0)[["elapsed"]]))
