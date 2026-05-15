#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05g_socioeconomic.R
#
# Lectura post-hoc socioeconomica de los clusters. CNAE descriptiva, p1_kw,
# contexto provincial e indicador compuesto de pobreza energetica.
# Outputs:
#   outputs/tables/cluster_socioeconomic_v2.csv
#   outputs/tables/cluster_poverty_proxy_v2.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 06g: Lectura socioeconomica post-hoc")
t0 <- proc.time(); set.seed(SEED)

clusters <- read_parquet_safe(USER_CLUSTERS_V2_PARQUET, "clusters_v2")
pool <- read_parquet_safe(path(FEATURES_DIR, "cluster_pool.parquet"), "pool")
nh_path <- path(FEATURES_DIR, "cluster_no_habitual.parquet")
if (file_exists(nh_path)) {
  pool <- dplyr::bind_rows(pool, read_parquet_safe(nh_path, "no_habitual"))
}
df <- pool |> inner_join(clusters |> select(user_id, cluster), by = "user_id") |>
  mutate(cluster_label = dplyr::case_when(
    cluster == 0L  ~ "noise",
    cluster == -1L ~ "no_habitual",
    TRUE ~ sprintf("C%d", cluster)
  ))

# 1. Provincia / contexto territorial.
ctx_cols <- intersect(c("cod_provincia", "ccaa", "coastal_flag",
                        "density_bucket", "climate_zone",
                        "goiener_core_region"), names(df))
contextual <- df |>
  group_by(cluster_label) |>
  summarise(
    n = n(),
    pct_goiener_core = round(100 * mean(goiener_core_region, na.rm = TRUE), 1),
    pct_coastal = round(100 * mean(coastal_flag, na.rm = TRUE), 1),
    top_provincia = names(sort(table(cod_provincia), decreasing = TRUE))[1],
    median_p1_kw = round(median(p1_kw, na.rm = TRUE), 2),
    p10_p1_kw    = round(quantile(p1_kw, 0.10, na.rm = TRUE), 2),
    p90_p1_kw    = round(quantile(p1_kw, 0.90, na.rm = TRUE), 2),
    .groups = "drop"
  )

# 2. Indicador compuesto de pobreza energetica (proxy, no diagnostico).
#    Componentes (todos en [0,1] tras ranking, mayor = mas riesgo):
#      a) bajo consumo medio (rank inverso)
#      b) baja amplitud estacional (rank inverso)
#      c) baja sensibilidad al frio (beta_hdd_norm bajo) -> rank inverso
#      d) alto low_day_rate -> rank directo
rk_inv <- function(x) {
  r <- rank(x, ties.method = "average", na.last = "keep")
  out <- (max(r, na.rm = TRUE) - r + 1) / max(r, na.rm = TRUE)
  out[is.na(out)] <- 0.5
  out
}
rk_dir <- function(x) {
  r <- rank(x, ties.method = "average", na.last = "keep")
  out <- r / max(r, na.rm = TRUE)
  out[is.na(out)] <- 0.5
  out
}

df_pe <- df |>
  mutate(
    pe_kwh    = rk_inv(mean_daily_kWh),
    pe_amp    = rk_inv(seasonal_amplitude_norm),
    pe_hdd    = rk_inv(beta_hdd_norm),
    pe_lowday = rk_dir(low_day_rate),
    pe_proxy_score = round((pe_kwh + pe_amp + pe_hdd + pe_lowday) / 4, 3),
    pe_high_risk = pe_proxy_score >= 0.70
  )

poverty_summary <- df_pe |>
  group_by(cluster_label) |>
  summarise(
    n = n(),
    pe_proxy_mean = round(mean(pe_proxy_score, na.rm = TRUE), 3),
    pe_proxy_p75  = round(quantile(pe_proxy_score, 0.75, na.rm = TRUE), 3),
    pct_high_risk = round(100 * mean(pe_high_risk, na.rm = TRUE), 1),
    median_kwh_d = round(median(mean_daily_kWh, na.rm = TRUE), 2),
    median_beta_hdd_norm = round(median(beta_hdd_norm, na.rm = TRUE), 4),
    median_seasonal_amp = round(median(seasonal_amplitude_norm, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  arrange(desc(pct_high_risk))

write_csv_audit(contextual, "cluster_socioeconomic_v2.csv")
write_csv_audit(poverty_summary, "cluster_poverty_proxy_v2.csv")
# Persist user-level proxy for forecasting/business use.
arrow::write_parquet(
  df_pe |> select(user_id, cluster, cluster_label, pe_proxy_score, pe_high_risk),
  path(FEATURES_DIR, "user_poverty_proxy.parquet")
)

message(sprintf("06g en %.1f s", (proc.time() - t0)[["elapsed"]]))
