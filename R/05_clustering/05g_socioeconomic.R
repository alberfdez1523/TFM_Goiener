#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05g_socioeconomic.R
#
# Lectura post-hoc socioeconomica de los clusters. CNAE descriptiva, p1_kw,
# contexto provincial e indicador compuesto de pobreza energetica.
# Outputs:
#   outputs/tables/cluster_socioeconomic.csv
#   outputs/tables/cluster_poverty_proxy.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 06g: Lectura socioeconomica post-hoc")
t0 <- proc.time(); set.seed(SEED)

clusters <- read_parquet_safe(USER_CLUSTERS_PARQUET, "clusters")
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
  ),
  cluster_report = ifelse(cluster == -1L, 0L, cluster))

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

write_csv_audit(contextual, "cluster_socioeconomic.csv")
write_csv_audit(poverty_summary, "cluster_poverty_proxy.csv")

cnae_section <- function(x) {
  code <- suppressWarnings(as.integer(substr(gsub("\\D", "", as.character(x)), 1, 2)))
  dplyr::case_when(
    is.na(code) ~ "Desconocido",
    code <= 3 ~ "A Agricultura",
    code <= 9 ~ "B Extractivas",
    code <= 33 ~ "C Industria",
    code == 35 ~ "D Energia",
    code <= 39 ~ "E Agua y residuos",
    code <= 43 ~ "F Construccion",
    code <= 47 ~ "G Comercio",
    code <= 53 ~ "H Transporte",
    code <= 56 ~ "I Hosteleria",
    code <= 63 ~ "J Informacion",
    code <= 66 ~ "K Finanzas",
    code == 68 ~ "L Inmobiliarias",
    code <= 75 ~ "M Profesionales",
    code <= 82 ~ "N Administrativas",
    code == 84 ~ "O Administracion",
    code == 85 ~ "P Educacion",
    code <= 88 ~ "Q Sanidad",
    code <= 93 ~ "R Arte y ocio",
    code <= 96 ~ "S Otros servicios",
    code <= 98 ~ "T Hogares",
    TRUE ~ "U Organismos"
  )
}

cnae_base <- df |>
  mutate(
    cluster = cluster_report,
    cnae_clean = trimws(as.character(cnae)),
    cnae_known = !is.na(cnae_clean) & cnae_clean != "" & cnae_clean != "NA",
    cnae_section_label = ifelse(cnae_known, cnae_section(cnae_clean), "Desconocido"),
    cnae_section = substr(cnae_section_label, 1, 1),
    cnae_division = suppressWarnings(as.integer(substr(gsub("\\D", "", cnae_clean), 1, 2)))
  )

cluster_totals <- cnae_base |>
  count(cluster, cluster_label, name = "n_users")

cnae_coverage <- cnae_base |>
  group_by(cluster, cluster_label) |>
  summarise(
    n_users = n(),
    n_cnae_known = sum(cnae_known, na.rm = TRUE),
    n_cnae_unknown = n_users - n_cnae_known,
    coverage_pct = round(100 * n_cnae_known / n_users, 2),
    top_cnae_section_label = names(sort(table(cnae_section_label), decreasing = TRUE))[1],
    .groups = "drop"
  )

cnae_distribution <- cnae_base |>
  count(cluster, cluster_label, cnae_section, cnae_section_label, name = "n_users") |>
  left_join(cluster_totals, by = c("cluster", "cluster_label")) |>
  mutate(
    pct_cluster = round(100 * n_users.x / n_users.y, 3),
    pct_global = round(100 * n_users.x / sum(n_users.x), 3),
    support_ok = n_users.x >= CLUSTER_CNAE_MIN_N
  ) |>
  select(cluster, cluster_label, cnae_section, cnae_section_label,
         n_users = n_users.x, pct_cluster, pct_global, support_ok)

cnae_enrichment <- cnae_distribution |>
  group_by(cnae_section_label) |>
  mutate(global_share = sum(n_users) / sum(cnae_distribution$n_users)) |>
  ungroup() |>
  mutate(
    enrichment_ratio = round((pct_cluster / 100) / pmax(global_share, 1e-6), 3),
    is_interpretable = support_ok & cnae_section_label != "Desconocido"
  ) |>
  select(cluster, cluster_label, cnae_section_label, n_users,
         enrichment_ratio, is_interpretable)

cnae_division_distribution <- cnae_base |>
  mutate(cnae_division = ifelse(is.na(cnae_division), -1L, cnae_division)) |>
  count(cluster, cluster_label, cnae_division, name = "n_users") |>
  left_join(cluster_totals, by = c("cluster", "cluster_label")) |>
  mutate(pct_cluster = round(100 * n_users.x / n_users.y, 3)) |>
  select(cluster, cluster_label, cnae_division, n_users = n_users.x, pct_cluster)

business_interpretation <- cnae_coverage |>
  transmute(
    cluster,
    cluster_label,
    business_question = "Que lectura operativa aporta el segmento?",
    behavioral_signal = paste("Perfil", cluster_label, "con", n_users, "usuarios"),
    cnae_signal = paste("CNAE dominante:", top_cnae_section_label),
    goiener_action = "Usar como capa agregada de interpretacion, nunca como diagnostico individual.",
    caveat = "La lectura CNAE es descriptiva y depende de la calidad de metadatos."
  )

write_csv_audit(cnae_coverage, "cluster_cnae_coverage.csv")
write_csv_audit(cnae_distribution, "cluster_cnae_section_distribution.csv")
write_csv_audit(cnae_enrichment, "cluster_cnae_enrichment.csv")
write_csv_audit(cnae_division_distribution, "cluster_cnae_division_distribution.csv")
write_csv_audit(business_interpretation, "cluster_business_interpretation.csv")

# Persist user-level proxy for forecasting/business use.
arrow::write_parquet(
  df_pe |> select(user_id, cluster, cluster_label, pe_proxy_score, pe_high_risk),
  path(FEATURES_DIR, "user_poverty_proxy.parquet")
)

message(sprintf("06g en %.1f s", (proc.time() - t0)[["elapsed"]]))
