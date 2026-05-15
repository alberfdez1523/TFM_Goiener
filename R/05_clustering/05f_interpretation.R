#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05f_interpretation.R
#
# Perfiles agregados (horario, escalar) + separadores naturales por cluster.
# Outputs:
#   outputs/tables/cluster_profiles.csv
#   outputs/tables/cluster_top_separators.csv
#   outputs/figures/06_profiles_hourly.png
#   outputs/figures/06_pca_scatter.png
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))
source(here::here("R", "_lib", "plots.R"))

log_section("PASO 06f: Interpretacion")
t0 <- proc.time(); set.seed(SEED)

clusters <- read_parquet_safe(USER_CLUSTERS_PARQUET, "clusters")
pool <- read_parquet_safe(path(FEATURES_DIR, "cluster_pool.parquet"), "pool")
nh_path <- path(FEATURES_DIR, "cluster_no_habitual.parquet")
if (file_exists(nh_path)) {
  pool <- dplyr::bind_rows(pool, read_parquet_safe(nh_path, "no_habitual"))
}
art  <- readRDS(path(MODEL_DIR, "cluster_matrix.rds"))

df <- pool |> inner_join(clusters |> select(user_id, cluster), by = "user_id")

# Treat HDBSCAN noise (0) and rule-based no_habitual (-1) as distinct groups.
df <- df |> mutate(cluster_label = dplyr::case_when(
  cluster == 0L  ~ "noise",
  cluster == -1L ~ "no_habitual",
  TRUE ~ sprintf("C%d", cluster)
))

# Profiles: scalar means per cluster.
scalar_cols <- intersect(c(
  "mean_daily_kWh", "median_daily_kWh", "cv_daily",
  "ratio_night_day", "ratio_weekend_weekday", "peak_share", "valley_share",
  "flat_share", "peak_to_valley_ratio", "seasonal_amplitude_norm",
  "max_month_share", "monthly_entropy", "summer_winter_ratio",
  "low_day_rate", "zero_day_rate", "holiday_ratio", "bridge_ratio",
  "morning_kWh_share", "afternoon_kWh_share", "evening_kWh_share",
  "night_kWh_share", "peak_hour",
  "beta_hdd", "beta_cdd", "beta_hdd_norm", "beta_cdd_norm", "r2_joint",
  "corr_hdd", "corr_cdd", "p1_kw"
), names(df))

profiles <- df |>
  group_by(cluster_label) |>
  summarise(n = n(),
            across(all_of(scalar_cols), \(x) round(mean(x, na.rm = TRUE), 3)),
            .groups = "drop") |>
  mutate(pct = round(100 * n / sum(n), 2), .after = n)

write_csv_audit(profiles, "cluster_profiles.csv")
print(head(profiles))

# Top separators: standardized difference between cluster mean and global mean.
global_mean <- colMeans(df[, scalar_cols, drop = FALSE], na.rm = TRUE)
global_sd   <- apply(df[, scalar_cols, drop = FALSE], 2, sd, na.rm = TRUE)
global_sd[global_sd == 0 | is.na(global_sd)] <- 1

sep_list <- df |>
  group_by(cluster_label) |>
  group_modify(\(.x, .y) {
    m <- colMeans(.x[, scalar_cols, drop = FALSE], na.rm = TRUE)
    z <- (m - global_mean) / global_sd
    tibble::tibble(feature = scalar_cols, cluster_mean = m,
                   global_mean = global_mean, std_diff = z)
  }) |>
  arrange(cluster_label, desc(abs(std_diff))) |>
  group_by(cluster_label) |>
  slice_head(n = 8) |>
  ungroup() |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

write_csv_audit(sep_list, "cluster_top_separators.csv")

# Figure: hourly profiles per cluster.
hour_cols <- grep("^norm_h\\d{2}$", names(df), value = TRUE)
if (length(hour_cols) == 24L) {
  prof_h <- df |>
    select(cluster_label, all_of(hour_cols)) |>
    group_by(cluster_label) |>
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") |>
    pivot_longer(-cluster_label, names_to = "hour", values_to = "value") |>
    mutate(h = as.integer(sub("norm_h", "", hour)))

  p <- ggplot(prof_h, aes(h, value, colour = cluster_label)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(title = "Perfil horario normalizado por cluster",
         x = "hora del dia", y = "kWh normalizado (media usuario = 1)",
         colour = NULL) +
    theme_goiener()
  save_fig(p, "06_profiles_hourly.png", width = 9, height = 5)
}

# Figure: PCA scatter.
if (!is.null(art$X_pca) && ncol(art$X_pca) >= 2) {
  pca_df <- data.frame(PC1 = art$X_pca[, 1], PC2 = art$X_pca[, 2],
                       user_id = art$user_id) |>
    left_join(clusters |> select(user_id, cluster), by = "user_id") |>
    mutate(cluster_label = dplyr::case_when(
      cluster == 0L  ~ "noise",
      cluster == -1L ~ "no_habitual",
      TRUE ~ sprintf("C%d", cluster)
    ))
  # subsample for plotting
  set.seed(SEED)
  pca_df <- pca_df[sample.int(nrow(pca_df), min(8000, nrow(pca_df))), ]
  p <- ggplot(pca_df, aes(PC1, PC2, colour = cluster_label)) +
    geom_point(alpha = 0.5, size = 0.7) +
    labs(title = "Proyeccion PCA de los clusters",
         x = "PC1", y = "PC2", colour = NULL) +
    theme_goiener()
  save_fig(p, "06_pca_scatter.png", width = 8, height = 6)
}

message(sprintf("06f en %.1f s", (proc.time() - t0)[["elapsed"]]))
