#!/usr/bin/env Rscript
# ==============================================================================
# R/06_forecasting/06a_targets.R
#
# Construye 3 series objetivo agregadas sobre el pool clusterizado:
#   - portfolio_daily.parquet    (date, kWh_total, n_users)
#   - portfolio_hourly.parquet   (datetime, kWh_total, n_users)
#   - cluster_daily.parquet      (date, cluster, kWh_total)
# Une calendario y clima provincial agregado.
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(duckdb); library(DBI)
  library(glue); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 06a: Construyendo series objetivo")
t0 <- proc.time(); set.seed(SEED)

stopifnot(
  "Falta cluster_pool.parquet" = file_exists(path(FEATURES_DIR, "cluster_pool.parquet")),
  "Falta user_clusters.parquet" = file_exists(USER_CLUSTERS_PARQUET),
  "Falta daily_with_climate.parquet" = file_exists(DAILY_WITH_CLIMATE)
)

clusters <- read_parquet_safe(USER_CLUSTERS_PARQUET, "clusters")
pool_ids <- clusters$user_id

con <- connect_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

daily_abs <- path_abs(DAILY_WITH_CLIMATE) |> path_norm()
hourly_glob <- HOURLY_GLOB

# Register cluster mapping as a DuckDB view via Arrow.
clust_df <- clusters |> select(user_id, cluster) |>
  mutate(cluster = as.integer(cluster)) |>
  filter(cluster >= 0L)  # excluye no_habitual (cluster = -1)
duckdb::duckdb_register(con, "clust", clust_df)

# 1. Daily portfolio
message("[1/3] Construyendo portfolio_daily...")
portfolio_daily <- dbGetQuery(con, glue("
  SELECT
    CAST(d.date AS DATE) AS date,
    SUM(d.daily_kWh) AS kWh_total,
    COUNT(DISTINCT d.user_id) AS n_users,
    AVG(d.hdd) AS hdd_mean,
    AVG(d.cdd) AS cdd_mean,
    AVG(d.tmed) AS tmed_mean,
    MAX(d.is_holiday::INT) AS any_holiday
  FROM read_parquet('{daily_abs}') d
  INNER JOIN clust c ON c.user_id = d.user_id
  GROUP BY 1
  ORDER BY 1
"))
arrow::write_parquet(portfolio_daily, PORTFOLIO_DAILY_PARQUET)
message(sprintf("  portfolio_daily: %s filas (%s -> %s)",
                fmt_int(nrow(portfolio_daily)),
                min(portfolio_daily$date), max(portfolio_daily$date)))

# 2. Cluster-level daily
message("[2/3] Construyendo cluster_daily...")
cluster_daily <- dbGetQuery(con, glue("
  SELECT
    CAST(d.date AS DATE) AS date,
    c.cluster,
    SUM(d.daily_kWh) AS kWh_total,
    COUNT(DISTINCT d.user_id) AS n_users,
    AVG(d.hdd) AS hdd_mean,
    AVG(d.cdd) AS cdd_mean
  FROM read_parquet('{daily_abs}') d
  INNER JOIN clust c ON c.user_id = d.user_id
  GROUP BY 1, 2
  ORDER BY 1, 2
"))
arrow::write_parquet(cluster_daily, CLUSTER_DAILY_PARQUET)
message(sprintf("  cluster_daily: %s filas, %d clusters",
                fmt_int(nrow(cluster_daily)),
                length(unique(cluster_daily$cluster))))

# 3. Hourly portfolio (heavy; uses hourly parquet glob)
message("[3/3] Construyendo portfolio_hourly...")
portfolio_hourly <- dbGetQuery(con, glue("
  SELECT
    CAST(strftime(h.timestamp, '%Y-%m-%d %H:00:00') AS TIMESTAMP) AS datetime,
    SUM(h.kWh) AS kWh_total,
    COUNT(DISTINCT h.user_id) AS n_users
  FROM read_parquet('{hourly_glob}') h
  INNER JOIN clust c ON c.user_id = h.user_id
  WHERE h.kWh IS NOT NULL AND h.kWh >= 0
  GROUP BY 1
  ORDER BY 1
"))
arrow::write_parquet(portfolio_hourly, PORTFOLIO_HOURLY_PARQUET)
message(sprintf("  portfolio_hourly: %s filas (%s -> %s)",
                fmt_int(nrow(portfolio_hourly)),
                min(portfolio_hourly$datetime),
                max(portfolio_hourly$datetime)))

message(sprintf("06a en %.1f s", (proc.time() - t0)[["elapsed"]]))
