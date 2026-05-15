#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 02: Validacion y calidad del dato
# ==============================================================================
#
# Analiza la calidad de los datos ya convertidos a Parquet:
#   - Cobertura temporal por usuario (primer/ultimo registro, duracion)
#   - Gaps temporales (huecos > 24h)
#   - Lecturas sospechosas (zeros exactos, picos, negativos)
#   - Dias incompletos (< 24 horas)
#   - Resumen exportable para la memoria del TFM
#
# Inputs:
#   data/parquet/year=*/*.parquet    (dataset horario particionado)
#   data/parquet/daily_consumption.parquet
#   data/parquet/metadata.parquet
#
# Outputs:
#   outputs/tables/data_quality_summary.csv
#   outputs/tables/user_coverage.csv
#   outputs/tables/gap_analysis.csv
#   outputs/tables/schema_checks.csv
#   outputs/tables/data_quality_affected_users.csv
#   outputs/tables/pipeline_manifest.csv
#   outputs/tables/data_quality_timings.csv
#
# Uso:
#   Rscript R/02_data_quality.R
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(arrow)
  library(glue)
  library(fs)
})

source(here::here("_config.R"))

message("="
 |> strrep(60))
message("PASO 02: Validacion y calidad del dato")
message("=" |> strrep(60))

# --- Verificaciones previas ---
stopifnot(
  "No existe el parquet diario. Ejecuta primero 01_csv_to_parquet.R" =
    file_exists(DAILY_PARQUET),
  "No existe metadata.parquet. Ejecuta primero 01_csv_to_parquet.R" =
    file_exists(METADATA_PARQUET)
)

t0_total <- proc.time()
con <- connect_duckdb()

hourly_glob_abs <- HOURLY_GLOB
daily_abs <- path_abs(DAILY_PARQUET) |> path_norm()
metadata_abs <- path_abs(METADATA_PARQUET) |> path_norm()

status_from_bad <- function(n_bad, severity = "fail") {
  if (is.na(n_bad) || n_bad > 0) severity else "pass"
}

schema_for <- function(path_sql, hive = FALSE) {
  dbGetQuery(con, glue("
    DESCRIBE SELECT *
    FROM read_parquet('{path_sql}', hive_partitioning = {tolower(as.character(hive))})
    LIMIT 0
  ")) |>
    transmute(
      column_name = column_name,
      observed_type = column_type
    )
}

check_required_schema <- function(table_name, schema_df, required_cols) {
  tibble::tibble(
    check_id = paste0(table_name, "_schema_", required_cols),
    table_name = table_name,
    column_name = required_cols,
    check_type = "required_column",
    expected = "present",
    observed = ifelse(required_cols %in% schema_df$column_name, "present", "missing"),
    status = ifelse(required_cols %in% schema_df$column_name, "pass", "fail")
  )
}

check_range_row <- function(check_id, table_name, column_name, expected,
                            observed, n_bad, severity = "fail") {
  tibble::tibble(
    check_id = check_id,
    table_name = table_name,
    column_name = column_name,
    check_type = "range_or_contract",
    expected = expected,
    observed = as.character(observed),
    status = status_from_bad(n_bad, severity)
  )
}

# ==============================================================================
# 1. Resumen global del dataset
# ==============================================================================
message("\n[1/8] Resumen global del dataset...")
tic("1. Resumen global del dataset")

global_summary <- dbGetQuery(con, glue("
  SELECT
    COUNT(*)                     AS total_registros,
    COUNT(DISTINCT user_id)      AS total_usuarios,
    MIN(timestamp)               AS fecha_inicio,
    MAX(timestamp)               AS fecha_fin,
    ROUND(SUM(kWh), 0)          AS total_kWh,
    ROUND(AVG(kWh), 4)          AS media_kWh_horario,
    SUM(CASE WHEN kWh IS NULL THEN 1 ELSE 0 END) AS n_nulls,
    SUM(CASE WHEN kWh < 0 THEN 1 ELSE 0 END)     AS n_negativos,
    SUM(CASE WHEN kWh = 0 THEN 1 ELSE 0 END)     AS n_zeros
  FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
"))

message(sprintf("  Registros: %s | Usuarios: %s",
                fmt_int(global_summary$total_registros),
                fmt_int(global_summary$total_usuarios)))
message(sprintf("  Nulls: %s | Negativos: %s | Zeros: %s",
                fmt_int(global_summary$n_nulls),
                fmt_int(global_summary$n_negativos),
                fmt_int(global_summary$n_zeros)))
toc("1. Resumen global del dataset")

# ==============================================================================
# 2. Cobertura temporal por usuario
# ==============================================================================
message("\n[2/8] Cobertura temporal por usuario...")
tic("2. Cobertura temporal por usuario")

user_coverage <- dbGetQuery(con, glue("
  SELECT
    user_id,
    MIN(timestamp)                                 AS first_ts,
    MAX(timestamp)                                 AS last_ts,
    DATEDIFF('day', MIN(timestamp), MAX(timestamp)) AS span_days,
    COUNT(*)                                        AS total_records,
    COUNT(DISTINCT DATE(timestamp))                 AS distinct_days,
    SUM(CASE WHEN kWh IS NULL THEN 1 ELSE 0 END)  AS n_nulls,
    SUM(CASE WHEN kWh = 0 THEN 1 ELSE 0 END)      AS n_zeros,
    SUM(CASE WHEN kWh < 0 THEN 1 ELSE 0 END)      AS n_negativos
  FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
  WHERE user_id IS NOT NULL AND user_id <> ''
  GROUP BY user_id
"))

# Calcular metricas derivadas
user_coverage <- user_coverage |>
  mutate(
    span_days_inclusive = span_days + 1L,
    coverage_pct = ifelse(span_days_inclusive > 0,
                          round(100 * distinct_days / span_days_inclusive, 1),
                          100),
    zero_pct = round(100 * n_zeros / total_records, 2),
    null_pct = round(100 * n_nulls / total_records, 2)
  )

# Resumen de cobertura
coverage_summary <- user_coverage |>
  summarise(
    n_usuarios = n(),
    mediana_span_days = median(span_days_inclusive),
    media_span_days = round(mean(span_days_inclusive), 0),
    mediana_coverage_pct = median(coverage_pct),
    mediana_distinct_days = median(distinct_days),
    users_lt_30_days = sum(span_days_inclusive < 30),
    users_lt_365_days = sum(span_days_inclusive < 365),
    users_gt_5_years = sum(span_days_inclusive > 1825)
  )

message(sprintf("  Mediana de cobertura: %s dias (%s%%)",
                fmt_int(coverage_summary$mediana_span_days),
                fmt_num(coverage_summary$mediana_coverage_pct)))
message(sprintf("  Usuarios < 30 dias: %s | < 1 anio: %s | > 5 anios: %s",
                fmt_int(coverage_summary$users_lt_30_days),
                fmt_int(coverage_summary$users_lt_365_days),
                fmt_int(coverage_summary$users_gt_5_years)))

# Exportar
write.csv(user_coverage, path(TABLE_DIR, "user_coverage.csv"), row.names = FALSE)
toc("2. Cobertura temporal por usuario")

# ==============================================================================
# 3. Analisis de gaps temporales
# ==============================================================================
message("\n[3/8] Analisis de gaps temporales...")
tic("3. Analisis de gaps temporales")

# El criterio del TFM define gap como un hueco > 24h entre dias observados.
# Se calcula directamente sobre la tabla diaria pre-agregada para evitar el
# coste del window horario sobre todo el dataset.
gap_analysis <- dbGetQuery(con, glue("
  WITH ordered AS (
    SELECT
      user_id,
      date,
      LAG(date) OVER (PARTITION BY user_id ORDER BY date) AS prev_date
    FROM read_parquet('{daily_abs}')
    WHERE user_id IS NOT NULL AND user_id <> ''
  ),
  gaps AS (
    SELECT
      user_id,
      prev_date AS gap_start,
      date AS gap_end,
      DATEDIFF('day', prev_date, date) - 1 AS gap_days
    FROM ordered
    WHERE prev_date IS NOT NULL
      AND DATEDIFF('day', prev_date, date) > 1
  )
  SELECT
    user_id,
    gap_start,
    gap_end,
    gap_days
  FROM gaps
  ORDER BY gap_days DESC
"))

# Resumen global de gaps
if (nrow(gap_analysis) == 0) {
  gap_global <- tibble::tibble(
    users_with_gaps = 0L,
    total_gaps = 0L,
    mediana_gaps_per_user = 0,
    max_gap_days_global = 0L,
    median_max_gap_days = 0
  )
} else {
  gaps_by_user <- gap_analysis |>
    group_by(user_id) |>
    summarise(
      n_gaps = n(),
      max_gap_days = max(gap_days),
      .groups = "drop"
    )

  gap_global <- gaps_by_user |>
    summarise(
      users_with_gaps = n(),
      total_gaps = sum(n_gaps),
      mediana_gaps_per_user = median(n_gaps),
      max_gap_days_global = max(max_gap_days),
      median_max_gap_days = median(max_gap_days)
    )
}

message(sprintf("  Usuarios con gaps: %s de %s (%s%%)",
                fmt_int(gap_global$users_with_gaps),
                fmt_int(nrow(user_coverage)),
                fmt_num(100 * gap_global$users_with_gaps / nrow(user_coverage))))
message(sprintf("  Total gaps: %s | Max gap: %s dias",
                fmt_int(gap_global$total_gaps),
                fmt_int(gap_global$max_gap_days_global)))

write.csv(gap_analysis, path(TABLE_DIR, "gap_analysis.csv"), row.names = FALSE)
toc("3. Analisis de gaps temporales")

# ==============================================================================
# 4. Dias incompletos
# ==============================================================================
message("\n[4/8] Analisis de dias incompletos...")
tic("4. Analisis de dias incompletos")

incomplete_days <- dbGetQuery(con, glue("
  SELECT
    hours_recorded,
    COUNT(*) AS n_user_days
  FROM read_parquet('{daily_abs}')
  WHERE user_id IS NOT NULL AND user_id <> ''
  GROUP BY hours_recorded
  ORDER BY hours_recorded
"))

total_user_days <- sum(incomplete_days$n_user_days)
complete_days <- incomplete_days$n_user_days[incomplete_days$hours_recorded == 24]
if (length(complete_days) == 0) complete_days <- 0

message(sprintf("  Total dias-usuario: %s", fmt_int(total_user_days)))
message(sprintf("  Dias completos (24h): %s (%s%%)",
                fmt_int(complete_days),
                fmt_num(100 * complete_days / total_user_days)))
message(sprintf("  Dias incompletos: %s (%s%%)",
                fmt_int(total_user_days - complete_days),
                fmt_num(100 * (total_user_days - complete_days) / total_user_days)))
toc("4. Analisis de dias incompletos")

# ==============================================================================
# 5. Lecturas sospechosas (picos extremos)
# ==============================================================================
message("\n[5/8] Deteccion de lecturas sospechosas...")
tic("5. Deteccion de lecturas sospechosas")

spike_analysis <- dbGetQuery(con, glue("
  WITH stats AS (
    SELECT
      PERCENTILE_CONT(0.999) WITHIN GROUP (ORDER BY kWh) AS p999,
      PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY kWh) AS p99,
      PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY kWh) AS p95
    FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
    WHERE kWh IS NOT NULL AND kWh >= 0
  )
  SELECT
    s.p95, s.p99, s.p999,
    (SELECT COUNT(*) FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
     WHERE kWh > s.p999) AS n_above_p999,
    (SELECT COUNT(*) FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
     WHERE kWh > s.p99) AS n_above_p99,
    (SELECT COUNT(*) FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
     WHERE kWh > 100) AS n_above_100kWh
  FROM stats s
"))

message(sprintf("  P95: %s kWh | P99: %s kWh | P99.9: %s kWh",
                fmt_num(spike_analysis$p95),
                fmt_num(spike_analysis$p99),
                fmt_num(spike_analysis$p999)))
message(sprintf("  Lecturas > P99.9: %s | > 100 kWh: %s",
                fmt_int(spike_analysis$n_above_p999),
                fmt_int(spike_analysis$n_above_100kWh)))
toc("5. Deteccion de lecturas sospechosas")

# ==============================================================================
# 6. Usuarios afectados por negativos/extremos
# ==============================================================================
message("\n[6/8] Usuarios afectados por negativos/extremos...")
tic("6. Usuarios afectados por negativos/extremos")

affected_users <- dbGetQuery(con, glue(" 
  WITH hourly_user AS (
    SELECT
      user_id,
      COUNT(*) AS total_records,
      SUM(CASE WHEN kWh < 0 THEN 1 ELSE 0 END) AS n_negativos,
      ABS(SUM(CASE WHEN kWh < 0 THEN kWh ELSE 0 END)) AS kWh_negativos_abs,
      SUM(CASE WHEN kWh > {OUTLIER_KWH_HOUR} THEN 1 ELSE 0 END) AS n_extremos_horarios,
      SUM(CASE WHEN kWh > {OUTLIER_KWH_HOUR} THEN kWh ELSE 0 END) AS kWh_extremos_horarios,
      SUM(CASE WHEN kWh IS NOT NULL AND kWh >= 0 THEN kWh ELSE 0 END) AS kWh_total_horario_no_negativo
    FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
    WHERE user_id IS NOT NULL AND user_id <> ''
    GROUP BY user_id
  ),
  daily_user AS (
    SELECT
      user_id,
      SUM(CASE WHEN daily_kWh < 0 THEN 1 ELSE 0 END) AS n_dias_negativos,
      ABS(SUM(CASE WHEN daily_kWh < 0 THEN daily_kWh ELSE 0 END)) AS kWh_dias_negativos_abs,
      SUM(CASE WHEN daily_kWh > {OUTLIER_KWH_DAY} THEN 1 ELSE 0 END) AS n_dias_extremos,
      SUM(CASE WHEN daily_kWh > {OUTLIER_KWH_DAY} THEN daily_kWh ELSE 0 END) AS kWh_dias_extremos
    FROM read_parquet('{daily_abs}')
    WHERE user_id IS NOT NULL AND user_id <> ''
    GROUP BY user_id
  ),
  joined AS (
    SELECT
      h.user_id,
      h.total_records,
      h.n_negativos,
      h.kWh_negativos_abs,
      h.n_extremos_horarios,
      h.kWh_extremos_horarios,
      COALESCE(d.n_dias_negativos, 0) AS n_dias_negativos,
      COALESCE(d.kWh_dias_negativos_abs, 0) AS kWh_dias_negativos_abs,
      COALESCE(d.n_dias_extremos, 0) AS n_dias_extremos,
      COALESCE(d.kWh_dias_extremos, 0) AS kWh_dias_extremos,
      h.kWh_negativos_abs + h.kWh_extremos_horarios AS kWh_afectado_horario,
      h.kWh_total_horario_no_negativo
    FROM hourly_user h
    LEFT JOIN daily_user d ON h.user_id = d.user_id
    WHERE h.n_negativos > 0
       OR h.n_extremos_horarios > 0
       OR COALESCE(d.n_dias_negativos, 0) > 0
       OR COALESCE(d.n_dias_extremos, 0) > 0
  ),
  global_energy AS (
    SELECT SUM(kWh_total_horario_no_negativo) AS kWh_global_horario_no_negativo
    FROM hourly_user
  )
  SELECT
    j.user_id,
    CASE
      WHEN j.n_negativos > 0 AND j.n_extremos_horarios > 0 THEN 'negativos y extremos horarios'
      WHEN j.n_negativos > 0 THEN 'negativos'
      WHEN j.n_extremos_horarios > 0 THEN 'extremos horarios'
      ELSE 'extremos diarios'
    END AS tipo_anomalia,
    j.total_records,
    j.n_negativos,
    ROUND(j.kWh_negativos_abs, 4) AS kWh_negativos_abs,
    j.n_extremos_horarios,
    ROUND(j.kWh_extremos_horarios, 4) AS kWh_extremos_horarios,
    j.n_dias_negativos,
    ROUND(j.kWh_dias_negativos_abs, 4) AS kWh_dias_negativos_abs,
    j.n_dias_extremos,
    ROUND(j.kWh_dias_extremos, 4) AS kWh_dias_extremos,
    ROUND(j.kWh_afectado_horario, 4) AS kWh_afectado_horario,
    ROUND(100.0 * j.kWh_afectado_horario / NULLIF(j.kWh_total_horario_no_negativo, 0), 6) AS pct_kWh_usuario_afectado,
    ROUND(100.0 * j.kWh_afectado_horario / NULLIF(g.kWh_global_horario_no_negativo, 0), 9) AS pct_kWh_global_afectado
  FROM joined j
  CROSS JOIN global_energy g
  ORDER BY pct_kWh_usuario_afectado DESC, kWh_afectado_horario DESC
"))

write.csv(affected_users, path(TABLE_DIR, "data_quality_affected_users.csv"), row.names = FALSE)
message(sprintf("  Usuarios afectados: %s", fmt_int(nrow(affected_users))))
message(sprintf("  kWh horario afectado: %s", fmt_num(sum(affected_users$kWh_afectado_horario, na.rm = TRUE))))
toc("6. Usuarios afectados por negativos/extremos")

# ==============================================================================
# 7. Contratos de esquema y rango
# ==============================================================================
message("\n[7/8] Ejecutando contratos de esquema y rango...")
tic("7. Contratos de esquema y rango")

hourly_schema <- schema_for(hourly_glob_abs, hive = TRUE)
daily_schema <- schema_for(daily_abs)
metadata_schema <- schema_for(metadata_abs)

schema_checks <- bind_rows(
  check_required_schema(
    "hourly_parquet", hourly_schema,
    c("user_id", "timestamp", "kWh", "year")
  ),
  check_required_schema(
    "daily_consumption", daily_schema,
    c("user_id", "date", "daily_kWh", "hours_recorded")
  ),
  check_required_schema(
    "metadata", metadata_schema,
    c("cups", "codigo_postal", "tarifa_atr", "fecha_alta")
  )
)

hourly_contracts <- dbGetQuery(con, glue("
  SELECT
    COUNT(*) FILTER (WHERE timestamp IS NULL) AS n_timestamp_null,
    COUNT(*) FILTER (WHERE user_id IS NULL OR user_id = '') AS n_user_id_missing,
    COUNT(*) FILTER (WHERE kWh IS NULL) AS n_kwh_null,
    COUNT(*) FILTER (WHERE kWh < 0) AS n_kwh_negative,
    COUNT(*) FILTER (WHERE kWh > {OUTLIER_KWH_HOUR}) AS n_kwh_above_hour_threshold
  FROM read_parquet('{hourly_glob_abs}', hive_partitioning = TRUE)
"))

daily_contracts <- dbGetQuery(con, glue("
  SELECT
    COUNT(*) FILTER (WHERE date IS NULL) AS n_date_null,
    COUNT(*) FILTER (WHERE user_id IS NULL OR user_id = '') AS n_user_id_missing,
    COUNT(*) FILTER (WHERE daily_kWh IS NULL) AS n_daily_kwh_null,
    COUNT(*) FILTER (WHERE daily_kWh < 0) AS n_daily_kwh_negative,
    COUNT(*) FILTER (WHERE hours_recorded < 1 OR hours_recorded > 24) AS n_bad_hours_recorded,
    COUNT(*) FILTER (WHERE daily_kWh > {OUTLIER_KWH_DAY}) AS n_daily_kwh_above_threshold
  FROM read_parquet('{daily_abs}')
"))

metadata_contracts <- dbGetQuery(con, glue("
  SELECT
    COUNT(*) FILTER (WHERE cups IS NULL OR cups = '') AS n_cups_missing,
    COUNT(*) FILTER (
      WHERE codigo_postal IS NOT NULL
        AND LENGTH(LPAD(CAST(codigo_postal AS VARCHAR), 5, '0')) < 5
    ) AS n_codigo_postal_short,
    COUNT(*) FILTER (
      WHERE fecha_alta IS NOT NULL
        AND fecha_baja IS NOT NULL
        AND fecha_baja < fecha_alta
    ) AS n_fecha_baja_before_alta
  FROM read_parquet('{metadata_abs}')
"))

range_checks <- bind_rows(
  check_range_row(
    "hourly_timestamp_not_null", "hourly_parquet", "timestamp",
    "0 timestamp nulos", hourly_contracts$n_timestamp_null,
    hourly_contracts$n_timestamp_null
  ),
  check_range_row(
    "hourly_user_id_not_missing", "hourly_parquet", "user_id",
    "0 user_id vacios", hourly_contracts$n_user_id_missing,
    hourly_contracts$n_user_id_missing
  ),
  check_range_row(
    "hourly_kwh_not_null", "hourly_parquet", "kWh",
    "0 kWh nulos", hourly_contracts$n_kwh_null,
    hourly_contracts$n_kwh_null
  ),
  check_range_row(
    "hourly_kwh_non_negative", "hourly_parquet", "kWh",
    "0 kWh negativos", hourly_contracts$n_kwh_negative,
    hourly_contracts$n_kwh_negative
  ),
  check_range_row(
    "hourly_kwh_extreme_threshold", "hourly_parquet", "kWh",
    paste0("0 lecturas >", OUTLIER_KWH_HOUR, " kWh/h"),
    hourly_contracts$n_kwh_above_hour_threshold,
    hourly_contracts$n_kwh_above_hour_threshold,
    severity = "warn"
  ),
  check_range_row(
    "daily_date_not_null", "daily_consumption", "date",
    "0 fechas nulas", daily_contracts$n_date_null,
    daily_contracts$n_date_null
  ),
  check_range_row(
    "daily_hours_recorded_1_24", "daily_consumption", "hours_recorded",
    "hours_recorded entre 1 y 24", daily_contracts$n_bad_hours_recorded,
    daily_contracts$n_bad_hours_recorded
  ),
  check_range_row(
    "daily_kwh_non_negative", "daily_consumption", "daily_kWh",
    "0 kWh diarios negativos", daily_contracts$n_daily_kwh_negative,
    daily_contracts$n_daily_kwh_negative
  ),
  check_range_row(
    "daily_kwh_extreme_threshold", "daily_consumption", "daily_kWh",
    paste0("0 dias >", OUTLIER_KWH_DAY, " kWh/dia"),
    daily_contracts$n_daily_kwh_above_threshold,
    daily_contracts$n_daily_kwh_above_threshold,
    severity = "warn"
  ),
  check_range_row(
    "metadata_cups_not_missing", "metadata", "cups",
    "0 CUPS vacios", metadata_contracts$n_cups_missing,
    metadata_contracts$n_cups_missing
  ),
  check_range_row(
    "metadata_fecha_order", "metadata", "fecha_alta/fecha_baja",
    "0 fecha_baja anterior a fecha_alta",
    metadata_contracts$n_fecha_baja_before_alta,
    metadata_contracts$n_fecha_baja_before_alta
  )
)

schema_checks <- bind_rows(schema_checks, range_checks) |>
  mutate(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    status = if_else(status == "pass", "pass", status)
  )

write.csv(schema_checks, path(TABLE_DIR, "schema_checks.csv"), row.names = FALSE)

message(sprintf("  Checks: %s pass | %s warn | %s fail",
                fmt_int(sum(schema_checks$status == "pass")),
                fmt_int(sum(schema_checks$status == "warn")),
                fmt_int(sum(schema_checks$status == "fail"))))
toc("7. Contratos de esquema y rango")

# ==============================================================================
# 8. Exportar resumen consolidado
# ==============================================================================
message("\n[8/8] Exportando resumen de calidad...")
tic("8. Exportando resumen de calidad")

quality_summary <- tibble::tibble(
  metrica = c(
    "Total registros horarios",
    "Total usuarios",
    "Rango temporal",
    "Registros NULL",
    "Registros negativos",
    "Registros zero exacto",
    "% zeros",
    "Mediana cobertura por usuario (dias)",
    "Mediana cobertura (%)",
    "Usuarios con < 30 dias",
    "Usuarios con gaps",
    "Total gaps detectados",
    "Max gap (dias)",
    "Total dias-usuario",
    "% dias completos (24h)",
    "P95 consumo horario (kWh)",
    "P99 consumo horario (kWh)",
    "P99.9 consumo horario (kWh)",
    "Lecturas > 100 kWh/h",
    "Usuarios afectados por negativos/extremos",
    "kWh horario afectado por negativos/extremos"
  ),
  valor = c(
    fmt_int(global_summary$total_registros),
    fmt_int(global_summary$total_usuarios),
    paste(global_summary$fecha_inicio, "a", global_summary$fecha_fin),
    fmt_int(global_summary$n_nulls),
    fmt_int(global_summary$n_negativos),
    fmt_int(global_summary$n_zeros),
    fmt_num(100 * global_summary$n_zeros / global_summary$total_registros),
    fmt_int(coverage_summary$mediana_span_days),
    fmt_num(coverage_summary$mediana_coverage_pct),
    fmt_int(coverage_summary$users_lt_30_days),
    fmt_int(gap_global$users_with_gaps),
    fmt_int(gap_global$total_gaps),
    fmt_int(gap_global$max_gap_days_global),
    fmt_int(total_user_days),
    fmt_num(100 * complete_days / total_user_days),
    fmt_num(spike_analysis$p95),
    fmt_num(spike_analysis$p99),
    fmt_num(spike_analysis$p999),
    fmt_int(spike_analysis$n_above_100kWh),
    fmt_int(nrow(affected_users)),
    fmt_num(sum(affected_users$kWh_afectado_horario, na.rm = TRUE))
  )
)

write.csv(quality_summary, path(TABLE_DIR, "data_quality_summary.csv"), row.names = FALSE)
message(sprintf("\nResumen exportado a: %s", path(TABLE_DIR, "data_quality_summary.csv")))
toc("8. Exportando resumen de calidad")

phase_timings <- tibble::tibble(
  fase = names(get_timings()),
  segundos = round(
    vapply(get_timings(), function(x) {
      if (!is.null(x$elapsed)) x$elapsed else NA_real_
    }, numeric(1)),
    1
  )
)

write.csv(phase_timings, path(TABLE_DIR, "data_quality_timings.csv"), row.names = FALSE)
message(sprintf("Timings exportados a: %s", path(TABLE_DIR, "data_quality_timings.csv")))

daily_manifest <- dbGetQuery(con, glue("
  SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT user_id) AS n_users,
    MIN(date) AS date_min,
    MAX(date) AS date_max,
    SUM(CASE WHEN daily_kWh IS NULL THEN 1 ELSE 0 END) AS n_null_measure
  FROM read_parquet('{daily_abs}')
"))

metadata_manifest <- dbGetQuery(con, glue("
  SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT cups) AS n_users,
    MIN(fecha_alta) AS date_min,
    MAX(fecha_baja) AS date_max,
    SUM(CASE WHEN cups IS NULL OR cups = '' THEN 1 ELSE 0 END) AS n_null_measure
  FROM read_parquet('{metadata_abs}')
"))

manifest_row <- function(step, artifact, path_value, n_rows, n_users = NA,
                         date_min = NA, date_max = NA,
                         n_null_measure = NA, notes = "") {
  tibble::tibble(
    step = step,
    artifact = artifact,
    path = as.character(path_value),
    n_rows = as.numeric(n_rows),
    n_users = as.numeric(n_users),
    date_min = as.character(date_min),
    date_max = as.character(date_max),
    n_null_measure = as.numeric(n_null_measure),
    notes = notes,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

pipeline_manifest <- bind_rows(
  manifest_row(
    "01_csv_to_parquet", "hourly_parquet", HOURLY_GLOB,
    global_summary$total_registros, global_summary$total_usuarios,
    global_summary$fecha_inicio, global_summary$fecha_fin,
    global_summary$n_nulls,
    "Dataset horario particionado por year"
  ),
  manifest_row(
    "01_csv_to_parquet", "daily_consumption", DAILY_PARQUET,
    daily_manifest$n_rows, daily_manifest$n_users,
    daily_manifest$date_min, daily_manifest$date_max,
    daily_manifest$n_null_measure,
    "Agregado diario usuario-dia"
  ),
  manifest_row(
    "01_csv_to_parquet", "metadata", METADATA_PARQUET,
    metadata_manifest$n_rows, metadata_manifest$n_users,
    metadata_manifest$date_min, metadata_manifest$date_max,
    metadata_manifest$n_null_measure,
    "Metadata contractual sin deduplicar"
  ),
  manifest_row(
    "02_data_quality", "user_coverage", path(TABLE_DIR, "user_coverage.csv"),
    nrow(user_coverage), nrow(user_coverage),
    min(as.Date(user_coverage$first_ts)), max(as.Date(user_coverage$last_ts)),
    sum(user_coverage$n_nulls, na.rm = TRUE),
    "Cobertura inclusiva: distinct_days / (span_days + 1)"
  ),
  manifest_row(
    "02_data_quality", "gap_analysis", path(TABLE_DIR, "gap_analysis.csv"),
    nrow(gap_analysis), dplyr::n_distinct(gap_analysis$user_id),
    if (nrow(gap_analysis) > 0) min(as.Date(gap_analysis$gap_start)) else NA,
    if (nrow(gap_analysis) > 0) max(as.Date(gap_analysis$gap_end)) else NA,
    NA,
    "Huecos entre dias observados"
  ),
  manifest_row(
    "02_data_quality", "schema_checks", path(TABLE_DIR, "schema_checks.csv"),
    nrow(schema_checks), NA, NA, NA,
    sum(schema_checks$status != "pass"),
    "Contratos de esquema/rango; n_null_measure cuenta warnings/fallos"
  ),
  manifest_row(
    "02_data_quality", "data_quality_affected_users", path(TABLE_DIR, "data_quality_affected_users.csv"),
    nrow(affected_users), nrow(affected_users), NA, NA,
    sum(affected_users$n_negativos + affected_users$n_extremos_horarios +
          affected_users$n_dias_negativos + affected_users$n_dias_extremos, na.rm = TRUE),
    "Usuarios con lecturas negativas o consumos por encima de umbrales horarios/diarios"
  ),
  manifest_row(
    "02_data_quality", "data_quality_summary", path(TABLE_DIR, "data_quality_summary.csv"),
    nrow(quality_summary), NA, NA, NA, NA,
    "Resumen legible para memoria"
  )
)

write.csv(pipeline_manifest, path(TABLE_DIR, "pipeline_manifest.csv"), row.names = FALSE)
message(sprintf("Manifest exportado a: %s", path(TABLE_DIR, "pipeline_manifest.csv")))

# --- Cierre ---
DBI::dbDisconnect(con, shutdown = TRUE)

elapsed_total <- (proc.time() - t0_total)["elapsed"]
message(sprintf("\nPaso 02 completado en %.1f s.", elapsed_total))

