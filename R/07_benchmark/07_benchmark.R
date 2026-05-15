#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 08: Benchmark de arquitectura de datos
# ==============================================================================
#
# Demuestra el valor de la arquitectura Parquet + DuckDB comparandola con
# enfoques mas tradicionales (CSV + R base / readr). El objetivo no es una
# comparativa universal contra Spark, sino justificar que este stack es
# suficiente para un entorno local reproducible en este TFM.
#
# Experimentos:
#   1. Lectura completa: CSV vs Parquet vs DuckDB
#   2. Seleccion columnar: leer solo 2 columnas
#   3. Filtro temporal: leer un mes concreto (predicate pushdown)
#   4. Agregacion diaria: GROUP BY sobre datos crudos vs tabla pre-agregada
#   5. Consulta analitica compleja: perfil horario por region y estacion
#   6. Tamano en disco: comparativa de formatos
#
# Cada medicion se repite 5 veces. Se reporta la primera iteracion como
# aproximacion fria de sesion y la mediana de iteraciones posteriores como
# medicion caliente; no se vacia la cache del sistema operativo.
#
# Inputs:
#   data/extracted/imputed_goiener_v7/*.csv  (CSV originales)
#   data/parquet/year=*/*.parquet            (Parquet particionado)
#   data/parquet/daily_consumption.parquet   (tabla pre-agregada)
#
# Outputs:
#   outputs/tables/benchmark_results.csv
#   outputs/tables/benchmark_disk_size.csv
#   outputs/tables/benchmark_environment.csv
#   outputs/figures/08_*.png
#
# Dependencias:
#   bench, DBI, duckdb, readr, arrow, dplyr, ggplot2, fs
#
# Uso:
#   Rscript R/07_benchmark/07_benchmark.R
# ==============================================================================

suppressPackageStartupMessages({
  library(bench)
  library(DBI)
  library(duckdb)
  library(readr)
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(fs)
})

source(here::here("_config.R"))

message("=" |> strrep(60))
message("PASO 08: Benchmark de arquitectura de datos")
message("=" |> strrep(60))

t0_total <- proc.time()

# --- Verificaciones ---
csv_dir <- EXTRACTED_DIR
stopifnot(
  "Falta carpeta de CSV extraidos" = dir_exists(csv_dir),
  "Falta directorio Parquet" = dir_exists(PARQUET_DIR),
  "Falta daily_consumption.parquet" = file_exists(DAILY_PARQUET)
)

csv_files <- dir_ls(csv_dir, glob = "*.csv")
n_csv <- length(csv_files)
message(sprintf("  CSV disponibles: %s", fmt_int(n_csv)))

# Seleccionar una muestra de CSV para los benchmarks de lectura individual
# (leer todos los CSV es innecesariamente lento para un benchmark)
set.seed(SEED)
csv_sample <- sample(csv_files, min(100, n_csv))

hourly_glob <- HOURLY_GLOB
daily_abs <- path_abs(DAILY_PARQUET) |> path_norm()
hourly_partition_dirs <- dir_ls(PARQUET_DIR, type = "directory")
hourly_partition_dirs <- hourly_partition_dirs[grepl("year=", path_file(hourly_partition_dirs), fixed = TRUE)]
hourly_files <- Sys.glob(hourly_glob)

stopifnot(
  "No se encontraron particiones year=* en PARQUET_DIR" = length(hourly_partition_dirs) > 0,
  "No se encontraron ficheros Parquet horarios" = length(hourly_files) > 0
)

open_hourly_dataset <- function() {
  # Abrir solo el dataset horario particionado; PARQUET_DIR tambien contiene
  # tablas auxiliares con esquemas distintos (p. ej. otras columnas `year`).
  datasets <- lapply(
    as.character(hourly_partition_dirs),
    open_dataset,
    format = "parquet",
    partitioning = hive_partition()
  )
  open_dataset(datasets)
}

bench_median_seconds <- function(bm) {
  as.numeric(bm[["median"]][[1]])
}

bench_times_seconds <- function(bm) {
  times <- suppressWarnings(as.numeric(bm[["time"]][[1]]))
  times[is.finite(times)]
}

bench_first_seconds <- function(bm) {
  times <- bench_times_seconds(bm)
  if (length(times) == 0) return(NA_real_)
  times[[1]]
}

bench_warm_median_seconds <- function(bm) {
  times <- bench_times_seconds(bm)
  if (length(times) >= 2) return(stats::median(times[-1], na.rm = TRUE))
  if (length(times) == 1) return(times[[1]])
  NA_real_
}

bench_mem_alloc_mb <- function(bm) {
  bytes <- suppressWarnings(as.numeric(bm[["mem_alloc"]][[1]]))
  if (!is.finite(bytes)) return(NA_real_)
  round(bytes / 1024^2, 2)
}

bench_result_row <- function(experimento, metodo, bm) {
  tibble(
    experimento = experimento,
    metodo = metodo,
    first_iter_s = bench_first_seconds(bm),
    warm_mediana_s = bench_warm_median_seconds(bm),
    mediana_s = bench_median_seconds(bm),
    mem_alloc_mb = bench_mem_alloc_mb(bm)
  )
}

read_ps_value <- function(command) {
  if (.Platform$OS.type != "windows") return(NA_character_)
  out <- suppressWarnings(system2(
    "powershell",
    args = c("-NoProfile", "-Command", command),
    stdout = TRUE,
    stderr = FALSE
  ))
  out <- trimws(out[nzchar(trimws(out))])
  if (length(out) == 0) return(NA_character_)
  paste(out, collapse = " ")
}

get_process_peak_working_set_mb <- function() {
  if (.Platform$OS.type != "windows") return(NA_real_)
  out <- read_ps_value(sprintf("(Get-Process -Id %d).PeakWorkingSet64", Sys.getpid()))
  bytes <- suppressWarnings(as.numeric(out))
  if (!is.finite(bytes)) return(NA_real_)
  round(bytes / 1024^2, 1)
}

# Informacion del sistema para reproducibilidad
sys_info <- list(
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  platform = R.version$platform,
  os = paste(na.omit(c(
    read_ps_value("(Get-CimInstance Win32_OperatingSystem).Caption"),
    read_ps_value("(Get-CimInstance Win32_OperatingSystem).Version")
  )), collapse = " | "),
  cpu = read_ps_value("(Get-CimInstance Win32_Processor | Select-Object -First 1 -ExpandProperty Name)"),
  logical_cores = parallel::detectCores(logical = TRUE),
  ram_gb = suppressWarnings(round(as.numeric(read_ps_value(
    "(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory"
  )) / 1024^3, 1)),
  duckdb_version = packageVersion("duckdb") |> as.character(),
  arrow_version = packageVersion("arrow") |> as.character(),
  bench_version = packageVersion("bench") |> as.character()
)

message(sprintf("  R %s | DuckDB %s | Arrow %s | %s",
                sys_info$r_version, sys_info$duckdb_version,
                sys_info$arrow_version, sys_info$os))
message(sprintf("  CPU: %s | RAM fisica: %s GB | cores logicos: %s",
                sys_info$cpu, sys_info$ram_gb, sys_info$logical_cores))

N_ITER <- 5  # Repeticiones por experimento
cache_context <- paste(
  "No se vacia la cache del sistema operativo entre iteraciones.",
  "first_iter_s aproxima la primera ejecucion de la sesion y warm_mediana_s resume iteraciones 2-5.",
  sprintf("DuckDB object cache configurada: %s.", DUCKDB_ENABLE_OBJECT_CACHE)
)
message(sprintf("  Contexto cache: %s", cache_context))
results <- list()

# ==============================================================================
# Experimento 1: Lectura completa de una muestra de CSV vs Parquet
# ==============================================================================
message("\n[1/6] Exp. 1: Lectura completa (100 CSV vs Parquet equivalente)...")

# Preparar la lista de CSV como cadena para DuckDB
csv_sample_str <- paste0("'", path_abs(csv_sample) |> path_norm(), "'", collapse = ", ")

# 1a. Lectura CSV con readr (100 ficheros). Para comparacion justa con los
#     motores Parquet (que filtran user_id), aqui tambien se materializa la
#     misma tabla concatenando los 100 CSV (todos pertenecen a user_ids de la
#     muestra), y se devuelve nrow().
bm1_csv <- bench::mark(
  csv_readr = {
    dfs <- lapply(csv_sample, read_csv, show_col_types = FALSE)
    combined <- bind_rows(dfs)
    nrow(combined)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

# 1b. Lectura Parquet con Arrow.
# Leemos el dataset completo, aplicamos pushdown del filtro de user_id y
# materializamos solo la columna count() (mismo trabajo que DuckDB COUNT*),
# para que la comparacion no penalice a Arrow por recolectar todas las
# columnas de cientos de miles de filas en memoria de R.
sample_users <- gsub("\\.csv$", "", path_file(csv_sample))
user_filter_file <- path(tempdir(), "sample_users.parquet")
arrow::write_parquet(data.frame(user_id = sample_users), user_filter_file)
user_filter_abs <- path_abs(user_filter_file) |> path_norm()

bm1_parquet <- bench::mark(
  parquet_arrow = {
    ds <- open_hourly_dataset()
    n_rows <- ds |>
      filter(user_id %in% sample_users) |>
      summarise(n = n()) |>
      collect()
    n_rows$n
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

# 1c. Lectura Parquet con DuckDB
con <- connect_duckdb()

bm1_duckdb <- bench::mark(
  duckdb_read = {
    r <- dbGetQuery(con, glue("
      SELECT COUNT(*) AS n
      FROM read_parquet('{hourly_glob}', hive_partitioning = TRUE)
      WHERE user_id IN (SELECT user_id FROM read_parquet('{user_filter_abs}'))
    "))
    r$n
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

results[["exp1_csv_readr"]]     <- bench_result_row("1. Lectura completa", "CSV + readr",       bm1_csv)
results[["exp1_parquet_arrow"]] <- bench_result_row("1. Lectura completa", "Parquet + Arrow",    bm1_parquet)
results[["exp1_duckdb"]]        <- bench_result_row("1. Lectura completa", "Parquet + DuckDB",   bm1_duckdb)

message("  OK")

# ==============================================================================
# Experimento 2: Seleccion columnar
# ==============================================================================
message("\n[2/6] Exp. 2: Seleccion columnar (solo user_id + kWh)...")

bm2_csv <- bench::mark(
  csv_2cols = {
    dfs <- lapply(csv_sample[1:20], function(f) {
      df <- read_csv(f, show_col_types = FALSE)
      df[, c("index", "kWh")]
    })
    nrow(bind_rows(dfs))
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

bm2_parquet <- bench::mark(
  parquet_2cols = {
    ds <- open_hourly_dataset()
    result <- ds |>
      filter(user_id %in% sample_users[1:20]) |>
      select(user_id, kWh) |>
      collect()
    nrow(result)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

results[["exp2_csv"]]     <- bench_result_row("2. Seleccion columnar", "CSV (todas columnas -> filtrar)", bm2_csv)
sample_users_20_sql <- paste(sprintf("('%s')", sample_users[1:20]), collapse = ", ")
bm2_duckdb <- bench::mark(
  duckdb_2cols = {
    r <- dbGetQuery(con, glue("
      SELECT user_id, kWh
      FROM read_parquet('{hourly_glob}', hive_partitioning = TRUE)
      WHERE user_id IN (
        SELECT user_id FROM (VALUES {sample_users_20_sql}) AS t(user_id)
      )
    "))
    nrow(r)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

results[["exp2_parquet"]] <- bench_result_row("2. Seleccion columnar", "Parquet + Arrow (col pushdown)",  bm2_parquet)
results[["exp2_duckdb"]]  <- bench_result_row("2. Seleccion columnar", "Parquet + DuckDB (col pushdown)", bm2_duckdb)

message("  OK")

# ==============================================================================
# Experimento 3: Filtro temporal (enero 2020)
# ==============================================================================
message("\n[3/6] Exp. 3: Filtro temporal (enero 2020)...")

bm3_duckdb_nopart <- bench::mark(
  duckdb_scan_all = {
    r <- dbGetQuery(con, glue("
      SELECT COUNT(*), SUM(kWh)
      FROM read_parquet('{hourly_glob}', hive_partitioning = TRUE)
      WHERE timestamp >= '2020-01-01' AND timestamp < '2020-02-01'
    "))
    r
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

# Con particionado (solo lee year=2020)
parquet_2020_glob <- path(PARQUET_DIR, "year=2020", "*.parquet") |> path_abs() |> path_norm()

bm3_duckdb_part <- bench::mark(
  duckdb_partition = {
    r <- dbGetQuery(con, glue("
      SELECT COUNT(*), SUM(kWh)
      FROM read_parquet('{parquet_2020_glob}', hive_partitioning = TRUE)
      WHERE timestamp >= '2020-01-01' AND timestamp < '2020-02-01'
    "))
    r
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

results[["exp3_scan"]] <- bench_result_row("3. Filtro temporal", "DuckDB scan completo", bm3_duckdb_nopart)
results[["exp3_part"]] <- bench_result_row("3. Filtro temporal", "DuckDB con particion",  bm3_duckdb_part)

message("  OK")

# ==============================================================================
# Experimento 4: Agregacion diaria desde horario vs tabla pre-agregada
# ==============================================================================
message("\n[4/6] Exp. 4: Agregacion diaria (from scratch vs pre-agregada)...")

bm4_scratch <- bench::mark(
  agg_from_hourly = {
    r <- dbGetQuery(con, glue("
      SELECT user_id, DATE(timestamp) AS date, SUM(kWh) AS daily_kWh
      FROM read_parquet('{parquet_2020_glob}', hive_partitioning = TRUE)
      WHERE kWh IS NOT NULL AND kWh >= 0
      GROUP BY user_id, DATE(timestamp)
    "))
    nrow(r)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

bm4_preagg <- bench::mark(
  read_preagg = {
    r <- dbGetQuery(con, glue("
      SELECT user_id, date, daily_kWh
      FROM read_parquet('{daily_abs}')
      WHERE YEAR(date) = 2020
    "))
    nrow(r)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

results[["exp4_scratch"]] <- bench_result_row("4. Agregacion diaria", "GROUP BY sobre horario", bm4_scratch)
results[["exp4_preagg"]]  <- bench_result_row("4. Agregacion diaria", "Tabla pre-agregada",     bm4_preagg)

message("  OK")

# ==============================================================================
# Experimento 5: Consulta analitica compleja
# ==============================================================================
message("\n[5/6] Exp. 5: Consulta analitica compleja (perfil horario por region)...")

metadata_abs <- path_abs(METADATA_PARQUET) |> path_norm()

bm5_complex <- bench::mark(
  complex_query = {
    r <- dbGetQuery(con, glue("
      WITH meta AS (
        SELECT cups AS user_id,
               SUBSTR(LPAD(CAST(codigo_postal AS VARCHAR), 5, '0'), 1, 2) AS cod_prov
        FROM read_parquet('{metadata_abs}')
      )
      SELECT
        CASE WHEN m.cod_prov IN ('01','20','48') THEN 'Pais Vasco'
             WHEN m.cod_prov = '28' THEN 'Madrid'
             ELSE 'Otro'
        END AS region,
        EXTRACT('hour' FROM h.timestamp)::INTEGER AS hour,
        AVG(h.kWh) AS mean_kWh
      FROM read_parquet('{parquet_2020_glob}', hive_partitioning = TRUE) h
      INNER JOIN meta m ON h.user_id = m.user_id
      WHERE h.kWh IS NOT NULL AND h.kWh >= 0
      GROUP BY 1, 2
      ORDER BY 1, 2
    "))
    nrow(r)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

# Misma consulta usando perfiles pre-agregados
user_hourly_abs <- path_abs(USER_HOURLY_PROFILE) |> path_norm()
user_region_abs <- path_abs(USER_REGION_MAP) |> path_norm()

bm5_preagg <- bench::mark(
  preagg_query = {
    r <- dbGetQuery(con, glue("
      SELECT r.region, p.hour, AVG(p.mean_kWh_user) AS mean_kWh
      FROM read_parquet('{user_hourly_abs}') p
      INNER JOIN read_parquet('{user_region_abs}') r ON p.user_id = r.user_id
      GROUP BY 1, 2
      ORDER BY 1, 2
    "))
    nrow(r)
  },
  iterations = N_ITER,
  check = FALSE,
  filter_gc = FALSE
)

results[["exp5_complex"]] <- bench_result_row("5. Consulta compleja", "Desde horario bruto",   bm5_complex)
results[["exp5_preagg"]]  <- bench_result_row("5. Consulta compleja", "Tablas pre-agregadas",  bm5_preagg)

DBI::dbDisconnect(con, shutdown = TRUE)
message("  OK")

# ==============================================================================
# Experimento 6: Tamano en disco
# ==============================================================================
message("\n[6/6] Exp. 6: Tamano en disco...")

# CSV
csv_total_bytes <- sum(file_size(csv_files))

# Parquet (solo particiones horarias, sin tablas auxiliares)
parquet_hourly_files <- hourly_files
parquet_hourly_bytes <- sum(file_size(parquet_hourly_files))

# Parquet total (incluyendo tablas auxiliares)
all_parquet_files <- dir_ls(PARQUET_DIR, recurse = TRUE, glob = "*.parquet")
parquet_total_bytes <- sum(file_size(all_parquet_files))

# Raw comprimido
raw_file <- path(RAW_DIR, "imputed_goiener_v7.tar.zst")
raw_bytes <- if (file_exists(raw_file)) file_size(raw_file) else NA

disk_summary <- tibble::tibble(
  formato = c("CSV crudo (extraido)", "Parquet horario (ZSTD-9)",
              "Parquet total (+ auxiliares)", "tar.zst original"),
  bytes = c(csv_total_bytes, parquet_hourly_bytes, parquet_total_bytes, raw_bytes),
  MB = round(bytes / 1024^2, 1),
  GB = round(bytes / 1024^3, 2)
)

# Ratio de compresion
disk_summary$ratio_vs_csv <- round(as.numeric(csv_total_bytes) / disk_summary$bytes, 1)

message("  Tamano en disco:")
print(as.data.frame(disk_summary))

write.csv(disk_summary, path(TABLE_DIR, "benchmark_disk_size.csv"), row.names = FALSE)

# ==============================================================================
# Consolidar y exportar resultados
# ==============================================================================
message("\nConsolidando resultados...")

bench_df <- bind_rows(results)

# Calcular speedup respecto al metodo mas lento de cada experimento
bench_df <- bench_df |>
  group_by(experimento) |>
  mutate(
    max_time = max(mediana_s),
    speedup = round(max_time / mediana_s, 1)
  ) |>
  ungroup() |>
  select(-max_time)

message("\nResultados del benchmark:")
print(as.data.frame(bench_df))

write.csv(bench_df, path(TABLE_DIR, "benchmark_results.csv"), row.names = FALSE)

benchmark_environment <- tibble::tibble(
  item = c(
    "timestamp",
    "r_version",
    "r_platform",
    "os",
    "cpu",
    "logical_cores",
    "physical_ram_gb",
    "duckdb_version",
    "arrow_version",
    "bench_version",
    "duckdb_memory_limit",
    "duckdb_object_cache",
    "iterations_per_experiment",
    "cache_context",
    "process_peak_working_set_mb",
    "memory_note",
    "interpretation",
    "spark_scope_note",
    "benchmark_role"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    sys_info$r_version,
    sys_info$platform,
    sys_info$os,
    sys_info$cpu,
    as.character(sys_info$logical_cores),
    as.character(sys_info$ram_gb),
    sys_info$duckdb_version,
    sys_info$arrow_version,
    sys_info$bench_version,
    DUCKDB_MEMORY_LIMIT,
    as.character(DUCKDB_ENABLE_OBJECT_CACHE),
    as.character(N_ITER),
    cache_context,
    as.character(get_process_peak_working_set_mb()),
    "mem_alloc_mb es memoria asignada estimada por bench; process_peak_working_set_mb es el pico del proceso R completo en Windows.",
    "Suficiente para entorno local reproducible en este TFM; no es una comparativa universal contra Spark.",
    "Si el dataset creciera 100x o se exigiera computo distribuido multiusuario, Spark podria volver a tener sentido.",
    "Anexo fuerte de arquitectura; no es el centro metodologico del TFM."
  )
)

write.csv(benchmark_environment,
          path(TABLE_DIR, "benchmark_environment.csv"),
          row.names = FALSE)

message("\nContexto de ejecucion:")
print(as.data.frame(benchmark_environment))

# --- Grafico de barras ---
p_bench <- bench_df |>
  mutate(
    label = sprintf("%.2fs (x%.1f)", mediana_s, speedup),
    metodo = forcats::fct_reorder(metodo, mediana_s)
  ) |>
  ggplot(aes(x = metodo, y = mediana_s, fill = metodo)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = label), hjust = -0.05, size = 3) +
  coord_flip() +
  facet_wrap(~experimento, scales = "free", ncol = 1) +
  scale_fill_viridis_d(option = "mako", direction = -1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Benchmark: CSV vs Parquet vs DuckDB",
    subtitle = sprintf("Mediana de %d iteraciones | cache SO no vaciada | R %s | DuckDB %s",
                        N_ITER, sys_info$r_version, sys_info$duckdb_version),
    x = NULL, y = "Tiempo (segundos)"
  ) +
  theme_goiener() +
  theme(strip.text = element_text(face = "bold"))

ggsave(path(FIG_DIR, "07_benchmark_comparison.png"), p_bench,
       width = 12, height = 14, dpi = 300)

# Tamano en disco
p_disk <- disk_summary |>
  filter(!is.na(MB)) |>
  mutate(formato = forcats::fct_reorder(formato, MB)) |>
  ggplot(aes(x = formato, y = MB, fill = formato)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = sprintf("%.0f MB (x%.1f)", MB, ratio_vs_csv)),
            hjust = -0.05, size = 3.5) +
  coord_flip() +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(
    title = "Comparativa de tamano en disco",
    subtitle = "Ratio respecto al CSV crudo extraido",
    x = NULL, y = "Tamano (MB)"
  ) +
  theme_goiener()

ggsave(path(FIG_DIR, "08_disk_size_comparison.png"), p_disk,
       width = 10, height = 5, dpi = 300)

elapsed_total <- (proc.time() - t0_total)["elapsed"]
message(sprintf("\nPaso 08 completado en %.1f s.", elapsed_total))

