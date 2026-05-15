#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 01: Conversion de CSV a Parquet con DuckDB
# ==============================================================================
#
# Este script transforma los CSV horarios del dataset GoiEner (imputed_goiener_v7)
# en un dataset Parquet particionado por anio, y ademas genera tablas agregadas
# (diaria y mensual) para agilizar el analisis exploratorio posterior.
#
# Por que Parquet?
#   - Es un formato columnar: las lecturas por columna son mucho mas rapidas
#     que leer CSV fila a fila, sobre todo cuando solo necesitamos unas pocas
#     columnas de las disponibles.
#   - Comprime muy bien datos tabulares (aqui usamos ZSTD nivel 9).
#   - Mantiene tipos de datos explicitos, evitando problemas de inferencia.
#
# Optimizaciones aplicadas:
#   - ZSTD nivel 9: buen equilibrio entre ratio de compresion y velocidad.
#   - ROW_GROUP_SIZE 500 000: bloques grandes que reducen el overhead de metadatos
#     y mejoran la lectura secuencial.
#   - Eliminacion de columnas redundantes del CSV original (index, filename).
#   - Tipos explicitos (TIMESTAMP, FLOAT, VARCHAR) para que DuckDB no tenga
#     que adivinar el tipo de cada columna.
#   - Tablas diaria y mensual pre-agregadas para que el EDA no tenga que
#     recorrer millones de filas horarias cada vez.
#   - Perfiles horarios pre-agregados por usuario para que el EDA no tenga
#     que recalcular los mismos GROUP BY sobre el dataset horario completo.
#
# Estructura de entrada esperada:
#   data/extracted/imputed_goiener_v7/*.csv   (un CSV por punto de suministro)
#   data/raw/metadata.csv                     (metadatos de contratos)
#
# Estructura de salida:
#   data/parquet/year=YYYY/*.parquet          (lecturas horarias particionadas)
#   data/parquet/daily_consumption.parquet    (resumen diario por usuario)
#   data/parquet/monthly_consumption.parquet  (resumen mensual por usuario)
#   data/parquet/user_hourly_profile.parquet  (media horaria por usuario)
#   data/parquet/user_dow_type_profile.parquet (media por usuario, tipo de dia y hora)
#   data/parquet/user_season_profile.parquet  (media por usuario, estacion y hora)
#   data/parquet/user_weekday_profile.parquet (media por usuario y dia de la semana)
#   data/parquet/user_region_map.parquet      (region geografica por usuario)
#   data/parquet/metadata.parquet             (metadatos en formato Parquet)
#
# Uso:
#   Rscript R/01_csv_to_parquet.R
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)      # Interfaz generica para bases de datos en R
  library(duckdb)   # Motor SQL analitico embebido, muy rapido para datos locales
  library(fs)       # Manipulacion de ficheros multiplataforma
  library(glue)     # Interpolacion de cadenas para construir consultas SQL
  library(here)     # Resolucion de rutas relativas al proyecto
})

source(here::here("_config.R"))

# ==============================================================================
# Configuracion de rutas
# ==============================================================================
# Todas las rutas se construyen de forma relativa al directorio raiz del
# proyecto gracias a here::here(). Esto hace que el script funcione igual
# independientemente de desde donde se lance.

data_dir      <- DATA_DIR
raw_dir       <- RAW_DIR
extracted_dir <- EXTRACTED_DIR
parquet_dir   <- PARQUET_DIR

# Crear los directorios de salida si no existen todavia
if (!dir_exists(data_dir))    dir_create(data_dir)
if (!dir_exists(raw_dir))     dir_create(raw_dir)
if (!dir_exists(parquet_dir)) dir_create(parquet_dir)

metadata_csv     <- path(raw_dir, "metadata.csv")
metadata_parquet <- METADATA_PARQUET

if (!file_exists(metadata_csv)) {
  stop("No existe metadata.csv: ", metadata_csv)
}

# Verificar que los CSV extraidos existen. Si no, hay que ejecutar primero
# R/00_extract_raw.R para descomprimir el .tar.zst.
if (!dir_exists(extracted_dir)) {
  stop("No existe la carpeta de CSV extraidos: ", extracted_dir)
}

csv_files <- dir_ls(extracted_dir, glob = "*.csv")
if (length(csv_files) == 0) {
  stop("No se han encontrado CSV en: ", extracted_dir)
}

message("CSV detectados: ", length(csv_files))
message("Salida parquet: ", parquet_dir)

# Cronometro global para medir el tiempo total del script
t0_total <- proc.time()

# ==============================================================================
# Conexion a DuckDB en memoria
# ==============================================================================
# Usamos una base de datos en memoria (":memory:") porque no necesitamos
# persistir nada: solo queremos aprovechar el motor SQL de DuckDB para
# transformar los datos de CSV a Parquet de forma eficiente.

con <- connect_duckdb()

# Construir las rutas absolutas que DuckDB necesita para leer/escribir.
# DuckDB trabaja con rutas completas, no con objetos de R.
csv_glob    <- path(extracted_dir, "*.csv") |> path_abs() |> path_norm()
parquet_abs <- path_abs(parquet_dir) |> path_norm()

# Limpiar particiones anteriores para evitar mezclar datos viejos con nuevos.
# Esto es especialmente importante si los CSV de origen han cambiado.
year_dirs <- dir_ls(parquet_dir, glob = "year=*", type = "directory", recurse = FALSE, fail = FALSE)
if (length(year_dirs) > 0) {
  dir_delete(year_dirs)
}

# ==============================================================================
# PASO 1: CSV -> Parquet horario particionado por anio
# ==============================================================================
# Cada CSV tiene columnas: index (timestamp), kWh (consumo).
# El nombre del fichero contiene el hash SHA-256 del CUPS del usuario.
#
# Lo que hacemos aqui:
#   1. Extraemos el user_id del nombre del fichero (los 64 caracteres del hash)
#   2. Convertimos la columna "index" a tipo TIMESTAMP
#   3. Creamos columnas auxiliares year y month para el particionado
#   4. Escribimos todo a Parquet particionado por year (year=2014, year=2015, ...)
#
# El particionado por anio permite que las consultas que filtran por rango de
# fechas solo lean los ficheros de los anios relevantes (predicate pushdown).

message("Convirtiendo CSV a Parquet particionado...")
t0 <- proc.time()

query_to_parquet <- glue("
COPY (
    SELECT
        -- Extraer el hash del usuario del nombre del fichero CSV.
        -- Cada fichero se llama <hash_64_chars>.csv, asi que recortamos
        -- los ultimos 68 caracteres (64 del hash + 4 de '.csv') y nos
        -- quedamos con los 64 del hash.
        SUBSTR(filename, LENGTH(filename) - 67, 64)::VARCHAR AS user_id,

        -- La columna 'index' del CSV contiene el timestamp en formato texto.
        -- La convertimos a tipo TIMESTAMP nativo para poder operar con fechas.
        CAST(\"index\" AS TIMESTAMP)                AS timestamp,

        -- Columnas derivadas para el particionado y futuras agregaciones.
        YEAR(CAST(\"index\" AS TIMESTAMP))          AS year,
        MONTH(CAST(\"index\" AS TIMESTAMP))         AS month,

        -- Consumo horario en kWh. Lo forzamos a FLOAT para ahorrar espacio
        -- (no necesitamos la precision de DOUBLE para valores de consumo).
        kWh::FLOAT                                  AS kWh
    FROM read_csv(
        '{csv_glob}',
        filename    = TRUE,      -- Aniadir columna 'filename' con la ruta del CSV
        header      = TRUE,
        columns     = {{
          'index': 'TIMESTAMP',
          'fl': 'INTEGER',
          'kWh': 'DOUBLE',
          'imp': 'INTEGER'
        }},
        nullstr     = 'NA'       -- Interpretar 'NA' como valor nulo
    )
)
TO '{parquet_abs}'
(
    FORMAT PARQUET,
    PARTITION_BY (year),         -- Crear una carpeta year=YYYY por cada anio
    CODEC 'ZSTD',               -- Compresion Zstandard (rapida y eficiente)
    COMPRESSION_LEVEL 9,        -- Nivel alto de compresion (mas lento al escribir, mas compacto)
    ROW_GROUP_SIZE 500000,       -- Grupos de 500k filas (menos metadatos, mejor lectura secuencial)
    OVERWRITE TRUE               -- Sobreescribir si ya existia
);
")

DBI::dbExecute(con, query_to_parquet)
elapsed <- (proc.time() - t0)["elapsed"]
message(sprintf("Conversion horaria completada en %.1f s.", elapsed))

# ==============================================================================
# PASO 2: Metadata CSV -> Parquet
# ==============================================================================
# El fichero metadata.csv contiene informacion contractual de cada punto de
# suministro: potencia contratada, codigo postal, CNAE, tipo de tarifa, etc.
# Lo convertimos a Parquet para poder cruzarlo con los datos de consumo en el
# EDA sin tener que leer CSV cada vez.
if (file_exists(metadata_csv)) {
  metadata_csv_abs     <- path_abs(metadata_csv)     |> path_norm()
  metadata_parquet_abs <- path_abs(metadata_parquet)  |> path_norm()

  # Borrar version anterior si existe, para regenerar siempre limpio
  if (file_exists(metadata_parquet)) {
    file_delete(metadata_parquet)
  }

  # La consulta es directa: leer todo el CSV y volcarlo a Parquet con ZSTD.
  # sample_size = -1 obliga a leer todas las filas para inferir bien los tipos.
  query_metadata <- glue("
  COPY (
      SELECT *
      FROM read_csv_auto(
          '{metadata_csv_abs}',
          sample_size = -1,
          nullstr     = 'NA'
      )
  )
  TO '{metadata_parquet_abs}'
  (
      FORMAT PARQUET,
      CODEC 'ZSTD',
      COMPRESSION_LEVEL 9,
      OVERWRITE TRUE
  );
  ")
  message("Convirtiendo metadata.csv a Parquet...")
  t0_meta <- proc.time()
  dbExecute(con, query_metadata)
  elapsed_meta <- (proc.time() - t0_meta)["elapsed"]
  message(sprintf("Metadata Parquet creada en %.1f s: %s", elapsed_meta, metadata_parquet))
} else {
  message("No se encontro metadata.csv en ", metadata_csv, ". Se omite.")
}

# ==============================================================================
# PASO 3: Tabla diaria pre-agregada
# ==============================================================================
# Para el analisis exploratorio, la inmensa mayoria de graficos trabajan con
# datos diarios (consumo por usuario y dia). Si cada vez tuvieramos que recorrer
# las decenas de millones de filas horarias, el EDA seria muy lento.
#
# Esta tabla agrega las lecturas horarias por usuario y dia, calculando:
#   - daily_kWh:        consumo total del dia
#   - mean_hourly_kWh:  consumo medio horario
#   - min/max:          valores extremos dentro del dia
#   - sd_hourly_kWh:    desviacion tipica intra-dia (mide lo irregular del perfil)
#   - hours_recorded:   numero de horas con lectura (24 = dia completo)
#
# Solo incluimos registros con kWh valido (no nulo, no negativo) y user_id no
# vacio, para que el EDA no tenga que repetir estos filtros constantemente.

daily_parquet     <- path(parquet_dir, "daily_consumption.parquet")
daily_parquet_abs <- path_abs(daily_parquet) |> path_norm()
parquet_glob      <- path(parquet_dir, "year=*", "*.parquet") |> path_abs() |> path_norm()

if (file_exists(daily_parquet)) {
  file_delete(daily_parquet)
}

query_daily <- glue("
COPY (
    SELECT
        user_id,
        DATE(timestamp)          AS date,
        SUM(kWh)                 AS daily_kWh,
        AVG(kWh)                 AS mean_hourly_kWh,
        MIN(kWh)                 AS min_hourly_kWh,
        MAX(kWh)                 AS max_hourly_kWh,
        STDDEV_SAMP(kWh)        AS sd_hourly_kWh,
        COUNT(*)                 AS hours_recorded
    FROM read_parquet(
        '{parquet_glob}',
        hive_partitioning = TRUE
    )
    WHERE kWh IS NOT NULL AND kWh >= 0 AND user_id IS NOT NULL AND user_id <> ''
    GROUP BY user_id, DATE(timestamp)
)
TO '{daily_parquet_abs}'
(
    FORMAT PARQUET,
    CODEC 'ZSTD',
    COMPRESSION_LEVEL 9,
    ROW_GROUP_SIZE 500000,
    OVERWRITE TRUE
);
")

message("Creando daily_consumption.parquet...")
t0 <- proc.time()
dbExecute(con, query_daily)
elapsed <- (proc.time() - t0)["elapsed"]
message(sprintf("Dataset diario creado en %.1f s: %s", elapsed, daily_parquet))

# ==============================================================================
# PASO 4: Tabla mensual pre-agregada
# ==============================================================================
# Idem que la diaria, pero un nivel mas arriba: agrega los datos diarios por
# usuario, anio y mes. Esto facilita graficos de evolucion interanual y
# comparativas mensuales sin tener que agregar en tiempo de consulta.
monthly_parquet     <- path(parquet_dir, "monthly_consumption.parquet")
monthly_parquet_abs <- path_abs(monthly_parquet) |> path_norm()
user_hourly_profile_path   <- path(parquet_dir, "user_hourly_profile.parquet")
user_hourly_profile_abs    <- path_abs(user_hourly_profile_path) |> path_norm()
user_dow_type_profile_path <- path(parquet_dir, "user_dow_type_profile.parquet")
user_dow_type_profile_abs  <- path_abs(user_dow_type_profile_path) |> path_norm()
user_season_profile_path   <- path(parquet_dir, "user_season_profile.parquet")
user_season_profile_abs    <- path_abs(user_season_profile_path) |> path_norm()
user_weekday_profile_path  <- path(parquet_dir, "user_weekday_profile.parquet")
user_weekday_profile_abs   <- path_abs(user_weekday_profile_path) |> path_norm()
user_region_map_path       <- path(parquet_dir, "user_region_map.parquet")
user_region_map_abs        <- path_abs(user_region_map_path) |> path_norm()

if (file_exists(monthly_parquet)) {
  file_delete(monthly_parquet)
}

query_monthly <- glue("
COPY (
    SELECT
        user_id,
        YEAR(date)               AS year,
        MONTH(date)              AS month,
        SUM(daily_kWh)           AS monthly_kWh,
        AVG(daily_kWh)           AS mean_daily_kWh,
        MAX(daily_kWh)           AS max_daily_kWh,
        STDDEV_SAMP(daily_kWh)  AS sd_daily_kWh,
        SUM(hours_recorded)      AS total_hours,
        COUNT(*)                 AS days_recorded
    FROM read_parquet(
        '{daily_parquet_abs}',
        hive_partitioning = FALSE
    )
    GROUP BY user_id, YEAR(date), MONTH(date)
)
TO '{monthly_parquet_abs}'
(
    FORMAT PARQUET,
    CODEC 'ZSTD',
    COMPRESSION_LEVEL 9,
    OVERWRITE TRUE
);
")

message("Creando monthly_consumption.parquet...")
t0_monthly <- proc.time()
dbExecute(con, query_monthly)
elapsed_monthly <- (proc.time() - t0_monthly)["elapsed"]
message(sprintf("Dataset mensual creado en %.1f s: %s", elapsed_monthly, monthly_parquet))

# ==============================================================================
# PASO 5: Perfiles horarios pre-agregados para el EDA
# ==============================================================================
# El EDA necesita varias vistas horarias calculadas a nivel de usuario. Si se
# recalculan en cada render del QMD, DuckDB tiene que recorrer ~1e9 filas del
# parquet horario. Materializamos aqui esas agregaciones una sola vez.

profile_outputs <- c(
  user_hourly_profile_path,
  user_dow_type_profile_path,
  user_season_profile_path,
  user_weekday_profile_path,
  user_region_map_path
)

existing_profile_outputs <- profile_outputs[file_exists(profile_outputs)]
if (length(existing_profile_outputs) > 0) {
  file_delete(existing_profile_outputs)
}

query_user_hourly_profile <- glue("\
COPY (\
  SELECT\
    user_id,\
    EXTRACT('hour' FROM timestamp)::INTEGER AS hour,\
    AVG(kWh) AS mean_kWh_user\
  FROM read_parquet('{parquet_glob}', hive_partitioning = TRUE)\
  WHERE kWh IS NOT NULL AND kWh >= 0\
    AND user_id IS NOT NULL AND user_id <> ''\
  GROUP BY 1, 2\
)\
TO '{user_hourly_profile_abs}'\
(\
  FORMAT PARQUET,\
  CODEC 'ZSTD',\
  COMPRESSION_LEVEL 9,\
  OVERWRITE TRUE\
);\
")

message("Creando user_hourly_profile.parquet...")
t0_hourly_profile <- proc.time()
dbExecute(con, query_user_hourly_profile)
elapsed_hourly_profile <- (proc.time() - t0_hourly_profile)["elapsed"]
message(sprintf("Perfil horario por usuario creado en %.1f s: %s", elapsed_hourly_profile, user_hourly_profile_path))

query_user_dow_type_profile <- glue("\
COPY (\
  SELECT\
    user_id,\
    CASE\
      WHEN EXTRACT('dow' FROM timestamp) IN (0, 6) THEN 'Fin de semana'\
      ELSE 'Laborable'\
    END AS tipo_dia,\
    EXTRACT('hour' FROM timestamp)::INTEGER AS hour,\
    AVG(kWh) AS mean_kWh_user\
  FROM read_parquet('{parquet_glob}', hive_partitioning = TRUE)\
  WHERE kWh IS NOT NULL AND kWh >= 0\
    AND user_id IS NOT NULL AND user_id <> ''\
  GROUP BY 1, 2, 3\
)\
TO '{user_dow_type_profile_abs}'\
(\
  FORMAT PARQUET,\
  CODEC 'ZSTD',\
  COMPRESSION_LEVEL 9,\
  OVERWRITE TRUE\
);\
")

message("Creando user_dow_type_profile.parquet...")
t0_dow_type_profile <- proc.time()
dbExecute(con, query_user_dow_type_profile)
elapsed_dow_type_profile <- (proc.time() - t0_dow_type_profile)["elapsed"]
message(sprintf("Perfil por tipo de dia creado en %.1f s: %s", elapsed_dow_type_profile, user_dow_type_profile_path))

query_user_season_profile <- glue("\
COPY (\
  SELECT\
    user_id,\
    CASE\
      WHEN EXTRACT('month' FROM timestamp) IN (12, 1, 2) THEN 'Invierno'\
      WHEN EXTRACT('month' FROM timestamp) IN (3, 4, 5) THEN 'Primavera'\
      WHEN EXTRACT('month' FROM timestamp) IN (6, 7, 8) THEN 'Verano'\
      ELSE 'Otono'\
    END AS season,\
    EXTRACT('hour' FROM timestamp)::INTEGER AS hour,\
    AVG(kWh) AS mean_kWh_user\
  FROM read_parquet('{parquet_glob}', hive_partitioning = TRUE)\
  WHERE kWh IS NOT NULL AND kWh >= 0\
    AND user_id IS NOT NULL AND user_id <> ''\
  GROUP BY 1, 2, 3\
)\
TO '{user_season_profile_abs}'\
(\
  FORMAT PARQUET,\
  CODEC 'ZSTD',\
  COMPRESSION_LEVEL 9,\
  OVERWRITE TRUE\
);\
")

message("Creando user_season_profile.parquet...")
t0_season_profile <- proc.time()
dbExecute(con, query_user_season_profile)
elapsed_season_profile <- (proc.time() - t0_season_profile)["elapsed"]
message(sprintf("Perfil estacional creado en %.1f s: %s", elapsed_season_profile, user_season_profile_path))

query_user_weekday_profile <- glue("\
COPY (\
  SELECT\
    user_id,\
    EXTRACT('isodow' FROM timestamp)::INTEGER AS dow_num,\
    AVG(kWh) AS mean_kWh_user\
  FROM read_parquet('{parquet_glob}', hive_partitioning = TRUE)\
  WHERE kWh IS NOT NULL AND kWh >= 0\
    AND user_id IS NOT NULL AND user_id <> ''\
  GROUP BY 1, 2\
)\
TO '{user_weekday_profile_abs}'\
(\
  FORMAT PARQUET,\
  CODEC 'ZSTD',\
  COMPRESSION_LEVEL 9,\
  OVERWRITE TRUE\
);\
")

message("Creando user_weekday_profile.parquet...")
t0_weekday_profile <- proc.time()
dbExecute(con, query_user_weekday_profile)
elapsed_weekday_profile <- (proc.time() - t0_weekday_profile)["elapsed"]
message(sprintf("Perfil por dia de la semana creado en %.1f s: %s", elapsed_weekday_profile, user_weekday_profile_path))

query_user_region_map <- glue("\
COPY (\
  WITH meta AS (\
    SELECT user_id, cod_prov\
    FROM (\
      SELECT\
        cups AS user_id,\
        SUBSTR(LPAD(CAST(codigo_postal AS VARCHAR), 5, '0'), 1, 2) AS cod_prov,\
        ROW_NUMBER() OVER (\
          PARTITION BY cups\
          ORDER BY\
            CASE WHEN tarifa_atr IS NOT NULL AND tarifa_atr <> '' THEN 0 ELSE 1 END,\
            fecha_alta DESC NULLS LAST\
        ) AS rn\
      FROM read_parquet('{metadata_parquet_abs}')\
    ) ranked\
    WHERE rn = 1\
  )\
  SELECT\
    user_id,\
    CASE\
      WHEN cod_prov IN ('01', '20', '48') THEN 'Pais Vasco'\
      WHEN cod_prov = '28' THEN 'Madrid'\
    END AS region\
  FROM meta\
  WHERE cod_prov IN ('01', '20', '48', '28')\
)\
TO '{user_region_map_abs}'\
(\
  FORMAT PARQUET,\
  CODEC 'ZSTD',\
  COMPRESSION_LEVEL 9,\
  OVERWRITE TRUE\
);\
")

message("Creando user_region_map.parquet...")
t0_region_map <- proc.time()
dbExecute(con, query_user_region_map)
elapsed_region_map <- (proc.time() - t0_region_map)["elapsed"]
message(sprintf("Mapa usuario-region creado en %.1f s: %s", elapsed_region_map, user_region_map_path))

# ==============================================================================
# PASO 6: Resumen final y validacion
# ==============================================================================
# Hacemos una consulta rapida sobre el Parquet recien creado para verificar que
# los datos tienen sentido: numero de filas, usuarios unicos, rango temporal y
# consumo total. Si el numero de usuarios es anormalmente bajo, algo fue mal
# en la extraccion del user_id del nombre del fichero.
t0_summary <- proc.time()
summary_tbl <- dbGetQuery(con, glue("
SELECT
    COUNT(*)                    AS n_rows,
    COUNT(DISTINCT user_id)     AS n_users,
    MIN(timestamp)              AS min_ts,
    MAX(timestamp)              AS max_ts,
    SUM(kWh)                    AS total_kWh
FROM read_parquet(
    '{parquet_glob}',
    hive_partitioning = TRUE
);
"))

elapsed_summary <- (proc.time() - t0_summary)["elapsed"]
message(sprintf("\n-- Resumen del dataset (consulta en %.1f s) --", elapsed_summary))
print(summary_tbl)

# Validacion basica: si hay menos de 100 usuarios, probablemente la extraccion
# del user_id desde el nombre del fichero no funciono bien.
if (summary_tbl$n_users[[1]] < 100) {
  stop("La conversion genero un numero invalido de usuarios. Revisa la extraccion de user_id.")
}

# Cerrar la conexion a DuckDB y liberar recursos
DBI::dbDisconnect(con, shutdown = TRUE)

elapsed_total <- (proc.time() - t0_total)["elapsed"]
message(sprintf("\nProceso completado. Tiempo total: %.1f s", elapsed_total))



