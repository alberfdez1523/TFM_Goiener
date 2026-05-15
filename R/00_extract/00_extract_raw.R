#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 00: Extraccion de los CSV crudos
# ==============================================================================
#
# Este script descomprime el archivo .tar.zst que contiene los CSV horarios del
# dataset GoiEner (version imputed_goiener_v7). Cada CSV corresponde a un punto
# de suministro (CUPS) y contiene lecturas horarias de consumo electrico en kWh.
#
# El archivo comprimido se descarga previamente de la fuente original y se
# coloca en data/raw/. Este script lo extrae en data/extracted/imputed_goiener_v7/.
#
# Dependencias:
#   - archive: para descomprimir archivos .tar.zst (Zstandard)
#   - here:    para resolver rutas relativas al proyecto
#   - fs:      para manipulacion de ficheros y directorios de forma portable
#
# Salida:
#   data/extracted/imputed_goiener_v7/*.csv  (un CSV por punto de suministro)
#
# Uso:
#   Rscript R/00_extract_raw.R
# ==============================================================================

suppressPackageStartupMessages({
  library(archive)
  library(fs)
  library(here)
})

source(here::here("_config.R"))

main <- function() {

  # --- Definir rutas de entrada y salida ---
  # El archivo .tar.zst original se encuentra en data/raw/.
  # Los CSV individuales se extraeran a data/extracted/imputed_goiener_v7/.
  raw_file   <- path(RAW_DIR, "imputed_goiener_v7.tar.zst")
  output_dir <- EXTRACTED_DIR

  # Comprobacion: si el archivo comprimido no existe, no podemos continuar.
  if (!file_exists(raw_file)) {
    stop("No existe el archivo: ", raw_file)
  }

  manifest_file <- path(output_dir, "_extract_manifest.csv")
  raw_hash <- unname(tools::md5sum(raw_file))

  if (dir_exists(output_dir) && file_exists(manifest_file)) {
    manifest <- tryCatch(read.csv(manifest_file, stringsAsFactors = FALSE),
                         error = function(e) data.frame())
    existing_csv <- dir_ls(output_dir, recurse = TRUE, glob = "*.csv", fail = FALSE)
    if (nrow(manifest) == 1 &&
        identical(manifest$raw_md5[[1]], raw_hash) &&
        length(existing_csv) > 0) {
      message("Extraccion existente valida. Se omite descompresion.")
      message("CSV disponibles: ", length(existing_csv))
      message("Ruta final: ", output_dir)
      return(invisible(existing_csv))
    }
  }
  
  # Si no hay manifest valido, regeneramos la extraccion completa para evitar
  # mezclar CSV de versiones distintas.
  if (dir_exists(output_dir)) {
    dir_delete(output_dir)
  }
  dir_create(output_dir, recurse = TRUE)

  # --- Descomprimir el archivo ---
  # archive_extract maneja directamente el formato .tar.zst sin necesidad de
  # descomprimir el .zst primero y luego el .tar por separado.
  archive::archive_extract(
    archive = raw_file,
    dir = output_dir
  )

  # --- Corregir estructura de carpetas duplicadas ---
  # Algunos archivos .tar crean una carpeta interna con el mismo nombre
  # (imputed_goiener_v7/imputed_goiener_v7/). Si eso ocurre, sacamos los
  # ficheros al nivel esperado y borramos la carpeta sobrante.
  inner_dir <- path(output_dir, "imputed_goiener_v7")

  if (dir_exists(inner_dir)) {
    message("Se detecto carpeta duplicada. Recolocando archivos...")

    # Listar todos los ficheros de la subcarpeta interna
    files_to_move <- dir_ls(inner_dir, recurse = FALSE, all = TRUE)

    # Moverlos un nivel arriba, al directorio de salida esperado
    file_move(
      path = files_to_move,
      new_path = path(output_dir, path_file(files_to_move))
    )

    # Eliminar la carpeta ahora vacia
    dir_delete(inner_dir)
  }

  # --- Resumen de la extraccion ---
  csv_files <- dir_ls(output_dir, recurse = TRUE, glob = "*.csv")

  write.csv(
    data.frame(
      raw_file = path_file(raw_file),
      raw_md5 = raw_hash,
      raw_size_bytes = file_info(raw_file)$size,
      extracted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      csv_count = length(csv_files)
    ),
    manifest_file,
    row.names = FALSE
  )

  message("CSV extraidos: ", length(csv_files))
  message("Ruta final: ", output_dir)
}

# Ejecutar la funcion principal
main()

