#!/usr/bin/env Rscript
# ==============================================================================
# R/99_orchestrator.R
# Ejecuta el pipeline completo en orden: scripts R y documentos Quarto.
#
# Uso:
#   Rscript R/99_orchestrator.R                    # R + QMD completos
#   Rscript R/99_orchestrator.R --r-only           # solo scripts R
#   Rscript R/99_orchestrator.R --qmd-only         # solo documentos QMD
#   Rscript R/99_orchestrator.R 05 --r-only        # solo fase R 05
#   Rscript R/99_orchestrator.R qmd05 --qmd-only   # solo qmd/05_forecasting.qmd
#   Rscript R/99_orchestrator.R --keep-going       # no parar al primer fallo
# ==============================================================================
suppressPackageStartupMessages({
  library(here)
  library(fs)
})

args <- commandArgs(trailingOnly = TRUE)
flags <- args[startsWith(args, "--")]
requested <- setdiff(args, flags)

run_r <- !"--qmd-only" %in% flags
run_qmd <- !"--r-only" %in% flags
keep_going <- "--keep-going" %in% flags

if (length(requested) && run_r && !any(grepl("^qmd\\d{2}$", requested))) {
  run_qmd <- FALSE
}

if (!run_r && !run_qmd) {
  stop("Seleccion contradictoria: no hay nada que ejecutar.")
}

R_PHASES <- list(
  "00" = c("R/00_extract/00_extract_raw.R"),
  "01" = c("R/01_ingest/01_csv_to_parquet.R"),
  "02" = c("R/02_quality/02_data_quality.R"),
  "03" = c(
    "R/03_climate/03a_build_climate_dataset.R",
    "R/03_climate/03b_augment_climate.R",
    "R/03_climate/03c_impute_climate.R",
    "R/03_climate/03d_merge_climate.R"
  ),
  "04" = c(
    "R/04_features/04a_legacy_features.R",
    "R/04_features/04b_climate_sensitivity.R"
  ),
  "05" = c(
    "R/05_clustering/05a_pool.R",
    "R/05_clustering/05b_matrix.R",
    "R/05_clustering/05c_models.R",
    "R/05_clustering/05d_validation.R",
    "R/05_clustering/05e_selection.R",
    "R/05_clustering/05f_interpretation.R",
    "R/05_clustering/05g_socioeconomic.R",
    "R/05_clustering/05h_business_segments.R"
  ),
  "06" = c(
    "R/06_forecasting/06a_targets.R",
    "R/06_forecasting/06b_features_temporal.R",
    "R/06_forecasting/06c_models_daily.R",
    "R/06_forecasting/06d_models_hourly.R",
    "R/06_forecasting/06e_models_cluster.R",
    "R/06_forecasting/06f_evaluation.R"
  ),
  "07" = c("R/07_benchmark/07_benchmark.R")
)

QMD_DOCS <- list(
  "01" = "qmd/01_eda.qmd",
  "02" = "qmd/02_data_quality.qmd",
  "03" = "qmd/03_climate_integration.qmd",
  "04" = "qmd/04_clustering.qmd",
  "05" = "qmd/05_forecasting.qmd",
  "06" = "qmd/06_benchmark.qmd",
  "07" = "qmd/07_etica_privacidad.qmd"
)

log_dir <- here::here("outputs", "logs")
table_dir <- here::here("outputs", "tables")
dir_create(log_dir)
dir_create(table_dir)

rscript_bin <- file.path(R.home("bin"), "Rscript.exe")
if (!file.exists(rscript_bin)) rscript_bin <- file.path(R.home("bin"), "Rscript")

quarto_bin <- Sys.which("quarto")
if (run_qmd && !nzchar(quarto_bin)) {
  stop("No se encontro el ejecutable `quarto` en PATH.")
}

select_r_phases <- function(requested_keys) {
  numeric_keys <- requested_keys[grepl("^\\d{2}$", requested_keys)]
  if (!length(numeric_keys)) return(names(R_PHASES))
  invalid <- setdiff(numeric_keys, names(R_PHASES))
  if (length(invalid)) {
    stop("Fase R no reconocida: ", paste(invalid, collapse = ", "))
  }
  numeric_keys
}

select_qmd_docs <- function(requested_keys) {
  qmd_keys <- sub("^qmd", "", requested_keys[grepl("^qmd\\d{2}$", requested_keys)])
  if (!length(qmd_keys) && !run_r) {
    qmd_keys <- requested_keys[grepl("^\\d{2}$", requested_keys)]
  }
  if (!length(qmd_keys)) return(names(QMD_DOCS))
  invalid <- setdiff(qmd_keys, names(QMD_DOCS))
  if (length(invalid)) {
    stop("Documento QMD no reconocido: ", paste(invalid, collapse = ", "))
  }
  qmd_keys
}

run_process <- function(kind, phase, artifact, command, args, log_file) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(">>> [", kind, " ", phase, "] ", artifact, "\n", sep = "")

  if (!file_exists(here::here(artifact))) {
    stop("No existe el artefacto esperado: ", artifact)
  }

  t0 <- proc.time()
  rc <- system2(command, args = args, stdout = log_file, stderr = log_file)
  elapsed <- (proc.time() - t0)[["elapsed"]]
  status <- if (identical(as.integer(rc), 0L)) "OK" else paste0("ERROR: exit=", rc)

  cat(sprintf("    %s [%.1fs] %s\n", artifact, elapsed, status))

  data.frame(
    phase = phase,
    type = kind,
    artifact = artifact,
    elapsed_s = round(elapsed, 1),
    status = status,
    log_file = as.character(log_file),
    stringsAsFactors = FALSE
  )
}

timings <- list()
failed <- FALSE

if (run_r) {
  for (phase in select_r_phases(requested)) {
    for (script in R_PHASES[[phase]]) {
      log_file <- path(log_dir, paste0("run_", path_file(script), ".txt"))
      result <- run_process(
        kind = "R",
        phase = phase,
        artifact = script,
        command = rscript_bin,
        args = shQuote(here::here(script)),
        log_file = log_file
      )
      timings[[length(timings) + 1]] <- result
      failed <- startsWith(result$status, "ERROR")
      if (failed && !keep_going) break
    }
    if (failed && !keep_going) break
  }
}

if (run_qmd && (!failed || keep_going)) {
  for (doc_id in select_qmd_docs(requested)) {
    qmd <- QMD_DOCS[[doc_id]]
    log_file <- path(log_dir, paste0("render_", path_ext_remove(path_file(qmd)), ".txt"))
    result <- run_process(
      kind = "QMD",
      phase = doc_id,
      artifact = qmd,
      command = quarto_bin,
      args = c("render", shQuote(here::here(qmd)), "--to", "html"),
      log_file = log_file
    )
    timings[[length(timings) + 1]] <- result
    failed <- startsWith(result$status, "ERROR")
    if (failed && !keep_going) break
  }
}

timings_df <- if (length(timings)) {
  do.call(rbind, timings)
} else {
  data.frame(
    phase = character(), type = character(), artifact = character(),
    elapsed_s = numeric(), status = character(), log_file = character()
  )
}

write.csv(
  timings_df,
  here::here("outputs", "tables", "pipeline_timings_v2.csv"),
  row.names = FALSE
)

print(timings_df)

if (any(startsWith(timings_df$status, "ERROR"))) {
  quit(status = 1, save = "no")
}
