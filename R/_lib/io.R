# R/_lib/io.R - Lightweight readers used across phases.
# Avoids duplicating arrow/duckdb boilerplate.

suppressPackageStartupMessages({
  library(arrow)
  library(fs)
})

read_parquet_safe <- function(path, what = "fichero") {
  if (!fs::file_exists(path)) {
    stop(sprintf("Falta %s en %s", what, path), call. = FALSE)
  }
  arrow::read_parquet(path)
}

write_csv_audit <- function(df, filename, dir = NULL) {
  if (is.null(dir)) {
    if (!exists("TABLE_DIR")) stop("TABLE_DIR no definido; carga _config.R.")
    dir <- TABLE_DIR
  }
  out <- fs::path(dir, filename)
  utils::write.csv(df, out, row.names = FALSE)
  invisible(out)
}

log_section <- function(title, level = 1) {
  bar <- if (level == 1) strrep("=", 70) else strrep("-", 60)
  message(bar)
  message(title)
  message(bar)
}
