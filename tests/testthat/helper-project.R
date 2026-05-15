suppressPackageStartupMessages({
  library(testthat)
  library(fs)
})

source(here::here("_config.R"))

read_csv_if_exists <- function(path) {
  skip_if_not(fs::file_exists(path), paste("Falta output:", path))
  read.csv(path, stringsAsFactors = FALSE)
}

