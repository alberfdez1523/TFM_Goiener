# R/_lib/report_helpers.R - Common helpers for Quarto chunks.
# Mirrors what _config.R exposes but kept thin to source from qmd.

if (!exists("fmt_num")) {
  fmt_num <- function(x, digits = 2) {
    formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  }
}
if (!exists("fmt_int")) {
  fmt_int <- function(x) {
    format(round(x), big.mark = ".", decimal.mark = ",",
           scientific = FALSE, trim = TRUE)
  }
}
if (!exists("fmt_pct")) {
  fmt_pct <- function(x, digits = 1) {
    paste0(fmt_num(100 * x, digits = digits), "%")
  }
}

# Standardised section header for qmd subarchives.
section_header <- function(qmd_id, title) {
  cat(sprintf("\n### %s. %s\n", qmd_id, title))
}

# Inline narrative block used everywhere in new qmd. Keeps prose discipline.
narrative <- function(what, why, reading, implication) {
  cat("\n**Qué se muestra.** ", what, "\n", sep = "")
  cat("\n**Por qué.** ", why, "\n", sep = "")
  cat("\n**Lectura.** ", reading, "\n", sep = "")
  cat("\n**Implicación operativa.** ", implication, "\n", sep = "")
}
