# R/_lib/plots.R - Theming and palette helpers (moved from _config.R).
# Sourced by scripts that need theme_goiener(). _config.R still defines the
# palette constants so legacy code keeps working.

suppressPackageStartupMessages(library(ggplot2))

if (!exists("PAL_MAIN")) PAL_MAIN <- "#2C6E91"
if (!exists("PAL_ACCENT")) PAL_ACCENT <- "#E8734A"
if (!exists("PAL_FILL")) PAL_FILL <- "#B8D8E8"

save_fig <- function(plot, filename, width = 8, height = 5, dpi = 110) {
  if (!exists("FIG_DIR")) stop("FIG_DIR no definido; carga _config.R antes.")
  out <- fs::path(FIG_DIR, filename)
  ggplot2::ggsave(out, plot, width = width, height = height, dpi = dpi,
                  bg = "white")
  invisible(out)
}
