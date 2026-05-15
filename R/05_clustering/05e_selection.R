#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05e_selection.R
#
# Aplica la regla de seleccion sobre el leaderboard. Si ninguna candidata pasa
# todos los umbrales, registra el motivo y elige la mejor por silhouette
# entre las que cumplen Jaccard y tamano.
# Escribe outputs/tables/cluster_selection_decision.csv y guarda etiquetas
# en data/parquet/features/user_clusters_v2.parquet.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 06e: Seleccion final")
t0 <- proc.time(); set.seed(SEED)

leaderboard <- read.csv(path(TABLE_DIR, "cluster_leaderboard.csv"),
                        stringsAsFactors = FALSE)
art <- readRDS(path(MODEL_DIR, "cluster_matrix.rds"))
models <- readRDS(path(MODEL_DIR, "cluster_models.rds"))

passes <- leaderboard |> filter(passes_all)

if (nrow(passes) > 0) {
  chosen <- passes |> arrange(desc(composite_score), desc(silhouette)) |> slice(1)
  rationale <- "Cumple todos los umbrales; mejor composite_score (silhouette + Jaccard + balance)."
} else {
  # Fallback: mejor composite_score entre las que cumplen estabilidad y tamano max.
  fb <- leaderboard |>
    filter(passes_jaccard | is.na(passes_jaccard)) |>
    filter(passes_max_pct) |>
    arrange(desc(composite_score))
  chosen <- fb |> slice(1)
  if (nrow(chosen) == 0) {
    chosen <- leaderboard |> arrange(desc(composite_score)) |> slice(1)
  }
  rationale <- "Ninguna candidata cumple todos los umbrales; elegida la mejor en composite_score respetando estabilidad y tamano maximo."
}

decision <- chosen |>
  mutate(rationale = rationale)
write_csv_audit(decision, "cluster_selection_decision.csv")

message("Decision:")
print(decision)

# Persist labels for the chosen solution.
solution_name <- decision$solution[1]
labels <- models[[solution_name]]$labels

clusters_df <- tibble::tibble(
  user_id = art$user_id,
  cluster = labels,
  solution = solution_name,
  algo = decision$algo[1],
  k = decision$k[1]
)

# Anade el segmento descriptivo "no_habitual" si existe.
nh_path <- path(FEATURES_DIR, "cluster_no_habitual.parquet")
if (file_exists(nh_path)) {
  nh <- arrow::read_parquet(nh_path)
  if (nrow(nh) > 0) {
    nh_df <- tibble::tibble(
      user_id  = nh$user_id,
      cluster  = -1L,
      solution = "no_habitual",
      algo     = "rule",
      k        = decision$k[1]
    )
    clusters_df <- dplyr::bind_rows(clusters_df, nh_df)
    message(sprintf("  Anadidos %s usuarios no_habitual (cluster=-1).",
                    fmt_int(nrow(nh_df))))
  }
}

arrow::write_parquet(clusters_df, USER_CLUSTERS_V2_PARQUET)
message(sprintf("Etiquetas guardadas: %s (%s usuarios totales)",
                USER_CLUSTERS_V2_PARQUET, fmt_int(nrow(clusters_df))))
message(sprintf("06e en %.1f s", (proc.time() - t0)[["elapsed"]]))
