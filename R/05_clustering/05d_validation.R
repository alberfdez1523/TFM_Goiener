#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05d_validation.R
#
# Indices internos + bootstrap Jaccard + ARI cross-algoritmo.
# Escribe outputs/tables/cluster_leaderboard.csv.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))
source(here::here("R", "_lib", "cluster_metrics.R"))

log_section("PASO 06d: Validacion (indices + Jaccard + ARI)")
t0 <- proc.time(); set.seed(SEED)

art <- readRDS(path(MODEL_DIR, "cluster_matrix.rds"))
models <- readRDS(path(MODEL_DIR, "cluster_models.rds"))
X_pca <- art$X_pca

# Sample for distance-based indices (avoid full O(n^2)).
sample_n <- min(4000, nrow(X_pca))
set.seed(SEED + 9001)
samp_idx <- sample(seq_len(nrow(X_pca)), sample_n)
d_samp <- dist(X_pca[samp_idx, , drop = FALSE])

rows <- list()
for (nm in names(models)) {
  m <- models[[nm]]
  lbl_full <- m$labels
  lbl_samp <- lbl_full[samp_idx]
  if (length(unique(lbl_samp[lbl_samp > 0])) < 2L) next
  idx <- internal_indices(lbl_samp[lbl_samp > 0], dist(X_pca[samp_idx[lbl_samp > 0], , drop = FALSE]))
  size <- cluster_sizes_summary(lbl_full[lbl_full > 0])
  rows[[nm]] <- data.frame(
    solution = nm, algo = m$algo, k = m$k,
    silhouette = idx$silhouette,
    calinski_harabasz = idx$calinski_harabasz,
    dunn = idx$dunn,
    n_clusters_eff = size$n_clusters,
    min_pct = size$min_pct,
    max_pct = size$max_pct,
    n_below_3pct = size$n_below_3pct,
    n_above_60pct = size$n_above_60pct,
    noise_pct = ifelse(is.null(m$noise_pct), 0, m$noise_pct),
    stringsAsFactors = FALSE
  )
}
ix_df <- bind_rows(rows)
message(sprintf("  Indices calculados para %d soluciones.", nrow(ix_df)))

# Score compuesto para preseleccion (sil normalizado + balance penalizado).
# El objetivo es promover soluciones con tamanos razonables sin imponer
# uniformidad artificial.
ix_df <- ix_df |>
  mutate(
    balance_penalty = pmax(0, (max_pct - 50) / 50),  # >50% empieza a penalizar
    composite_pre = silhouette - 0.3 * balance_penalty
  )

# Bootstrap Jaccard sobre top-15 segun composite_pre.
top <- ix_df |> arrange(desc(composite_pre)) |> head(15)

fit_factory <- function(algo, k) {
  if (algo == "kmeans_pca" || algo == "fpca_kmeans") {
    function(M) kmeans(M, centers = k, nstart = 10L, iter.max = 50L)$cluster
  } else if (algo == "pam_pca") {
    function(M) {
      if (nrow(M) > 5000) {
        cluster::clara(M, k = k, sampsize = 2000, samples = 5,
                       pamLike = TRUE, metric = "manhattan")$clustering
      } else {
        cluster::pam(M, k = k, metric = "manhattan")$clustering
      }
    }
  } else if (algo == "ward_pca") {
    function(M) cutree(hclust(dist(M), method = "ward.D2"), k = k)
  } else if (algo == "gmm") {
    function(M) tryCatch(
      as.integer(mclust::Mclust(M, G = k, modelNames = "VVI",
                                verbose = FALSE)$classification),
      error = function(e) rep(1L, nrow(M))
    )
  } else if (algo == "hdbscan") {
    function(M) {
      cl <- dbscan::hdbscan(M, minPts = CLUSTER_HDBSCAN_MINPTS[1])$cluster
      cl[cl == 0] <- max(cl) + 1L  # treat noise as own group for Jaccard
      cl
    }
  } else NULL
}

# Use a smaller boot sample for speed.
boot_n <- min(3000, nrow(X_pca))
set.seed(SEED + 7777)
boot_idx <- sample(seq_len(nrow(X_pca)), boot_n)
X_boot <- X_pca[boot_idx, , drop = FALSE]

message(sprintf("[Jaccard] Bootstrap (B=%d) sobre top-%d soluciones...",
                CLUSTER_BOOTSTRAP_B, nrow(top)))
jac_results <- vector("list", nrow(top))
for (i in seq_len(nrow(top))) {
  nm <- top$solution[i]
  m <- models[[nm]]
  ff <- fit_factory(m$algo, m$k)
  if (is.null(ff)) next
  lbl_boot <- m$labels[boot_idx]
  jac <- tryCatch(
    bootstrap_jaccard(X_boot, lbl_boot, ff, B = CLUSTER_BOOTSTRAP_B,
                      sample_fraction = 0.7, seed = SEED + i),
    error = function(e) NULL
  )
  jac_results[[i]] <- if (is.null(jac)) NA_real_ else jac$overall_mean
  message(sprintf("  %s: mean Jaccard = %.3f", nm,
                  ifelse(is.null(jac), NA_real_, jac$overall_mean)))
}
top$mean_jaccard <- unlist(jac_results)

# Merge stability into leaderboard.
ix_df <- ix_df |>
  left_join(top |> select(solution, mean_jaccard), by = "solution")

# Decision flags.
ix_df <- ix_df |>
  mutate(
    passes_silhouette = !is.na(silhouette) & silhouette >= CLUSTER_MIN_SILHOUETTE,
    passes_jaccard    = !is.na(mean_jaccard) & mean_jaccard >= CLUSTER_MIN_JACCARD,
    passes_min_pct    = !is.na(min_pct) & min_pct >= CLUSTER_MIN_PCT_V2,
    passes_max_pct    = !is.na(max_pct) & max_pct <= CLUSTER_MAX_PCT_V2,
    passes_all = passes_silhouette & passes_jaccard & passes_min_pct & passes_max_pct,
    # Composite score final: combina calidad geometrica, estabilidad y balance.
    # Normalizado para que cada componente aporte ~1/3 del valor.
    composite_score = round(
      0.45 * pmin(silhouette / 0.4, 1) +
      0.35 * pmin(mean_jaccard / 0.9, 1) +
      0.20 * (1 - balance_penalty),
      3
    )
  ) |>
  arrange(desc(passes_all), desc(composite_score), desc(silhouette))

write_csv_audit(ix_df, "cluster_leaderboard.csv")
print(head(ix_df, 10))

message(sprintf("06d en %.1f s", (proc.time() - t0)[["elapsed"]]))
