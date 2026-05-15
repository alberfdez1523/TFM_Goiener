# R/_lib/cluster_metrics.R - Internal validity and stability metrics.
# Compact, dependency-light implementations used by R/05_clustering/.

suppressPackageStartupMessages({
  library(cluster)
  library(fpc)
})

internal_indices <- function(labels, dist_mat) {
  labels <- as.integer(as.factor(labels))
  if (length(unique(labels)) < 2L) {
    return(list(silhouette = NA_real_, calinski_harabasz = NA_real_,
                davies_bouldin = NA_real_, dunn = NA_real_))
  }
  stats <- fpc::cluster.stats(dist_mat, labels, silhouette = TRUE,
                              G2 = FALSE, G3 = FALSE, sepwithnoise = FALSE,
                              compareonly = FALSE)
  list(
    silhouette = unname(stats$avg.silwidth),
    calinski_harabasz = unname(stats$ch),
    davies_bouldin = NA_real_,  # fpc no lo expone; opcional via clusterCrit
    dunn = unname(stats$dunn)
  )
}

cluster_share_table <- function(labels) {
  tab <- table(labels)
  data.frame(
    cluster = as.character(names(tab)),
    n = as.integer(tab),
    pct = round(100 * as.numeric(tab) / sum(tab), 2),
    stringsAsFactors = FALSE
  )
}

cluster_sizes_summary <- function(labels) {
  share <- cluster_share_table(labels)
  list(
    n_clusters = nrow(share),
    min_pct = min(share$pct),
    max_pct = max(share$pct),
    n_below_3pct = sum(share$pct < 3),
    n_above_60pct = sum(share$pct > 60)
  )
}

# Bootstrap Jaccard stability for an algorithm specified by fit_fn().
# fit_fn(X) must return a vector of cluster labels of length nrow(X).
# Returns vector of mean Jaccard per cluster of the reference solution.
bootstrap_jaccard <- function(X, reference_labels, fit_fn, B = 50,
                              sample_fraction = 0.8, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ref_clusters <- sort(unique(reference_labels))
  n <- nrow(X)
  scores <- matrix(NA_real_, nrow = B, ncol = length(ref_clusters),
                   dimnames = list(NULL, as.character(ref_clusters)))
  for (b in seq_len(B)) {
    idx <- sample.int(n, size = floor(n * sample_fraction), replace = FALSE)
    new_labels <- tryCatch(fit_fn(X[idx, , drop = FALSE]),
                           error = function(e) NULL)
    if (is.null(new_labels)) next
    ref_sub <- reference_labels[idx]
    for (g in seq_along(ref_clusters)) {
      ref_idx <- which(ref_sub == ref_clusters[g])
      if (length(ref_idx) < 2) next
      # best matching cluster in new partition
      candidates <- unique(new_labels[ref_idx])
      best <- 0
      for (c in candidates) {
        c_idx <- which(new_labels == c)
        inter <- length(intersect(ref_idx, c_idx))
        uni <- length(union(ref_idx, c_idx))
        if (uni > 0) best <- max(best, inter / uni)
      }
      scores[b, g] <- best
    }
  }
  list(
    per_cluster_mean = colMeans(scores, na.rm = TRUE),
    overall_mean = mean(scores, na.rm = TRUE),
    raw = scores
  )
}

adjusted_rand_index <- function(a, b) {
  if (length(a) != length(b)) stop("a and b must have same length")
  tab <- table(a, b)
  n <- sum(tab)
  sum_n_ij <- sum(choose(tab, 2))
  sum_a <- sum(choose(rowSums(tab), 2))
  sum_b <- sum(choose(colSums(tab), 2))
  expected <- sum_a * sum_b / choose(n, 2)
  max_term <- 0.5 * (sum_a + sum_b)
  if (max_term - expected == 0) return(NA_real_)
  (sum_n_ij - expected) / (max_term - expected)
}
