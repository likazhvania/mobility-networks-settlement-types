# =============================================================================
# Development script - not part of the final analysis
#
# Feature engineering and k-means clustering of users into mobility-network
# profiles, for an earlier formulation of the second research question.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Libraries
library(data.table)
library(tidyverse)
library(scales)
library(cluster)

set.seed(params$seed)

# Data input & variable mapping ----------------------------------------------

# --- Variable name mapping ---
varmap <- list(

  # --- Mobility (TOTAL) ---
  rog_total      = "sm_rog_total",               # radius of gyration (total), meters or km
  nloc_total     = "sm_nul_total",               # number of unique locations (total)
  entr_total     = "sm_entr_total",              # spatial entropy (total)
  moves_avg      = "sm_avg_daily_moves",         # average daily moves
  active_days    = "sm_active_days_total",       # active days during period

  # --- Workday vs Weekend versions (averages / for rhythm ratios) ---
  nloc_work      = "sm_avg_workday_nul",
  nloc_week      = "sm_avg_weekend_nul",
  entr_work      = "sm_avg_workday_entr",
  entr_week      = "sm_avg_weekend_entr",
  moves_work     = "sm_avg_workday_moves",
  moves_week     = "sm_avg_weekend_moves",
  rog_work       = "sm_avg_workday_rog",
  rog_week       = "sm_avg_weekend_rog",

  # --- Pre-computed ratios (Workday vs Weekend) ---
  rog_ratio      = "sm_rog_ratio",
  nloc_ratio     = "sm_nul_ratio",
  entr_ratio     = "sm_entr_ratio",
  moves_ratio    = "sm_moves_ratio",

  # --- Contextual composition (shares should sum ~1) ---
  pct_big        = "cg_pct_big_city",
  pct_small      = "cg_pct_small_city",
  pct_rural      = "cg_pct_rural",

  # --- Additional context diversity ---
  hier_div       = "cg_geodiv_hierarchy",        # integer in {1,2,3}
  muni_count     = "cg_geodiv_municipality"      # count of visited municipalities (if absent, set to NA and skip)
)

df <- analysis_data

present <- names(df)
missing_core <- setdiff(unlist(varmap[c("rog_total", "nloc_total", "entr_total", "moves_avg", "active_days",
                                        "pct_big", "pct_small", "pct_rural", "hier_div")]), present)

if (length(missing_core)) {
  warning("Missing required variables: ", paste(missing_core, collapse = ", "))
}

# Rhythm variables optional
rhythm_vars <- unlist(varmap[c("nloc_work", "nloc_week", "entr_work", "entr_week", "moves_work", "moves_week", "rog_work", "rog_week")])
missing_rhythm <- setdiff(rhythm_vars, present)

if (length(missing_rhythm) == length(rhythm_vars)) {
  message("No workday/weekend variables found: rhythm features will be omitted.")
}

# Optional municipality cound
if (!varmap$muni_count %in% present) varmap$muni_count <- NA_character_

# Feature engineering (no extra packages) ------------------------------------

DT <- as.data.table(df)

# Helper: winsorize upper tail for positives
winsor_top <- function(x, p = params$winsor_top) {
  stopifnot(is.numeric(x))
  q <- stats::quantile(x, probs = 1 - p, na.rm = TRUE)
  x <- ifelse(x > q, q, x)
  x
}

# 2.1 Mobility base features
DT[, `:=`(
  extent_log = scale(winsor_top(log1p(get(varmap$rog_total)), p = params$winsor_top)),
  diversity_z = as.numeric(scale(get(varmap$entr_total))),
  freq_log = scale(winsor_top(log1p(get(varmap$moves_avg)), p = params$winsor_top)),
  engage_days = get(varmap$active_days)
)]

# 2.2 Weekday-Weekend rhythm ratios (prefer provided ratios; fall back to compute from averages)
# extent (RoG)
if (varmap$rog_ratio %in% names(DT)) {
  DT[, rhythm_extent := log2(get(varmap$rog_ratio) + 1e-9)]
} else if (all(c(varmap$rog_work, varmap$rog_week) %in% names(DT))) {
  DT[, rhythm_extent := log2( (get(varmap$rog_work) + 1e-6) / (get(varmap$rog_week) + 1e-6) )]
} else {
  DT[, rhythm_extent := NA_real_]
}

# diversity (combine NUL & entropy for stability)
if (all(c(varmap$nloc_ratio, varmap$entr_ratio) %in% names(DT))) {
  DT[, rhythm_div := scale(log2(get(varmap$nloc_ratio) + 1e-9)) + 
       scale(log2(get(varmap$entr_ratio) + 1e-9))]
  DT[, rhythm_div := as.numeric(rhythm_div)/2]
} else if (all(c(varmap$nloc_work, varmap$nloc_week, varmap$entr_work, varmap$entr_week) %in% names(DT))) {
  DT[, rhythm_div := log2( (as.numeric(scale(get(varmap$nloc_work))) + as.numeric(scale(get(varmap$entr_work))))  / 
                             (as.numeric(scale(get(varmap$nloc_week))) + as.numeric(scale(get(varmap$entr_week)))) )]
} else {
  DT[, rhythm_div := NA_real_]
}

# frequency
if (varmap$moves_ratio %in% names(DT)) {
  DT[, rhythm_freq := log2(get(varmap$moves_ratio) + 1e-9)]
} else if (all(c(varmap$moves_work, varmap$moves_week) %in% names(DT))) {
  DT[, rhythm_freq := log2( (get(varmap$moves_work) + 1e-6) / (get(varmap$moves_week) + 1e-6) )]
} else {
  DT[, rhythm_freq := NA_real_]
}

# 2.3 Context composition (custom ILR without 'compositions'). (custom ILR without 'compositions') ensure non-negative, bounded, and re-normalised shares.
ilr3 <- function(M) {
  # M: n x 3 matrix of shares (rows sum to 1)
  eps <- 1e-12
  M[M < eps] <- eps
  rs <- rowSums(M)
  M <- M / rs
  L <- log(M)
  V <- matrix(c(
    sqrt(2/3), -1/sqrt(6), -1/sqrt(6),
    0,          1/sqrt(2), -1/sqrt(2)
  ), nrow = 3, byrow = TRUE)  # 3x2 basis
  Z <- L %*% V                # n x 2
  colnames(Z) <- c("ilr_1","ilr_2")
  Z
}

ilr3_inv <- function(Z) {
  # Z: n x 2 matrix of ilr coords
  V <- matrix(c(
    sqrt(2/3), -1/sqrt(6), -1/sqrt(6),
    0,          1/sqrt(2), -1/sqrt(2)
  ), nrow = 3, byrow = TRUE)  # 3x2
  Lhat <- Z %*% t(V)          # n x 3 (log parts up to a constant)
  X <- exp(Lhat)
  X / rowSums(X)
}

shares <- as.matrix(DT[, .(get(varmap$pct_big), get(varmap$pct_small), get(varmap$pct_rural))])
colnames(shares) <- c("big", "small", "rural")
shares[is.na(shares)] <- 0
rs <- rowSums(shares)
shares[rs == 0, ] <- c(1/3, 1/3, 1/3)  # fallback it all zero
shares <- shares / rowSums(shares)
Z <- ilr3(shares)
DT[, `:=`(ilr_1 = Z[,1], ilr_2 = Z[,2])]

# Hierarchy diversity (1-3) and municipality count (log1p) if present
DT[, hier_div := get(varmap$hier_div)]
if (!is.na(varmap$muni_count)) {
  DT[, muni_div_log := log1p(get(varmap$muni_count))]
} else {
  DT[, muni_div_log := NA_real_]
}

# 2.4 Assemble feature matrix (drop NAs)
feature_cols <- c("extent_log", "diversity_z", "freq_log", "engage_days",
                  "rhythm_div", "rhythm_freq", "rhythm_extent",
                  "ilr_1", "ilr_2", "hier_div", "muni_div_log")
X <- DT[, ..feature_cols]

# keep only columns with *zero* NAs
keep_cols <- names(X)[colSums(is.na(X)) == 0]

if (length(keep_cols) == 0) stop("No feature column without NAs; cannot proceed.")
if (length(keep_cols) < ncol(X)) {
  message(sprintf("Dropping %d feature(s) with any NA: %s",
                  ncol(X) - length(keep_cols),
                  paste(setdiff(names(X), keep_cols), collapse = ", ")))
}

X <- X[, ..keep_cols]

id <- if (params$id_col %in% names(DT)) DT[[params$id_col]] else seq_len(nrow(DT))

ok <- stats::complete.cases(X)
X <- X[ok, ]
DT <- DT[ok]
id <- id[ok]

# Standardisation & clustering (base k-means; manual CH/DB) ------------------

X_scaled <- scale(X)

# Between/within utilities for indices
wss_total <- function(X, cl, centers){
  sum(vapply(unique(cl), function(k){
    rows <- which(cl == k)
    if (length(rows) == 0) return(0)
    rowSums((X[rows,,drop = FALSE] - matrix(centers[k,], nrow = length(rows), ncol = ncol(X), byrow = TRUE))^2)
  }, numeric(1)))
}

ch_index <- function(X, cl){
  n <- nrow(X); p <- ncol(X); k <- length(unique(cl))
  centers <- rowsum(X, cl) / as.vector(table(cl))
  overall <- matrix(colMeans(X), nrow = k, ncol = p, byrow = TRUE)
  B <- sum(as.vector(table(cl)) * rowSums((centers - overall)^2))
  W <- sum(vapply(seq_len(k), function(i){
    rows <- which(cl == sort(unique(cl))[i])
    if (!length(rows)) return(0)
    sum(rowSums((X[rows,,drop = FALSE] - matrix(centers[i,], nrow = length(rows), ncol = p, byrow = TRUE))^2))
  }, numeric(1)))
  (B/(k-1)) / (W/(n-k))
}

# Davies-Bouldin
db_index <- function(X, cl){
  labs <- sort(unique(cl)); k <- length(labs)
  centers <- rowsum(X, cl) / as.vector(table(cl))
  # s_i: average distance to centroid
  s <- vapply(seq_len(k), function(i){
    rows <- which(cl == labs[i])
    if (length(rows) == 0) return(0)
    mean(sqrt(rowSums((X[rows,,drop = FALSE] - matrix(centers[i,], nrow = length(rows), ncol = ncol(X), byrow = TRUE))^2)))
  }, numeric(1))
  # centroid distances
  D <- as.matrix(dist(centers))
  D[D == 0] <- Inf  
  R <- outer(seq_len(k), seq_len(k), Vectorize(function(i,j){ if (i==j) return(NA_real_); (s[i] + s[j]) / D[i,j] }))
  Ri <- apply(R, 1, function(x) max(x, na.rm = TRUE))
  mean(Ri)
}

ks <- params$k_min:params$k_max
crit <- lapply(ks, function(k){
  km <- stats::kmeans(X_scaled, centers = k, nstart = 50, iter.max = 100)   # nstart = 10, (nstart = 50 doesn't change the method, just reduces random variable)
  ch <- ch_index(X_scaled, km$cluster)
  db <- db_index(X_scaled, km$cluster)
  list(k=k, ch=ch, db=db, model = km)
})
crit_dt <- data.table(k = sapply(crit, `[[`, "k"),
                      ch = sapply(crit, `[[`, "ch"),
                      db = sapply(crit, `[[`, "db"))

best_k_ch <- crit_dt$k[which.max(crit_dt$ch)]
best_k_db <- crit_dt$k[which.min(crit_dt$db)]
K <- best_k_ch
if (length(unique(round(crit_dt$ch,5)))==1) K <- best_k_db

km_fit <- crit[[which(sapply(crit, function(x) x$k)==K)]]$model
clusters <- km_fit$cluster

message(sprintf("Selected K = %d (CH max), CH=%.1f, DB=%.3f", K,
                crit_dt$ch[crit_dt$k==K], crit_dt$db[crit_dt$k==K]))

# Optional silhouette on a subsample

# Profile summaries & visualisation ------------------------------------------

DT[, cluster := factor(clusters)]

# 4.1 Cluster sizes
sizes <- DT[, .N, by=cluster][order(cluster)]
sizes

# 4.2 Cluster centroids (standardised units)
# centroids <- as.data.table(km_fit$centers)
# centroids[, feature := colnames(km_fit$centers)]
# centroids_long <-melt(centroids, id.vars = "feature", variable.name = "cluster", value.name = "z")
# centroids_long[, cluster := factor(cluster, levels = levels(DT$cluster))]

centroids_t <- data.table::as.data.table(t(km_fit$centers))
centroids_t[, feature := colnames(km_fit$centers)]

centroids_long <- data.table::melt(
  centroids_t,
  id.vars        = "feature",
  variable.name  = "cluster",
  value.name     = "z"
)

plt_centroids <- ggplot2::ggplot(centroids_long, ggplot2::aes(x = feature, y = z, fill = cluster)) +
  ggplot2::geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  ggplot2::coord_flip() +
  ggplot2::labs(title = sprintf("Cluster centroids (K=%d), standardized features", K),
                x = NULL, y = "Standardized value") +
  ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")

plt_centroids

# Context mix per cluster (simple means of the original shares)
comp_long <- DT[, .(
  Big_city   = mean(cg_avg_pct_big_city,   na.rm=TRUE),
  Small_city = mean(cg_avg_pct_small_city, na.rm=TRUE),
  Rural      = mean(cg_avg_pct_rural,      na.rm=TRUE)
), by = cluster]

comp_long <- data.table::melt(
  comp_long, 
  id.vars = "cluster",
  variable.name = "context",
  value.name = "share"
)

plt_ctx <- ggplot2::ggplot(comp_long, 
                           ggplot2::aes(x = cluster, y = share, fill = context)) +
  ggplot2::geom_col(position = "fill") +
  ggplot2::geom_text(aes(label = round(share, 2)), position = "fill") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(title = "Average activity composition by cluster", 
                x = "Cluster", y = "Share") +
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "bottom")

plt_ctx

# Network differences across profiles (base stats) ---------------------------

# make sure cluster exists
DT[, cluster := factor(cluster)]

# SN columns that are present
sn_vars <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness", "sn_closeness",
             "sn_clustering", "sn_reciprocity")

exists_sn <- intersect(sn_vars, names(DT))
if (!length(exists_sn)) stop("No social-network metrics found in DT.")

# Long format for tests/plots (CORRECT)
sn_long <- data.table::melt(
  DT,
  id.vars           = "cluster",
  measure.vars      = exists_sn,     # <- pas the vector, not the name of the vector
  variable.name     = "metric",
  value.name        = "value",
  variable.factor   = FALSE
)

# Kruskal-Wallis per metric + eta^2_H effect size
kw_list <- lapply(split(sn_long, sn_long$metric), function(dd){
  dd <- dd[is.finite(value)]
  kt <- stats::kruskal.test(value ~ cluster, data = dd)
  k <- length(unique(dd$cluster))
  n <- nrow(dd)
  eta2H <- (as.numeric(kt$statistic) - k + 1) / (n - k)
  data.table(metric = unique(dd$metric), H = as.numeric(kt$statistic),
             df = as.numeric(kt$parameter), p = kt$p.value, eta2H = max(0, eta2H))
})

kw_res <- rbindlist(kw_list)[order(-eta2H)]
kw_res

# Pairwise Wilcoxon (BH-adjusted)
pairwise_list <- lapply(split(sn_long, sn_long$metric), function(dd){
  dd <- dd[is.finite(value)]
  pw <- pairwise.wilcox.test(dd$value, dd$cluster, p.adjust.method = "BH")
  # convert to long table
  tabs <- as.data.frame(as.table(pw$p.value))
  setDT(tabs)
  setnames(tabs, c("group1", "group2", "p"))
  tabs[, metric := unique(dd$metric)]
  tabs
})

pairwise <- rbindlist(pairwise_list, fill = TRUE)
head(pairwise)

# Simple boxplot for a few key SN metrics (optional)
sel <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness", "sn_closeness", "sn_clustering")
sel <- sel[sel %in% exists_sn]

if (length(sel)) {
  plt_sn <- ggplot(sn_long[metric %in% sel], aes(x = cluster, y = value, fill = cluster)) + 
    geom_boxplot(outlier.alpha = 0.1) +
    facet_wrap(~metric, scales = "free_y") +
    scale_y_log10()+
    labs(title = "Social-network metrics by mobility profiles (log (10))", x = NULL, y = NULL) +
    theme_minimal() + theme(legend.position = "none")

  plt_sn
}

# palette
prof_cols <- c("1" = "#5A97C1",
               "2" = "#74C8C3",
               "3" = "#67322E",
               "4" = "#99610A")

# palette
prof_cols <- c("1" = "#295384",
               "2" = "#74C8C3",
               "3" = "#6E948C",
               "4" = "#67322E")

# palette
prof_cols <- c("1" = "#295384",
               "2" = "#99610A",
               "3" = "#6E948C",
               "4" = "#67322E")

sn_long$metric[sn_long$metri=="sn_totaldegree"] <- "Total-degree"
sn_long$metric[sn_long$metri=="sn_indegree"] <- "In-degree"
sn_long$metric[sn_long$metri=="sn_outdegree"] <- "Out-degree"
sn_long$metric[sn_long$metri=="sn_betweenness"] <- "Betweenness Centrality"
sn_long$metric[sn_long$metri=="sn_closeness"] <- "Closeness Centrality"
sn_long$metric[sn_long$metri=="sn_clustering"] <- "Clustering Coefficient"
sn_long$metric[sn_long$metri=="sn_reciprocity"] <- "Reciprocity Index"

sel <- c("Total-degree", "In-degree", "Out-degree", "Betweenness Centrality", "Closeness Centrality", 
         "Clustering Coefficient")

sn_long$metric <- factor(sn_long$metric, 
                         levels = c("Total-degree", "In-degree", "Out-degree", "Betweenness Centrality",                                              "Closeness Centrality", "Clustering Coefficient"))

if (length(sel)) {
  plt_sn <- ggplot(sn_long[metric %in% sel], aes(x = cluster, y = value, fill = cluster)) + 
    geom_boxplot(data = sn_long[metric %in% sel], 

                 colour = "#f0f0f0",             # colour for borders

                 outlier.alpha = 0.3,
                 outlier.size = 0.4,             # size of outlier points
                 outlier.stroke = 0.05,          # stroke width of outlier points
                 outlier.colour = "#969696",     # colour of outlier point strokes
                 #outlier.fill = "#969696",      # colour of outlier point
                 fatten = 0.4,                  # middle line width size

                 notch = FALSE
    ) +

    scale_fill_manual(values = prof_cols) +

    scale_x_discrete(labels = c("Profile 1", "Profile 2", "Profile 3", "Profile 4")) + 
    scale_y_log10()+

    facet_wrap(~metric, scales = "free_y") +
    labs( x = NULL, y = NULL) +   # title = "Social-network metrics by mobility profiles",

    theme_minimal(base_family = "sans") +

    theme(

      #axis.title = element_blank(),
      axis.text.x = element_text(size = 7, colour = "#252525", face =  "italic"),
      axis.text.y = element_text(size = 6, colour = "#252525"),  

      #strip.background = element_rect(fill = "#d9d9d9"),
      strip.text =  element_text(face =  "bold", size = 9, colour = "#252525"),

      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),

      legend.position = "none",

      plot.background = element_rect(fill = "#F9F9EB", colour = NA),

      plot.margin        = margin(6, 8, 8, 10),  # mm around the figure

      panel.spacing.x = unit(4, "mm"),
      panel.spacing.y = unit(2, "mm")
    )

  plt_sn
}

# TIFF @ 1000 dpi
ggsave("figs/plt_sn.tiff", plt_sn, device = "tiff",
       width = 183, height = 130, units = "mm",
       dpi = 1000, compression = "lzw")

# Export table ---------------------------------------------------------------

# fwrite(DT[, .(id = if (params$id_col %in% names(DT)) get(params$id_col) else .I,
# cluster = cluster)], "data/cluster_membership.csv")

# centroids_wide <- as.data.table(km_fit$centers)
# centroids_wide[, feature := colnames(km_fit$centers)]
# setcolorder(centroids_wide, c("feature", setdiff(names(centroids_wide), "feature")))
# fwrite(centroids_wide, "data/cluster_centroids_z.csv")

# fwrite(kw_res, "data/sn_kw_results.csv")
# fwrite(pairwise, "data/sn_pairwise_wilcox.csv")

# sizes <- DT[, .N, by=cluster][order(cluster)]
# fwrite(sizes, "data/cluster_sizes.csv")
