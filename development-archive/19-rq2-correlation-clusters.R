# =============================================================================
# Development script - not part of the final analysis
#
# Correlations between measures within the user clusters (profiles) of the
# earlier profiling design.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Libraries
library(data.table)
library(tidyverse)

# Correlation - All Users ----------------------------------------------------

# Select variables
rq2_data_total <- DT %>%
  select('user_id',
         'sn_indegree', 'sn_outdegree', 'sn_totaldegree', 'sn_betweenness', 'sn_closeness', 'sn_clustering', 'sn_reciprocity',
         'sm_nul_total', 'sm_active_days_total', 'sm_entr_total', 'sm_rog_total',
         'cluster')

rq2_data_total <- rq2_data_total %>%
  rename("SN_indeg" = "sn_indegree", 
         "SN_outdeg" = "sn_outdegree", 
         "SN_totaldeg" = "sn_totaldegree", 
         "SN_betw" = "sn_betweenness", 
         "SN_clos" = "sn_closeness", 
         "SN_clust" = "sn_clustering", 
         "SN_recip" = "sn_reciprocity",
         "SM_NUL" = "sm_nul_total", 
         "SM_actdays" = "sm_active_days_total", 
         "SM_entr" = "sm_entr_total", 
         "SM_RoG" = "sm_rog_total")

# --- 1. Correlation ---

# Subdatasets by clusters
cluster_1 <- rq2_data_total %>%
  filter(cluster == 1)

cluster_2 <- rq2_data_total %>%
  filter(cluster == 2)

cluster_3 <- rq2_data_total %>%
  filter(cluster == 3)

cluster_4 <- rq2_data_total %>%
  filter(cluster == 4)

# Dominant Settlement: Cluster 1
num_vars_cluster1 <- names(which(vapply(cluster_1, is.numeric, logical(1))))
num_vars_cluster1 <- setdiff(num_vars_cluster1, "user_id")   # just in case it's numeric

cluster1_cor <- cor(cluster_1[, ..num_vars_cluster1],
                    use = "pairwise.complete.obs",
                    method = "spearman")

# Dominant Settlement: Cluster 2
num_vars_cluster2 <- names(which(vapply(cluster_2, is.numeric, logical(1))))
num_vars_cluster2 <- setdiff(num_vars_cluster2, "user_id")   # just in case it's numeric

cluster2_cor <- cor(cluster_2[, ..num_vars_cluster2],
                    use = "pairwise.complete.obs",
                    method = "spearman")

# Dominant Settlement: Cluster 3
num_vars_cluster3 <- names(which(vapply(cluster_3, is.numeric, logical(1))))
num_vars_cluster3 <- setdiff(num_vars_cluster3, "user_id")   # just in case it's numeric

cluster3_cor <- cor(cluster_3[, ..num_vars_cluster3],
                    use = "pairwise.complete.obs",
                    method = "spearman")

# Dominant Settlement: Cluster 4
num_vars_cluster4 <- names(which(vapply(cluster_4, is.numeric, logical(1))))
num_vars_cluster4 <- setdiff(num_vars_cluster4, "user_id")   # just in case it's numeric

cluster4_cor <- cor(cluster_4[, ..num_vars_cluster4],
                    use = "pairwise.complete.obs",
                    method = "spearman")

# Optional: save to disk
# fwrite(as.data.table(as.table(clusters_cor_total)), "correlation_all.csv")

# clusters_cor_total now holds a square matrix (#vars x #vars) for the whole
# dataset.

# Calculate significane P values ---------------------------------------------

  # --- helper: Significance ---
  # p-value matrix for Spearman
  p_mat <- function(df_num) {

    # keep only numeric; coerce integer64 to double
    df <- df_num %>%
      mutate(across(
        everything(),
        ~ if (inherits(.x, "integer64")) {as.double(.x)} else .x
      )) %>%
      select(where(is.numeric))

    v <- colnames(df); m <- length(v)
    P <- matrix(NA_real_, m, m, dimnames = list(v, v))
    if (m < 2) return(P)

    for (i in 2:m) for (j in 1:(i-1)) {
      x <- df[[i]]; y <- df[[j]]
      ok <- is.finite(x) & is.finite(y)     # pairwise NA removal
      if (sum(ok) > 2) {
        P[i, j] <- P[j, i] <- suppressWarnings(
          stats::cor.test(x[ok], y[ok], method = "spearman",exact = FALSE)$p.value
        )
      }
    }
    diag(P) <- 0
    P
  }

  # turn p-matrix into long
  p_long <- function(P, group) {
    vars <- colnames(P)
    idx <- lower.tri(P)    # logical matrix: TRUE for lower triangle
    data.frame(
      Var1 = factor(vars[row(P)][idx], levels = vars),
      Var2 = factor(vars[col(P)][idx], levels = vars),
      p    = as.numeric(P[idx]),
      grp  = group,
      row.names = NULL
    )
  }

  # build once for each group
  df_p <- bind_rows(
    p_long(p_mat(cluster_1),   "Profile 1"),
    p_long(p_mat(cluster_2),   "Profile 2"),
    p_long(p_mat(cluster_3),   "Profile 3"),
    p_long(p_mat(cluster_4),   "Profile 4")
  )

  # --- helper: matrix -> long + keep only one triangle, same var order everywhere ---
  cor_long <- function(mat, group) {
    vars <- colnames(mat)
    as.data.frame(as.table(mat), stringsAsFactors = FALSE) %>%
      rename(Var1 = Var1, Var2 = Var2, cor = Freq) %>%
      mutate(
        Var1 = factor(Var1, levels = vars),
        Var2 = factor(Var2, levels = vars),
        grp = group
      ) %>%
      # keep lower triangle only (drop diagonal)
      filter(as.integer(Var1) > as.integer(Var2))
  }

  # build one big tidy table
  df_cor <- bind_rows(
    cor_long(cluster1_cor,    "Profile 1"),
    cor_long(cluster2_cor,    "Profile 2"),
    cor_long(cluster3_cor,    "Profile 3"),
    cor_long(cluster4_cor,    "Profile 4")
  )

  # labels
  #df_lab <- df_cor %>% mutate(lab = sprintf("%.2f", cor))
  df_lab <- df_cor %>% mutate(lab = ifelse(abs(cor) < 5e-3, "<0.01", sprintf("%.2f", cor)))

  # show only for |r| >= 0.30 for readability
  #df_lab <- df_cor %>% mutate(lab = ifelse(abs(cor) >= 0.30, sprintf("%.2f", cor), ""))

  # Join correlation dataset with dataset of p_values
  df_cor <- df_cor %>%
    left_join(df_p, by = c("Var1", "Var2", "grp")) %>%
    group_by(grp) %>%
    # significance marks (stars)
    mutate(p_adj = p.adjust(p, method = "BH"),
           star = case_when(
             p_adj < 0.001      ~ "***",
             p_adj < 0.01       ~ "**",
             p_adj < 0.05       ~ "*",
             TRUE               ~ ""
           )) %>%
    ungroup()

  df_p$grp   <- factor(df_p$grp,   levels = c("Profile 1", "Profile 2", "Profile 3", "Profile 4"))
  df_cor$grp <- factor(df_cor$grp, levels = c("Profile 1", "Profile 2", "Profile 3", "Profile 4"))

# PLOT -----------------------------------------------------------------------

  # --- one facet plot, shared scale and axis order
  p_cor <- ggplot(df_cor, aes(Var2, Var1, fill = cor)) +
    geom_tile(colour = "#d9d9d9", linewidth=0.1) +

    scale_fill_gradient2(limits = c(-1, 1), midpoint = 0, name = "Spearman \u03C1",
                         low = "#C38F16",       # negative
                         mid = "#F9F9EB",       # zero
                         high = "#0A2E57") +    # positive

    geom_text(data = df_lab, aes(label = lab, colour = ifelse(abs(cor) > 0.55, "dark", "light")),
              size = 6, size.unit = "pt", show.legend = FALSE) +    # r labels
    geom_text(data = df_cor, aes(label = star, colour = ifelse(abs(cor) > 0.55, "dark", "light")),
              vjust = -0.08, size = 3, show.legend = FALSE) +       # starts above
    scale_colour_manual(values = c(dark = "#FFFFFF", light = "#111111")) +

    facet_wrap(~ grp, ncol = 2) +     # 2x2 grid; use ncol = 4 if you prefer one row
    labs(x = NULL, y = NULL) +

    coord_equal() +
    coord_fixed(ratio = 1) +    # xlim = c(0, 6), ylim = c(0.25, 5.5)

    # add caption for displaying the significance
    labs(caption = "Significance (BH-adjusted p): *** p<0.001, ** p<0.01, * p<0.05") +

    theme_minimal(base_family = "sans") +

    theme(

      #plot.title         = element_text(),
      #plot.subtitle      = element_text(),
      #plot.caption       = element_text("),
      #plot.tag           = element_text(),
      #plot.tag.position  = c(),

      axis.title = element_blank(),
      axis.text.y = element_text(size = 7, colour = "#525252", hjust = 1),
      axis.text.x = element_text(size = 7, colour = "#525252", angle = 45, hjust = 1),

      plot.caption.position = "plot",
      plot.caption = element_text(face = "italic", size = 7, colour = "#252525", hjust = -0.01, vjust = -4.7),

      #strip.background = element_rect(fill = "#d9d9d9"),
      strip.text =  element_text(face =  "bold", size = 12, vjust = 0.8),

      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),

      legend.position = c(1, -0.19),
      legend.direction = "horizontal",
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.box.background = element_blank(),

      #legend.margin = margin(t = 0, r = 0, b = -1, l =0),

      #legend.box.margin = margin(t = 0, r = 0, b = 2, l =0),

      #legend.spacing.y = unit(0, "mm"),

      legend.title       = element_text(size = 8, colour = "#252525"),
      legend.text        = element_text(size = 7, colour = "#525252"),
      legend.key.height  = unit(4.0, "mm"),
      legend.key.width   = unit(8.0, "mm"),

      plot.background = element_rect(fill = "#F9F9EB", colour = NA),

      panel.spacing.x = unit(12, "mm"),
      panel.spacing.y = unit(6, "mm"),

      plot.margin        = margin(10, 0, 24, 0)  # mm around the figure
    )

  p_cor

  # TIFF @ 1000 dpi
  ggsave("figs/p_cor_clusters.tiff", p_cor, device = "tiff",
         width = 183, height = 165, units = "mm",
         dpi = 1000, compression = "lzw")
