# =============================================================================
# Development script - not part of the final analysis
#
# Correlations between network and mobility measures, overall and by dominant
# settlement type, for an earlier formulation of the first research question.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Libraries
library(data.table)
library(tidyverse)

# Correlation - All Users ----------------------------------------------------

# --- 1. Correlation: all numeric variables (whole sample) ---
# Identify EVERY numeric column except user_id

rq1_data_total <- analysis_data %>%
  select("user_id",
         "sn_indegree", "sn_outdegree", "sn_totaldegree", "sn_betweenness", "sn_closeness", "sn_clustering", "sn_reciprocity",
         "sm_nul_total", "sm_active_days_total", "sm_entr_total", "sm_rog_total",
         "cg_domset")

rq1_data_total <- rq1_data_total %>%
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

# Dominant Settlement: Big City
rq1_data_domset_bc <- rq1_data_total %>%
  filter(cg_domset == 'Urban: big city')

# Dominant Settlement: Small City
rq1_data_domset_sc <- rq1_data_total %>%
  filter(cg_domset == 'Urban: small city')

# Dominant Settlement: Rural
rq1_data_domset_r <- rq1_data_total %>%
  filter(cg_domset == 'rural')

# Total Analysis data
num_vars_total <- names(which(vapply(rq1_data_total, is.numeric, logical(1))))
num_vars_total <- setdiff(num_vars_total, "user_id")   # just in case it's numeric

rq1_cor_total <- cor(rq1_data_total[, ..num_vars_total],
                     use = "pairwise.complete.obs",
                     method = "spearman")

# Dominant Settlement: Big City
num_vars_domset_bc <- names(which(vapply(rq1_data_domset_bc, is.numeric, logical(1))))
num_vars_domset_bc <- setdiff(num_vars_domset_bc, "user_id")   # just in case it's numeric

rq1_cor_domset_bc <- cor(rq1_data_domset_bc[, ..num_vars_domset_bc],
                         use = "pairwise.complete.obs",
                         method = "spearman")

# Dominant Settlement: Small City
num_vars_domset_sc <- names(which(vapply(rq1_data_domset_sc, is.numeric, logical(1))))
num_vars_domset_sc <- setdiff(num_vars_domset_sc, "user_id")   # just in case it's numeric

rq1_cor_domset_sc <- cor(rq1_data_domset_sc[, ..num_vars_domset_sc],
                         use = "pairwise.complete.obs",
                         method = "spearman")

# Dominant Settlement: Rural
num_vars_domset_r <- names(which(vapply(rq1_data_domset_r, is.numeric, logical(1))))
num_vars_domset_r <- setdiff(num_vars_domset_r, "user_id")   # just in case it's numeric

rq1_cor_domset_r <- cor(rq1_data_domset_r[, ..num_vars_domset_r],
                        use = "pairwise.complete.obs",
                        method = "spearman")

# rq1_cor_total now holds a square matrix (#vars x #vars) for the whole
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
    p_long(p_mat(rq1_data_total),       "All users"),
    p_long(p_mat(rq1_data_domset_bc),   "Big city"),
    p_long(p_mat(rq1_data_domset_sc),   "Small city"),
    p_long(p_mat(rq1_data_domset_r),    "Rural")
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
    cor_long(rq1_cor_total,         "All users"),
    cor_long(rq1_cor_domset_bc,     "Big city"),
    cor_long(rq1_cor_domset_sc,     "Small city"),
    cor_long(rq1_cor_domset_r,      "Rural")
  )

  # Join with 'var_labels'
  # df_cor <- df_cor %>% mutate(variable = Var1) %>%
  # left_join(var_labels, by = "variable") %>%
  # mutate(Var1_lab = label,
  # Var1_cat = category) %>%
  # select(Var1, Var1_lab, Var1_cat, Var2, cor, grp) %>%

  # mutate(variable = Var2) %>%

  # left_join(var_labels, by = "variable") %>%
  # mutate(Var2_lab = label,
  # Var2_cat = category) %>% select(Var1, Var1_lab, Var1_cat, Var2, Var2_lab, Var2_cat, cor, grp)

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

  df_p$grp <- factor(df_p$grp, levels = c("All users", "Big city", "Small city", "Rural"))
  df_cor$grp <- factor(df_cor$grp, levels = c("All users", "Big city", "Small city", "Rural"))

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
  ggsave("figs/p_cor.tiff", p_cor, device = "tiff",
         width = 183, height = 165, units = "mm",
         dpi = 1000, compression = "lzw")
