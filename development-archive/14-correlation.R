# =============================================================================
# Development script - not part of the final analysis
#
# Correlations between all measures, for the whole sample and by dominant
# settlement type.
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
num_vars <- names(which(vapply(df, is.numeric, logical(1))))
num_vars <- setdiff(num_vars, "user_id")   # just in case it's numeric

cor_all <- cor(df[, ..num_vars],
               use = "pairwise.complete.obs",
               method = "spearman")

# Optional: save to disk
# fwrite(as.data.table(as.table(cor_all)), "correlation_all.csv")

# cor_all now holds a square matrix (#vars x #vars) for the whole dataset.

# Correlation - by dominant settlement ---------------------------------------

  # --- Correlation split by cg_domset ---
  df[ , cg_domset := factor(cg_domset)]   # ensure it's a factor

  # Function to compute Spearman matrix for one subset
  corr_subset <- function(sub) {
    cor(sub[, ..num_vars],
        use = "pairwise.complete.obs",
        method = "spearman")
  }

  corr_list <- lapply(levels(df$cg_domset), function(lvl) {
    corr_subset(df[cg_domset == lvl])
  })

  names(corr_list) <- levels(df$cg_domset)

  # Each element: corr_list[["Urban: big city"]] etc.

# Quick heat-map helper (re-use for every matrix) ----------------------------

  plot_corr_heatmap <- function(mat, title = "Correlation") {
    dt <- as.data.table(as.table(mat))
    setnames(dt, c("Var1", "Var2", "cor"))
    dt <- dt[Var1 < Var2]    # upper-triangle only to declutter

    ggplot(dt, aes(Var1, Var2, fill = cor)) +
      geom_tile() +
      scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                           low = scales::muted("blue"),
                           high = scales::muted("red"),
                           mid = "white") +
      labs(title = title, x = NULL, y = NULL, fill = "\u03C1") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 7))
  }

# Visualise ------------------------------------------------------------------

  # Whole sample
  plot_corr_heatmap(cor_all, "Spearman Correlation - All Users")

  # By settlement
  plot_corr_heatmap(corr_list[["Urban: big city"]],
                    "Correlation - Dominant Settlement: Big City")

  plot_corr_heatmap(corr_list[["Urban: small city"]],
                    "Correlation - Dominant Settlement: Small City")

  plot_corr_heatmap(corr_list[["rural"]],
                    "Correlation - Dominant Settlement: Rural")

  # --- helper to melt and optionally filter ---
  corr_long <- function(mat, digits = 2, thresh = NULL) {
    out <- as.data.table(as.table(mat))          # Var1, Var2, Freq
    setnames(out, c("Var1", "Var2", "rho"))
    out <- out[Var1 < Var2]                      # keep upper triangle only
    if (!is.null(thresh)) out <- out[abs(rho) >= thresh] 
    out[ , rho := round(rho, digits)]
    out[order(-abs(rho))]
  }

  # --- overall table ---
  tbl_all <- corr_long(cor_all, thresh = 0.30)   # show rho >= .30; set NULL for all

  knitr::kable(tbl_all,
               caption = "Spearman correlations rho >= 0.30 - All users")

  # By dominant settlement
  # --- Big City ---
  bc <- corr_list[["Urban: big city"]]
  tbl_bc <- corr_long(bc, thresh = 0.30)   # show rho >= .30; set NULL for all

  knitr::kable(tbl_bc,
               caption = "Spearman correlations rho >= 0.30 - Dominant Settlement: Big City")

  # --- Small City ---
  sc <- corr_list[["Urban: small city"]]
  tbl_sc <- corr_long(sc, thresh = 0.30)   # show rho >= .30; set NULL for all

  knitr::kable(tbl_sc,
               caption = "Spearman correlations rho >= 0.30 - Dominant Settlement: Small City")

  # --- Rural ---
  r <- corr_list[["rural"]]
  tbl_r <- corr_long(r, thresh = 0.30)   # show rho >= .30; set NULL for all

  knitr::kable(tbl_r,
               caption = "Spearman correlations rho >= 0.30 - Dominant Settlement: Rural")
