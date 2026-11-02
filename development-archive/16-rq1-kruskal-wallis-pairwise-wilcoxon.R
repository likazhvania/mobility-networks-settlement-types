# =============================================================================
# Development script - not part of the final analysis
#
# Kruskal-Wallis and pairwise Wilcoxon tests for an earlier formulation of
# the first research question.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Libraries
library(data.table)
library(tidyverse)
library(broom)

# --- 0. Ensure data.table & factor ---
setDT(rq1_data_total)
rq1_data_total[ , cg_domset := factor(cg_domset)]

rq1_sn_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "sn_")]
rq1_sm_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "sm_")]

rq1_test_vars <- c(rq1_sn_vars, rq1_sm_vars)

# --- 1. Run Kruskal-Wallis for every variable ---
kw_tbl <- lapply(rq1_test_vars, function(v) {
  kr <- kruskal.test(rq1_data_total[[v]] ~ rq1_data_total$cg_domset)
  broom::tidy(kr) |>
    mutate(variable = v) |>
    select(variable, statistic, p.value, parameter)
}) |> bind_rows()

# adjust p-values (BH)
kw_tbl <- kw_tbl |> mutate(p_adj = p.adjust(p.value, method = "BH"))

# attach human-readable labels from 'var_labels'
kw_tbl <- kw_tbl |>
  left_join(var_labels, by = c("variable")) |>
  relocate(label, .after = variable) |>
  relocate(category, .after = label) |>
  select(-label_order) |>
  arrange(p_adj)

knitr::kable(kw_tbl,
             digits = 4,
             caption = "Kruskal-Wallis results for selected SN & SM metrics (BH-adjusted)")

# Effect-size metric for Kruskal-Wallis --------------------------------------

#  ---------------------------------------------------------------------------
# 1. helper to compute eta2_H from kruskal.test object + sample size
eta2_H <- function(kr, n, k) {
  H <- kr$statistic
  max(0, (H - (k -1)) / (n - k))   # truncated at 0 to avoid negatives
}

k <- length(unique(rq1_data_total$cg_domset))   # = 3

#  ---------------------------------------------------------------------------
# 2. Redo the KW loop, this time grabbing n & eta2_H
kw_tbl <- lapply(rq1_test_vars, function(v) {

  # remove NAs per variable
  vec <- rq1_data_total[[v]]
  grp <- rq1_data_total$cg_domset
  ok <- !is.na(vec)

  kr <- kruskal.test(vec[ok] ~ grp[ok])
  n <- sum(ok)

  tibble::tibble(
    variable     = v,
    H            = unname(kr$statistic),
    df           = unname(kr$parameter),
    p_value      = kr$p.value,
    eta2_H       = eta2_H(kr, n, k)
  )
}) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(p_adj = p.adjust(p_value, method = "BH")) %>%
  dplyr::left_join(var_labels, by = "variable") %>%
  dplyr::relocate(label, .after = variable) %>%
  dplyr::relocate(category, .after = label) %>%
  dplyr::select(-label_order) %>%
  dplyr::arrange(dplyr::desc(eta2_H))

knitr::kable(
  #kw_tbl %>% dplyr::select(label, eta2_H, p_adj),
  kw_tbl,
  digits = 4,
  caption = "Kruskal-Wallis effect sizes and BH-adjusted p_values"
)

# Pairwise Wilcoxon test -----------------------------------------------------

# Libraries
# library(data.table)
# library(tidyverse)
# library(broom)

# Ensure data.table object and factor grouping
setDT(rq1_data_total)
rq1_data_total[ , cg_domset := factor(cg_domset)]

# Metrics to test
rq1_sn_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "sn_")]
rq1_sm_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "sm_")]

rq1_test_vars <- c(rq1_sn_vars, rq1_sm_vars)

# Helper to run pairwise Wilcoxon for one variable
run_pw <- function(v) {
  pw <- pairwise.wilcox.test(
    rq1_data_total[[v]],
    rq1_data_total$cg_domset,
    p.adjust.method = "BH")

  # convert the matrix to long form
  broom::tidy(pw) %>%        # columns: group1, group2, p.adjusted
    mutate(variable = v) %>%
    rename(p_adj = p.value)
}

# Loop over all variables
pw_long <- bind_rows(lapply(rq1_test_vars, run_pw))

# Attach readable labels from 'var_labels'
pw_long <- pw_long %>%
  left_join(var_labels, by = c("variable")) %>%
  relocate(label, .after = group2) %>%
  relocate(category, .after = label) %>%
  select(-label_order)

options(scipen = 999)

# Optional: filter significant contrasts
#sig_pw <- pw_long %>% filter(p_adj < 0.05)

knitr::kable(
  pw_long,
  digits = 4,
  caption = "Pairwise Wilcoxon (BH-adjusted)"
)
