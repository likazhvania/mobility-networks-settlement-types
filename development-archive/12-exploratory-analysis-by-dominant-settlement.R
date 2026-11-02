# =============================================================================
# Development script - not part of the final analysis
#
# Descriptive statistics of the measures by users' dominant settlement type;
# an early version of the exploratory analysis.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Introduction ---------------------------------------------------------------

# Here, we explore descriptive statistics by the dominant settlement types the
# users have visited most.

# Setup and Loading Data -----------------------------------------------------

# We begin by loading the necessary libraries for analysis and visualization,
# and then load the final 'analysis_data' data frame created in the previous
# chapter.

# Load libraries
library(data.table)
library(tidyverse)
library(patchwork) # For combining multiple ggplots
# library(ggcorrplot) # For plotting correlation matrices
# library(corrr) # For creating correlation matrices easily
# Note: ggcorrplot and corrr are not available, so we will use base R and ggplot2 alternatives.

# Load the final analysis dataset.
# In a real workflow, we would load the saved file.
# analysis_data <- readRDS("data/analysis_data.rds")

# For this example, we assume 'analysis_data' is already in the environment.

# Custom ggplot theme for journal-ready plots
custom_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(size = 8, hjust = 0))

theme_set(custom_theme)

# Colour-blind-friendly palette helper
pal <- function(n) scales::hue_pal(l = 65, c = 100)(n)

# Ensure key identifier is treated as character
if(is.integer(analysis_data$user_id) || is.numeric(analysis_data$user_id)) analysis_data[, user_id := as.character(user_id)]

# Create sub-datasets by dominant settlement types ---------------------------

# Big City
dom_cg_domset_bc <- analysis_data %>%
  filter(cg_domset == "Urban: big city")

# Small City
dom_cg_domset_sc <- analysis_data %>%
  filter(cg_domset == "Urban: small city")

# Rural
dom_cg_domset_r <- analysis_data %>%
  filter(cg_domset == "rural")

# Recreate variable families for convenience
sn_vars_bc <- names(dom_cg_domset_bc)[startsWith(names(dom_cg_domset_bc), "sn_")]
sn_vars_sc <- names(dom_cg_domset_sc)[startsWith(names(dom_cg_domset_sc), "sn_")]
sn_vars_r <- names(dom_cg_domset_r)[startsWith(names(dom_cg_domset_r), "sn_")]

sm_vars_bc <- names(dom_cg_domset_bc)[startsWith(names(dom_cg_domset_bc), "sm_")]
sm_vars_sc <- names(dom_cg_domset_sc)[startsWith(names(dom_cg_domset_sc), "sm_")]
sm_vars_r <- names(dom_cg_domset_r)[startsWith(names(dom_cg_domset_r), "sm_")]

cg_vars_bc <- names(dom_cg_domset_bc)[startsWith(names(dom_cg_domset_bc), "cg_")]
cg_vars_sc <- names(dom_cg_domset_sc)[startsWith(names(dom_cg_domset_sc), "cg_")]
cg_vars_r <- names(dom_cg_domset_r)[startsWith(names(dom_cg_domset_r), "cg_")]

# Dataset size: 'r format(nrow(analysis_data), big.mark = ",")' users x 'r
# ncol(analysis_data)' variables.

format(nrow(dom_cg_domset_bc), big.mark = ",")
ncol(dom_cg_domset_bc)

format(nrow(dom_cg_domset_sc), big.mark = ",")
ncol(dom_cg_domset_sc)

format(nrow(dom_cg_domset_r), big.mark = ",")
ncol(dom_cg_domset_r)

# Snapshot & Integrity Checks ------------------------------------------------

## Structure -----------------------------------------------------------------

str(dom_cg_domset_bc, vec.len = 2)
str(dom_cg_domset_sc, vec.len = 2)
str(dom_cg_domset_r, vec.len = 2)

# Descriptive Statistics -----------------------------------------------------

# Big City
num_cols_bc <- names(which(vapply(dom_cg_domset_bc, is.numeric, logical(1))))

# Small City
num_cols_sc <- names(which(vapply(dom_cg_domset_sc, is.numeric, logical(1))))

# Rural
num_cols_r <- names(which(vapply(dom_cg_domset_r, is.numeric, logical(1))))

if (length(num_cols_bc) == 0) {
  stop("No numeric columns found")
}
length(num_cols_bc)

if (length(num_cols_sc) == 0) {
  stop("No numeric columns found")
}
length(num_cols_sc)

if (length(num_cols_r) == 0) {
  stop("No numeric columns found")
}
length(num_cols_r)

dom_cg_domset_bc
dom_cg_domset_sc
dom_cg_domset_r

# Big City
num_cols_bc
# Small City
num_cols_sc
# Rural
num_cols_r

## Dominant Settlement: Big City ---------------------------------------------

num_stats_bc <- dom_cg_domset_bc %>%
  select(all_of(num_cols_bc)) %>%
  summarise(across(everything(), list(
    mean   = ~mean(.x, na.rm = TRUE),
    sd     = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    min    = ~min(.x, na.rm = TRUE),
    q25    = ~quantile(.x, 0.25, na.rm = TRUE),
    q75    = ~quantile(.x, 0.75, na.rm = TRUE),
    max    = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    everything(),
    names_to  = c("variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  )

print(num_stats_bc)

# Join to the variables labels
num_stats_bc <- add_var_labels(num_stats_bc)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(num_stats_bc, label_order)

num_stats_bc <- num_stats_bc %>%
  select(-label_order)

print(num_stats_bc)

# Display with knitr::kable() for a nice table format
knitr::kable(num_stats_bc, digits = 2, caption = "Descriptive Statistics of Key Metrics | Dominant Settlement: Big City", row.names = FALSE)

## Dominant Settlement: Small City -------------------------------------------

num_stats_sc <- dom_cg_domset_sc %>%
  select(all_of(num_cols_sc)) %>%
  summarise(across(everything(), list(
    mean   = ~mean(.x, na.rm = TRUE),
    sd     = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    min    = ~min(.x, na.rm = TRUE),
    q25    = ~quantile(.x, 0.25, na.rm = TRUE),
    q75    = ~quantile(.x, 0.75, na.rm = TRUE),
    max    = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    everything(),
    names_to  = c("variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  )

print(num_stats_sc)

# Join to the variables labels
num_stats_sc <- add_var_labels(num_stats_sc)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(num_stats_sc, label_order)

num_stats_sc <- num_stats_sc %>%
  select(-label_order)

print(num_stats_sc)

# Display with knitr::kable() for a nice table format
knitr::kable(num_stats_sc, digits = 2, caption = "Descriptive Statistics of Key Metrics | Dominant Settlement: Small City", row.names = FALSE)

## Dominant Settlement: Rural ------------------------------------------------

num_stats_r <- dom_cg_domset_r %>%
  select(all_of(num_cols_r)) %>%
  summarise(across(everything(), list(
    mean   = ~mean(.x, na.rm = TRUE),
    sd     = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    min    = ~min(.x, na.rm = TRUE),
    q25    = ~quantile(.x, 0.25, na.rm = TRUE),
    q75    = ~quantile(.x, 0.75, na.rm = TRUE),
    max    = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    everything(),
    names_to  = c("variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  )

print(num_stats_r)

# Join to the variables labels
num_stats_r <- add_var_labels(num_stats_r)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(num_stats_r, label_order)

num_stats_r <- num_stats_r %>%
  select(-label_order)

print(num_stats_r)

# Display with knitr::kable() for a nice table format
knitr::kable(num_stats_r, digits = 2, caption = "Descriptive Statistics of Key Metrics | Dominant Settlement: Rural", row.names = FALSE)
