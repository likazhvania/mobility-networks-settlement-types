# =============================================================================
# Development script - not part of the final analysis
#
# Exploratory analysis for an earlier formulation of the first research
# question (network position and mobility).
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Introduction ---------------------------------------------------------------

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

# Dataset size: 'r format(nrow(analysis_data), big.mark = ",")' users x 'r
# ncol(analysis_data)' variables.

format(nrow(analysis_data), big.mark = ",")
ncol(analysis_data)

# Snapshot & Integrity Checks ------------------------------------------------

## Structure -----------------------------------------------------------------

str(analysis_data, vec.len = 2)

# Descriptive Statistics -----------------------------------------------------

# Create (sub)datasets for RQ1. ----------------------------------------------

# We will also select out datasets to keep only the required variables that
# will be used for RQ1.

# --- The variables/metrics that will be analysed for RQ1 ---

# 'user_id'

# SN metrics
# 'sn_indegree'
# 'sn_outdegree'
# 'sn_totaldegree'
# 'sn_betweenness'
# 'sn_closeness'
# 'sn_reciprocity'

# SM metrics
# 'sm_nul_total'
# 'sm_active_days_total'
# 'sm_entr_total'
# 'sm_rog_total'

# CG
# 'cg_domset'   # from CG netrics, we take only 'cg_domset' (dominant settlement type) to make analysis by settlement types

# Total Analysis data
rq1_data_total <- analysis_data %>%
  select('user_id',
         'sn_indegree', 'sn_outdegree', 'sn_totaldegree', 'sn_betweenness', 'sn_closeness', 'sn_clustering', 'sn_reciprocity',
         'sm_nul_total', 'sm_active_days_total', 'sm_entr_total', 'sm_rog_total',
         'cg_domset')

# Dominant Settlement: Big City
rq1_data_domset_bc <- rq1_data_total %>%
  filter(cg_domset == 'Urban: big city')

# Dominant Settlement: Small City
rq1_data_domset_sc <- rq1_data_total %>%
  filter(cg_domset == 'Urban: small city')

# Dominant Settlement: Rural
rq1_data_domset_r <- rq1_data_total %>%
  filter(cg_domset == 'rural')

## Calculate statistics for the total data -----------------------------------

num_cols <- names(which(vapply(rq1_data_total, is.numeric, logical(1))))

if (length(num_cols) == 0) {
  stop("No numeric columns found")
}

length(num_cols)

rq1_num_stats_total <- rq1_data_total %>%
  select(all_of(num_cols)) %>%
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

print(rq1_num_stats_total)

# Join to the variables labels
rq1_num_stats_total <- add_var_labels(rq1_num_stats_total)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(rq1_num_stats_total, label_order)

rq1_num_stats_total <- rq1_num_stats_total %>%
  select(-label_order)

print(rq1_num_stats_total)

options(scipen = 999)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_total, digits = 2, caption = "Descriptive Statistics of Key Metrics: All Data", row.names = FALSE)

## Calculate statistics for the subset data: Dominant Settlement Type - Big City ----

num_cols <- names(which(vapply(rq1_data_domset_bc, is.numeric, logical(1))))

if (length(num_cols) == 0) {
  stop("No numeric columns found")
}

length(num_cols)

rq1_num_stats_domset_bc <- rq1_data_domset_bc %>%
  select(all_of(num_cols)) %>%
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

print(rq1_num_stats_domset_bc)

# Join to the variables labels
rq1_num_stats_domset_bc <- add_var_labels(rq1_num_stats_domset_bc)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(rq1_num_stats_domset_bc, label_order)

rq1_num_stats_domset_bc <- rq1_num_stats_domset_bc %>%
  select(-label_order)

print(rq1_num_stats_domset_bc)

options(scipen = 999)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_domset_bc, digits = 2, caption = "Descriptive Statistics of Key Metrics: Group - Dominant Settlement: Big City", row.names = FALSE)

## Calculate statistics for the subset data: Dominant Settlement Type - Small City ----

num_cols <- names(which(vapply(rq1_data_domset_sc, is.numeric, logical(1))))

if (length(num_cols) == 0) {
  stop("No numeric columns found")
}

length(num_cols)

rq1_num_stats_domset_sc <- rq1_data_domset_sc %>%
  select(all_of(num_cols)) %>%
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

print(rq1_num_stats_domset_sc)

# Join to the variables labels
rq1_num_stats_domset_sc <- add_var_labels(rq1_num_stats_domset_sc)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(rq1_num_stats_domset_sc, label_order)

rq1_num_stats_domset_sc <- rq1_num_stats_domset_sc %>%
  select(-label_order)

print(rq1_num_stats_domset_sc)

options(scipen = 999)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_domset_sc, digits = 2, caption = "Descriptive Statistics of Key Metrics: Group - Dominant Settlement: Small City", row.names = FALSE)

## Calculate statistics for the subset data: Dominant Settlement Type - Rural ----

num_cols <- names(which(vapply(rq1_data_domset_r, is.numeric, logical(1))))

if (length(num_cols) == 0) {
  stop("No numeric columns found")
}

length(num_cols)

rq1_num_stats_domset_r <- rq1_data_domset_r %>%
  select(all_of(num_cols)) %>%
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

print(rq1_num_stats_domset_r)

# Join to the variables labels
rq1_num_stats_domset_r <- add_var_labels(rq1_num_stats_domset_r)

# To keep the order as it is in 'var_labels' dataset.
data.table::setorder(rq1_num_stats_domset_r, label_order)

rq1_num_stats_domset_r <- rq1_num_stats_domset_r %>%
  select(-label_order)

print(rq1_num_stats_domset_r)

options(scipen = 999)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_domset_r, digits = 2, caption = "Descriptive Statistics of Key Metrics: Group - Dominant Settlement: Rural", row.names = FALSE)

# All tables

options(scipen = 999)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_total, digits = 4, caption = "Descriptive Statistics of Key Metrics: All Data", row.names = FALSE)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_domset_bc, digits = 4, caption = "Descriptive Statistics of Key Metrics: Group - Dominant Settlement: Big City", row.names = FALSE)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_domset_sc, digits = 4, caption = "Descriptive Statistics of Key Metrics: Group - Dominant Settlement: Small City", row.names = FALSE)

# Display with knitr::kable() for a nice table format
knitr::kable(rq1_num_stats_domset_r, digits = 4, caption = "Descriptive Statistics of Key Metrics: Group - Dominant Settlement: Rural", row.names = FALSE)

# Covariance -----------------------------------------------------------------

df <- data.table::as.data.table(rq1_data_total)  # keeps the original safe

# Recreate variable families for convenience
rq1_sn_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "sn_")]
rq1_sm_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "sm_")]
rq1_cg_vars <- names(rq1_data_total)[startsWith(names(rq1_data_total), "cg_")]

# --- 0. Pick *all* numeric sn_, sm_, and cg_ variables ---
cov_vars <- c(rq1_sn_vars, rq1_sm_vars, setdiff(rq1_cg_vars, "cg_domset"))

# --- 1. Drop any variables that turned out to be non-numeric or constant ---
cov_vars <- cov_vars[vapply(df[, ..cov_vars], is.numeric, logical(1))]

# dro zero-variance columns
zero_var <- names(which(vapply(df[, ..cov_vars], function(x) var(x, na.rm = TRUE) == 0,
                               logical(1))))

if (length(zero_var)) {
  message("Zero-variance variables (excluded): ", paste(zero_var, collapse = ", "))
  cov_vars <- setdiff(cov_vars, zero_var)
}

# --- 2. Covariance matrix (pairwise complete obs) ---
cov_mat <- cov(df[, ..cov_vars], use = "pairwise.complete.obs")

# --- 3. Top-20 absolute covariances with labels ---
cov_long <- as.data.table(as.table(cov_mat))[V1 < V2]  # upper triangle
setnames(cov_long, c("V1", "V2", "cov"))

# attach labels (assumes var_labels is already loaded)
cov_long <- cov_long %>%
  left_join(var_labels[, .(variable, label)], by = c("V1" = "variable")) %>%
  rename(label1 = label) %>%
  left_join(var_labels[, .(variable, label)], by = c("V2" = "variable")) %>%
  rename(label2 = label)

top20 <- cov_long[order(-abs(cov))][1:20]

knitr::kable(top20[, .(label1, label2, cov)],
             digits = 3,
             caption = "Top 20 absolute covariances among SN, SM, and CG metrics")

# --- 4. Covariance heat-map (upper triangle) ---
ggplot(cov_long, aes(V1, V2, fill = cov)) +
  geom_tile() +
  scale_fill_gradient2(low = scales::muted("blue"),
                       high = scales::muted("red"),
                       mid = "white",
                       midpoint = 0) +
  labs(title = "Covariance Heat-map (upper triangle)",
       x = NULL, y = NULL, fill = "cov") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7))

std_cov <- cov(scale(df[, ..cov_vars], center = TRUE, scale = TRUE),
               use = "pairwise.complete.obs")

std_long <- as.data.table(as.table(std_cov)) # V1, V2, Freq
setnames(std_long, c("V1", "V2", "std_cov"))

# Upper triangle only, for a half matrix
#std_long <- std_long[V1 < V2]

ggplot(std_long, aes(V1, V2, fill = std_cov)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1),
                       low = scales::muted("blue"),
                       high = scales::muted("red"),
                       mid = "white",
                       midpoint = 0) +

  # --- numeric labels ---
  geom_text(
    data = std_long,
    aes(label = sprintf("%.2f", std_cov)),
    colour = "black", size = 4
  ) +

  labs(title = "Standardised Covariance (= Correlation) Heat-map",
       x = NULL, y = NULL, fill = expression(rho)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7))
