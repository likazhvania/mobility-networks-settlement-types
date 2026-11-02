# =============================================================================
# 12  Exploratory analysis
#
# First look at the analysis dataset: structure and integrity checks,
# missing values, descriptive statistics for every measure, distributions of
# the network, mobility and settlement measures, the number of users by
# dominant settlement type, and covariance and correlation between all
# measures.
#
# Input:  analysis_data (script 08); var_labels and add_var_labels() (11)
# Output: num_stats (descriptive statistics) and diagnostic plots
# =============================================================================

# Setup ----------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(patchwork) # For combining multiple ggplots
# library(ggcorrplot) # For plotting correlation matrices
# library(corrr) # For creating correlation matrices easily
# ggcorrplot and corrr were not available, so base R and ggplot2 are used
# instead.

# analysis_data is taken from script 08 in the same R session; to start from
# a saved copy instead:
# analysis_data <- readRDS("data/analysis_data.rds")

# Plot theme
custom_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(size = 8, hjust = 0))

theme_set(custom_theme)

# Colour-blind-friendly palette helper
pal <- function(n) scales::hue_pal(l = 65, c = 100)(n)

# user_id as character
if(is.integer(analysis_data$user_id) || is.numeric(analysis_data$user_id)) analysis_data[, user_id := as.character(user_id)]

# Variable groups by prefix
sn_vars <- names(analysis_data)[startsWith(names(analysis_data), "sn_")]
sm_vars <- names(analysis_data)[startsWith(names(analysis_data), "sm_")]
cg_vars <- names(analysis_data)[startsWith(names(analysis_data), "cg_")]

# Number of users and variables

format(nrow(analysis_data), big.mark = ",")
ncol(analysis_data)

# Integrity checks -----------------------------------------------------------

## Structure -----------------------------------------------------------------

str(analysis_data, vec.len = 2)

## One row per user ----------------------------------------------------------

unique_ids <- uniqueN(analysis_data$user_id)
cat("Unique 'user_id' values:", unique_ids, "\n")

if(unique_ids < nrow(analysis_data)) {
  cat("Warning: duplicate user IDs found!\n")
}

## Missing values ------------------------------------------------------------

na_counts <- sapply(analysis_data, function(x) sum(is.na(x)))

na_analysis_data <- data.frame(variable = names(na_counts), n_miss = na_counts,
                               pct_miss = round(100 * na_counts / nrow(analysis_data), 2))

ggplot(na_analysis_data, aes(reorder(variable, pct_miss), pct_miss)) +
  geom_col(fill = pal(1)) +
  coord_flip() +
  labs(title = "Percentage of Missing Values by Variable", x = NULL, y = "% missing")

# Descriptive statistics -----------------------------------------------------

# Mean, standard deviation, median, minimum, quartiles and maximum of every
# numeric variable.

num_cols <- names(which(vapply(analysis_data, is.numeric, logical(1))))

if (length(num_cols) == 0) {
  stop("No numeric columns found")
}

length(num_cols)

num_stats <- analysis_data %>%
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

print(num_stats)

# Add readable labels
num_stats <- add_var_labels(num_stats)

# Dictionary order
data.table::setorder(num_stats, label_order)

num_stats <- num_stats %>%
  select(-label_order)

print(num_stats)

# Formatted table
knitr::kable(num_stats, digits = 2, caption = "Descriptive Statistics of Key Metrics", row.names = FALSE)

# Distributions --------------------------------------------------------------

# --- Histogram helpers ---
# Linear scale
plot_dist <- function(var, title) {
  analysis_data %>%
    ggplot(aes(x = .data[[var]] + 1)) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = title, x = paste(title), y = "Number of Users") +
    theme_minimal()
}

# Log scale (values + 1, so that zeros can be shown)
plot_dist_log <- function(var, title) {
  analysis_data %>%
    ggplot(aes(x = .data[[var]] + 1)) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
    scale_x_log10() +
    labs(title = title, x = paste(title, "(log-scale)"), y = "Number of Users") +
    theme_minimal()
}

options(scipen = 999)

## Network measures ----------------------------------------------------------

# Heavily skewed, so most are shown on a log scale.

p_sn_deg_total <- plot_dist_log("sn_totaldegree", "Degree Centrality: All-Degree (Total)")
p_sn_deg_in <- plot_dist_log("sn_indegree", "Degree Centrality: In-Degree")
p_sn_deg_out <- plot_dist_log("sn_outdegree", "Degree Centrality: Out-Degree")
p_sn_betw <- plot_dist_log("sn_betweenness", "Betweenness Centrality")
p_sn_close <- plot_dist_log("sn_closeness", "Closeness Centrality")
p_sn_clust <- plot_dist("sn_clustering", "Clustering Coefficient")
p_sn_reciprocity <- plot_dist("sn_reciprocity", "Reciprocity Index")

(p_sn_deg_total | p_sn_deg_in | p_sn_deg_out) / (p_sn_betw | p_sn_close | p_sn_clust / p_sn_reciprocity)

# The network measures are heavy-tailed, as is typical of social networks,
# which is why medians and non-parametric tests are used in the analysis.

## Mobility measures ---------------------------------------------------------

# Monthly totals (used in the analysis)

p_sm_rog_total <- plot_dist_log("sm_rog_total", "Radius of Gyration (Total)")
p_sm_nul_total <- plot_dist_log("sm_nul_total", "Number of Unique Locations (Total)")
p_sm_active_days_total <- plot_dist("sm_active_days_total", "Number of Active Days (Total)")
p_sm_entr_total <- plot_dist("sm_entr_total", "Spatial Entropy (Total)")

(p_sm_rog_total | p_sm_nul_total) / (p_sm_active_days_total | p_sm_entr_total)

# Daily, workday and weekend averages and ratios (earlier design, not used in
# the analysis)

# Average day

p_sm_avg_daily_rog <- plot_dist_log("sm_avg_daily_rog", "Radius of Gyration (Average Daily)")
p_sm_avg_daily_nul <- plot_dist_log("sm_avg_daily_nul", "Number of Unique Locations (Average Daily)")
p_sm_avg_daily_entr <- plot_dist_log("sm_avg_daily_entr", "Spatial Entropy (Average Daily)")
p_sm_avg_daily_moves <- plot_dist_log("sm_avg_daily_moves", "Movement Frequency (Average Daily)")

(p_sm_avg_daily_rog | p_sm_avg_daily_nul) / (p_sm_avg_daily_entr | p_sm_avg_daily_moves)

# Average workday

p_sm_avg_workday_rog <- plot_dist_log("sm_avg_workday_rog", "Radius of Gyration (Average Workday)")
p_sm_avg_workday_nul <- plot_dist_log("sm_avg_workday_nul", "Number of Unique Locations (Average Workday)")
p_sm_avg_workday_entr <- plot_dist_log("sm_avg_workday_entr", "Spatial Entropy (Average Workday)")
p_sm_avg_workday_moves <- plot_dist_log("sm_avg_workday_moves", "Movement Frequency (Average Workday)")

(p_sm_avg_workday_rog | p_sm_avg_workday_nul) / (p_sm_avg_workday_entr | p_sm_avg_workday_moves)

# Average weekend day

p_sm_avg_weekend_rog <- plot_dist_log("sm_avg_weekend_rog", "Radius of Gyration (Average Weekend)")
p_sm_avg_weekend_nul <- plot_dist_log("sm_avg_weekend_nul", "Number of Unique Locations (Average Weekend)")
p_sm_avg_weekend_entr <- plot_dist_log("sm_avg_weekend_entr", "Spatial Entropy (Average Weekend)")
p_sm_avg_weekend_moves <- plot_dist_log("sm_avg_weekend_moves", "Movement Frequency (Average Weekend)")

(p_sm_avg_weekend_rog | p_sm_avg_weekend_nul) / (p_sm_avg_weekend_entr | p_sm_avg_weekend_moves)

# Workday / weekend ratios

p_sm_rog_ratio <- plot_dist_log("sm_rog_ratio", "Radius of Gyration (Ratio: Workday vs. Weekend)")
p_sm_nul_ratio <- plot_dist_log("sm_nul_ratio", "Number of Unique Locations (Ratio: Workday vs. Weekend)")
p_sm_entr_ratio <- plot_dist_log("sm_entr_ratio", "Spatial Entropy (Ratio: Workday vs. Weekend)")
p_sm_moves_ratio <- plot_dist_log("sm_moves_ratio", "Movement Frequency (Ratio: Workday vs. Weekend)")

(p_sm_rog_ratio | p_sm_nul_ratio) / (p_sm_entr_ratio | p_sm_moves_ratio)

# The mobility measures are also strongly skewed: most users move within a
# small area, while a small group is highly mobile.

## Settlement context --------------------------------------------------------

plot_dist_cg <- function(var, title) {
  analysis_data %>%
    ggplot(aes(x = .data[[var]])) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = title, x = paste(title), y = "Number of Users") +
    theme_minimal()
}

# Average daily share of communication events by settlement type
p_cg_avg_pct_big_city <- plot_dist_cg("cg_avg_pct_big_city", "Daily Activity: Big City (Average Daily)")
p_cg_avg_pct_small_city <- plot_dist_cg("cg_avg_pct_small_city", "Daily Activity: Small City (Average Daily)")
p_cg_avg_pct_rural <- plot_dist_cg("cg_avg_pct_rural", "Daily Activity: Rural (Average Daily)")

(p_cg_avg_pct_big_city | p_cg_avg_pct_small_city | p_cg_avg_pct_rural)

# Share of all communication events by settlement type

p_cg_pct_big_city <- plot_dist_cg("cg_pct_big_city", "Big City (Percentage of Calls)")
p_cg_pct_small_city <- plot_dist_cg("cg_pct_small_city", "Small City (Percentage of Calls)")
p_cg_pct_rural <- plot_dist_cg("cg_pct_rural", "Rural (Percentage of Calls)")

(p_cg_pct_big_city | p_cg_pct_small_city | p_cg_pct_rural)

# Number of settlement types and municipalities visited

p_cg_geodiv_hierarchy <- plot_dist_cg("cg_geodiv_hierarchy", "Diversity of Visited Settlement Hierarchy Type")
p_cg_geodiv_municipality <- plot_dist_cg("cg_geodiv_municipality", "Diversity of Visited Municipalities")

(p_cg_geodiv_hierarchy | p_cg_geodiv_municipality)

## Users by dominant settlement type -----------------------------------------

p_cg_domset <- analysis_data %>%
  count(cg_domset) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = reorder(cg_domset, -n), y = n, fill = cg_domset)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.5) +
  labs(title = "User Distribution by Dominant Settlement Type", x = "Dominant Settlement Type", y = "Number of Users") +
  theme_minimal()

p_cg_domset

# Each of the three settlement types contains a large number of users, which
# supports comparisons between them.

# Covariance and correlation -------------------------------------------------

df <- data.table::as.data.table(analysis_data)  # copy, so analysis_data is left unchanged

# --- All numeric sn_, sm_ and cg_ variables ---
cov_vars <- c(sn_vars, sm_vars, setdiff(cg_vars, "cg_domset"))

# --- Keep numeric variables only ---
cov_vars <- cov_vars[vapply(df[, ..cov_vars], is.numeric, logical(1))]

# Drop variables with zero variance
zero_var <- names(which(vapply(df[, ..cov_vars], function(x) var(x, na.rm = TRUE) == 0,
                               logical(1))))

if (length(zero_var)) {
  message("Zero-variance variables (excluded): ", paste(zero_var, collapse = ", "))
  cov_vars <- setdiff(cov_vars, zero_var)
}

# --- Covariance matrix (pairwise complete observations) ---
cov_mat <- cov(df[, ..cov_vars], use = "pairwise.complete.obs")

# --- The 20 largest absolute covariances, with labels ---
cov_long <- as.data.table(as.table(cov_mat))[V1 < V2]  # upper triangle
setnames(cov_long, c("V1", "V2", "cov"))

# Labels from the variable dictionary (script 11)
cov_long <- cov_long %>%
  left_join(var_labels[, .(variable, label)], by = c("V1" = "variable")) %>%
  rename(label1 = label) %>%
  left_join(var_labels[, .(variable, label)], by = c("V2" = "variable")) %>%
  rename(label2 = label)

top20 <- cov_long[order(-abs(cov))][1:20]

knitr::kable(top20[, .(label1, label2, cov)],
             digits = 3,
             caption = "Top 20 absolute covariances among SN, SM, and CG metrics")

# --- Covariance heat map (upper triangle) ---
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

# --- Correlation heat map: covariance of the standardised variables ---
std_cov <- cov(scale(df[, ..cov_vars], center = TRUE, scale = TRUE),
               use = "pairwise.complete.obs")

std_long <- as.data.table(as.table(std_cov)) # V1, V2, Freq
setnames(std_long, c("V1", "V2", "std_cov"))

# For the upper triangle only:
#std_long <- std_long[V1 < V2]

ggplot(std_long, aes(V1, V2, fill = std_cov)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1),
                       low = scales::muted("blue"),
                       high = scales::muted("red"),
                       mid = "white",
                       midpoint = 0) +
  labs(title = "Standardised Covariance (= Correlation) Heat-map",
       x = NULL, y = NULL, fill = expression(rho)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7))
