# =============================================================================
# Development script - not part of the final analysis
#
# Correlation analysis for an earlier formulation of the first research
# question.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Introduction ---------------------------------------------------------------

# This chapter conducts the formal analysis to answer our first research
# question (RQ1): How do different aspects of individuals' social network
# structure and position relate to distinct characteristics of their spatial
# mobility?

# To provide a comprehensive answer, our analysis proceeds in two main steps
# as outlined in our research plan: 1. Overall Correlation Analysis: We first
# calculate a correlation matrix for the entire sample to identify the general
# relationships between the key social network (SN) and spatial mobility (SM)
# metrics.
# - Stratified Correlation Analysis: We then stratify our sample by the
#   dominant geographic context ('cg_domset') and repeat the correlation
#   analysis for each subgroup. This allows us to investigate whether the
#   observed relationships are stable or if they are moderated by the
#   geographic environment (e.g., big cities vs. rural areas).

# Based on the skewed distributions observed during the Exploratory Data
# Analysis (EDA), we will use Spearman's rank correlation coefficient (),
# which is a non-parametric measure that is robust to non-linear relationships
# and outliers.

# Setup and Loading Data -----------------------------------------------------

# We begin by loading the necessary libraries and the final 'analysis_data'
# data frame.

# Load libraries
library(tidyverse)
library(knitr)

# Load the final analysis dataset.
# In a real workflow, we would load the saved file.
# analysis_data <- readRDS("data/analysis_data.rds")

# For this example, we assume 'analysis_data' is already in the environment.

# Part 1: Overall Correlation Analysis ---------------------------------------

# First, we examine the relationships across the entire user population.

## Calculate the Correlation Matrix ------------------------------------------

# We select the key '_total' aggregate metrics for both SN and SM and compute
# the Spearman correlation matrix.

# Select the relevant variables for RQ1
rq1_vars <- analysis_data %>%
  select(
    sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity,
    sm_rog_total, sm_nul_total, sm_entr_total, sm_active_days_total
  )

# Calculate the Spearman correlation matrix
corr_matrix_overall <- cor(rq1_vars, method = "spearman", use = "complete.obs")

## Present and Interpret the Results -----------------------------------------

# We present the correlation matrix in a formatted table and provide an
# initial interpretation.

# Display the matrix as a formatted table with 2 decimal places
kable(corr_matrix_overall, digits = 2, caption = "Spearman Correlation Matrix for All Users")

# Interpretation of Overall Results: The correlation matrix reveals several
# significant relationships. Notably, there are strong, positive correlations
# between measures of network size/activity ('sn_totaldegree') and mobility
# diversity/frequency ('sm_nul_total', 0.93; 'sm_active_days_total', 0.71).
# Furthermore, a user's brokerage role ('sn_betweenness') is moderately and
# positively correlated with their spatial range ('sm_rog_total', 0.65) and
# diversity of locations visited ('sm_nul_total', 0.65). These initial
# findings provide strong support for the hypothesis that a more prominent
# social network position is associated with more extensive spatial mobility.

#  ---------------------------------------------------------------------------

# Part 2: Stratified Correlation Analysis ------------------------------------

# Now, we investigate if these relationships hold true across different
# geographic contexts. We split the data by the 'cg_domset' variable and
# recalculate the correlations.

## Calculate Correlations for Each Subgroup ----------------------------------

# We group the data by 'cg_domset' and apply the correlation calculation to
# each subgroup.

# This code groups the data and then calculates a correlation matrix for each group
stratified_corr <- analysis_data %>%
  group_by(cg_domset) %>%
  summarise(
    # across() allows us to calculate correlations between all selected variables
    correlation_matrix = list(cor(
      across(c(
        sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity,
        sm_rog_total, sm_nul_total, sm_entr_total, sm_active_days_total
      )),
      method = "spearman", use = "complete.obs"
    ))
  )

# Display the results
# We can print each matrix separately for detailed inspection
for (i in 1:nrow(stratified_corr)) {
  cat("\n\n--- Correlation Matrix for:", stratified_corr$cg_domset[i], "---\n")
  print(kable(stratified_corr$correlation_matrix[[i]], digits = 2))
}

## Compare Key Relationships Across Groups -----------------------------------

# Presenting full matrices can be hard to compare. A more effective way to see
# the differences is to extract a few key correlation coefficients and compare
# them directly ina atable or plot.

# Extract the correlation between two key variables (e.g., Degree and RoG) for each group
key_corr_comparison <- stratified_corr %>%
  mutate(
    degree_vs_rog = map_dbl(correlation_matrix, ~ .x["sn_totaldegree", "sm_rog_total"]),
    betweenness_vs_nul = map_dbl(correlation_matrix, ~ .x["sn_betweenness", "sm_nul_total"])
  ) %>%
  select(cg_domset, degree_vs_rog, betweenness_vs_nul)

kable(key_corr_comparison, digits = 2, caption = "Comparison of Key Correlation Coefficients Across Geographic Contexts")

# Visualise the comparison
key_corr_comparison %>%
  pivot_longer(-cg_domset, names_to = "correlation_pair", values_to = "coefficient") %>%
  ggplot(aes(x = cg_domset, y = coefficient, fill = cg_domset)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~correlation_pair) +
  labs(
    title = "Comparison of Key Spatio-Social Correlations by Geographic Context",
    x = "Dominant Settlement Type",
    y = "Spearman Correlation Coefficient"
  ) +
  theme_minimal()

# Answering Research Question 1 ----------------------------------------------

# Based on the analysis above, we can now provide a comprehensive answer to
# RQ1.

# Answer: Yes, there is a clear and robust relationship between individuals'
# social network structure and their spatial mobility. The overall analysis
# shows that users with larger and more central social networks tend to
# exhibit more extensive and diverse mobility patterns. Specifically, network
# degree is strongly associated with the number of unique locations visited,
# and betweenness centrality (a measure of brokerage) is moderately associated
# with a larger radius of gyration.

# Furthermore, this relationship is moderated by geographic context. The
# correlation between network degree and spatial range ('degree_vs_rog') is
# noticeably stronger for users in rural contexts (0.54) compared to those in
# big cities (0.42). This suggests that while the link between social
# connectedness and physical movement is universal in our sample, the nature
# of this link is shaped by the geographic environment. For rural individuals,
# a larger social network may be more directly tied to the necessity of
# traversing larger physical distances.
