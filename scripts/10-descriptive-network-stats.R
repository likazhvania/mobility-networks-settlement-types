# =============================================================================
# 10  Descriptive network statistics
#
# Network-level description: number of users and ties, distribution of tie
# weights, global reciprocity of the directed network (reported in the paper
# as r = 0.87), and summaries of the user-level network, mobility and
# settlement measures.
#
# Input:  graphs from scripts 04 and 09; user_network_metrics (05),
#         user_mobility_metrics (06), user_contextual_metrics (07)
# Output: printed statistics only
# =============================================================================

library(tidyverse)
library(igraph)

# The graphs are taken from scripts 04 and 09 in the same R session; to start
# from saved copies instead:
# g_dir <- readRDS("data/graph_directed.rds")
# g_und <- readRDS("data/graph_undirected.rds")

# Size of the network --------------------------------------------------------

## Users ---------------------------------------------------------------------

# Number of distinct users in the filtered network.
num_nodes <- gorder(g_dir_all)

cat("Total number of nodes (users) in the network:", num_nodes)

## Ties ----------------------------------------------------------------------

# Number of ties in the full and induced versions, directed and undirected.
num_edges_dir_all     <- gsize(g_dir_all)      # (directed ties)
num_edges_dir_induced <- gsize(g_dir_induced)  # (directed ties)
num_edges_und_all     <- gsize(g_und_all)      # (undirected ties)
num_edges_und_induced <- gsize(g_und_induced)  # (undirected ties)

cat("Total number of directed edges (full network):", num_edges_dir_all, "\n")
cat("Total number of directed edges (induced network):", num_edges_dir_induced, "\n")
cat("Total number of undirected edges (full network):", num_edges_und_all, "\n")
cat("Total number of undirected edges (induced network):", num_edges_und_induced)

## Tie weights ---------------------------------------------------------------

# Tie weight is the number of communication events on a tie, used as a proxy
# for tie strength.

# Directed graph
weight_summary_dir <- summary(E(g_dir_all)$weight)
cat("Summary of Edge Weights (Number of Calls) in directed graph:\n")
print(weight_summary_dir)

# Undirected graph
weight_summary_undir <- summary(E(g_und_all)$weight)
cat("\nSummary of Edge Weights (Number of Calls) in undirected graph:\n")
print(weight_summary_undir)

# # Distribution of tie weights
# ggplot(data.frame(weight = E(g_dir)$weight), aes(x = weight)) +
#   geom_histogram(bins = 50, fill = "steelblue", color = "white") +
#   scale_x_log10() +
#   labs(
#     title = "Distribution of Edge Weights (Tie Strength)",
#     x = "Number of Calls per Directed Tie (log-scale)",
#     y = "Frequency"
#   ) +
#   theme_minimal()

# Reciprocity ----------------------------------------------------------------

# Share of directed ties that are reciprocated. A value of 1 would mean that
# every tie from A to B is matched by a tie from B to A.
r_global <- igraph::reciprocity(g_dir)

cat("Overall graph reciprocity in directed graph:", round(r_global, 2))

# Summaries of the user-level measures ---------------------------------------

# Network measures

summ_sn_totaldegree <- round(summary(user_network_metrics$sn_totaldegree), 2)

cat("Summary of SN: total-degree in undirected graph:\n\n")
print(summ_sn_totaldegree)

summ_sn_indegree <- round(summary(user_network_metrics$sn_indegree), 2)

cat("\nSummary of SN: in-degree in undirected graph:\n\n")
print(summ_sn_indegree)

summ_sn_outdegree <- round(summary(user_network_metrics$sn_outdegree), 2)

cat("\nSummary of SN: out-degree in undirected graph:\n\n")
print(summ_sn_outdegree)

summ_sn_betweenness <- round(summary(user_network_metrics$sn_betweenness), 2)

cat("\nSummary of SN: betweenness in undirected graph:\n\n")
print(summ_sn_betweenness)

summ_sn_closeness <- round(summary(user_network_metrics$sn_closeness), 3)

cat("\nSummary of SN: closeness in undirected graph:\n\n")
print(summ_sn_closeness)

summ_sn_clustering <- round(summary(user_network_metrics$sn_clustering), 3)

cat("\nSummary of SN: clustering in undirected graph:\n\n")
print(summ_sn_clustering)

summ_sn_reciprocity <- round(summary(user_network_metrics$sn_reciprocity), 2)

cat("\nSummary of SN: reciprocity in undirected graph:\n\n")
print(summ_sn_reciprocity)

glimpse(user_network_metrics)

# Mobility measures

summ_sm_rog_total <- round(summary(user_mobility_metrics$sm_rog_total), 2)

cat("Summary of SM: RoG in undirected graph:\n\n")
print(summ_sm_rog_total)

summ_sm_entr_total <- round(summary(user_mobility_metrics$sm_entr_total), 2)

cat("\nSummary of SM: entropy in undirected graph:\n\n")
print(summ_sm_entr_total)

summ_sm_nul_total <- round(summary(user_mobility_metrics$sm_nul_total), 2)

cat("\nSummary of SM: NuL in undirected graph:\n\n")
print(summ_sm_nul_total)

summ_sm_active_days_total <- round(summary(user_mobility_metrics$sm_active_days_total), 2)

cat("\nSummary of SM: number of active days in total in undirected graph:\n\n")
print(summ_sm_active_days_total)

glimpse(user_mobility_metrics)

# Settlement context

summ_cg_domset <- round(summary(user_contextual_metrics$cg_pct_big_city), 2)

cat("Summary of CG: domset in undirected graph:\n\n")
print(summ_cg_domset)

glimpse(user_contextual_metrics)
