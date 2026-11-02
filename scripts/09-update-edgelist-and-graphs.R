# =============================================================================
# 09  Analysis graphs
#
# Rebuilds the graphs in two versions:
#   - full: every user in the filtered network
#   - induced: only ties between users in the final analysis dataset, so that
#     network and mobility measures refer to the same set of users
#
# Input:  edges_pass (script 03), directed_edges_weighted and
#         undirected_edges_weighted (04), analysis_data (08)
# Output: g_dir_all, g_dir_induced, g_und_all, g_und_induced
# =============================================================================

library(tidyverse)
library(igraph)

ids_all      <- unique(c(edges_pass$caller, edges_pass$callee))
ids_analysis <- analysis_data$user_id

# --- Full edge lists (as built in script 04) ---
dir_all <- directed_edges_weighted
und_all <- undirected_edges_weighted

# --- Induced edge lists: both endpoints in the analysis dataset ---
dir_induced <- directed_edges_weighted %>%
  dplyr::filter(caller %in% ids_analysis, callee %in% ids_analysis)

und_induced <- undirected_edges_weighted %>%
  dplyr::filter(u %in% ids_analysis, v %in% ids_analysis)

# Users and ties in each version
c(full_nodes = length(ids_all),
  full_dir_E = nrow(dir_all),
  full_und_E = nrow(und_all),
  analysis_nodes = length(ids_analysis),
  induced_dir_E = nrow(dir_induced),
  induced_und_E = nrow(und_induced))

# --- Graph objects ---
g_dir_all <- graph_from_data_frame(
  d = dir_all,
  directed = TRUE
)

# --- Check the graph ---
print(g_dir_all)
cat("\nIs the graph directed?", is_directed(g_dir_all))
cat("\nIs the graph weighted?", is_weighted(g_dir_all))

g_dir_induced <- graph_from_data_frame(
  d = dir_induced,
  directed = TRUE
)

# --- Check the graph ---
print(g_dir_induced)
cat("\nIs the graph directed?", is_directed(g_dir_induced))
cat("\nIs the graph weighted?", is_weighted(g_dir_induced))

g_und_all <- graph_from_data_frame(
  d = und_all,
  directed = FALSE
)

# --- Check the graph ---
print(g_und_all)
cat("\nIs the graph directed?", is_directed(g_und_all))

g_und_induced <- graph_from_data_frame(
  d = und_induced,
  directed = FALSE
)

# --- Check the graph ---
print(g_und_induced)
cat("\nIs the graph directed?", is_directed(g_und_induced))
