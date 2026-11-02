# =============================================================================
# 05  Social network measures
#
# Calculates the network measures for every user in the directed graph:
# in-, out- and total degree, betweenness, closeness, local clustering and
# node-level reciprocity. Betweenness and closeness are calculated on the
# largest weakly connected component and are not normalised.
#
# Input:  g_dir from script 04
# Output: user_network_metrics - one row per user, one column per measure
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(igraph)

# g_dir is taken from script 04 in the same R session; to start from a saved
# copy instead:
# g_dir <- readRDS("data/graph_directed.rds")

# Largest connected component ------------------------------------------------

# Path-based measures (betweenness, closeness) are only meaningful within a
# connected part of the network. The network contains several disconnected
# components, so these two measures are calculated on the largest weakly
# connected component.

# Weakly connected components
components_list <- components(g_dir, mode = "weak")
giant_component_id <- which.max(components_list$csize)

# Subgraph of the largest component
g_dir_gc <- induced_subgraph(g_dir, which(components_list$membership == giant_component_id))

# Size of the full graph and of the component
cat("Original graph has", gorder(g_dir), "nodes.\n")
cat("Giant component has", gorder(g_dir_gc), "nodes.\n")

# Node-level measures --------------------------------------------------------

## Degree --------------------------------------------------------------------

# Number of direct contacts: incoming (in-degree), outgoing (out-degree) and
# both together (total degree).
degree_metrics <- tibble(
  user_id = V(g_dir)$name,
  sn_indegree = degree(g_dir, mode = "in"),
  sn_outdegree = degree(g_dir, mode = "out"),
  sn_totaldegree = degree(g_dir, mode = "all")
)

head(degree_metrics)

saveRDS(degree_metrics, "data/degree_metrics.rds")

## Betweenness ---------------------------------------------------------------

# How often a user lies on the shortest paths between others - a measure of
# brokerage. Calculated on the largest component, unnormalised. Slow on a
# network of this size.
betweenness_scores <- betweenness(g_dir_gc, directed = TRUE)

# Allow scientific notation when printing
options(scipen = 0)

# Users outside the largest component are added later with a value of 0
betweenness_metrics <- tibble(
  user_id = V(g_dir_gc)$name,
  sn_betweenness = betweenness_scores
)

# Print in scientific notation
head(betweenness_metrics)

# Back to fixed notation
options(scipen = 999)

head(betweenness_metrics)

saveRDS(betweenness_metrics, "data/betweenness_metrics.rds")

## Closeness -----------------------------------------------------------------

# How close a user is to everyone else in the network. Calculated on the
# largest component, using incoming paths (mode = "in"): how easily the user
# can be reached by others.
closeness_scores <- closeness(g_dir_gc, mode = "in")

closeness_metrics <- tibble(
  user_id = V(g_dir_gc)$name,
  sn_closeness = closeness_scores
)

head(closeness_metrics)

## Local clustering ----------------------------------------------------------

# Share of a user's contacts who are also in contact with each other.
clustering_scores <- transitivity(g_dir, type = "local")

clustering_metrics <- tibble(
  user_id = V(g_dir)$name,
  sn_clustering = clustering_scores
)

# Users with fewer than two contacts have no defined value; set to 0
clustering_metrics <- clustering_metrics %>%
  mutate(sn_clustering = ifelse(is.nan(sn_clustering), 0, sn_clustering))

head(clustering_metrics)

## Node-level reciprocity ----------------------------------------------------

# Mutual outgoing ties divided by total degree (in + out). A reciprocated
# contact counts once in the numerator and twice in the denominator (one edge
# in each direction), so values range from 0 to 0.5; 0.5 means every contact
# is reciprocated.

# --- Mutual edges ---
mutual_edges <- E(g_dir)[is.mutual(g_dir)]                        # edge indices in mutual dyads
mutual_tails <- ends(g_dir, mutual_edges, names = FALSE)[, 1]     # tail (source) vertices

# --- Counts per user ---
out_deg <- degree(g_dir, mode = "out")
in_deg <- degree(g_dir, mode = "in")
tot_deg <- degree(g_dir, mode = "all")

mut_out <- tabulate(mutual_tails, nbins = vcount(g_dir))          # mutual outgoing ties

# --- Ratio ---
sn_reciprocity <- ifelse(tot_deg > 0, mut_out / tot_deg, 0)       # 0 for isolates

reciprocity_metrics <- tibble(
  user_id         = V(g_dir)$name,
  sn_reciprocity  = sn_reciprocity
)

head(reciprocity_metrics)

# Combine the measures -------------------------------------------------------

metrics_list <- list(
  degree_metrics,
  betweenness_metrics,
  closeness_metrics,
  clustering_metrics,
  reciprocity_metrics
)

# Join all tables by user_id
user_network_metrics <- metrics_list %>%
  reduce(full_join, by = "user_id")

# Users outside the largest component get 0 for betweenness and closeness
user_network_metrics <- user_network_metrics %>%
  mutate(across(everything(), ~replace_na(., 0)))

# --- Check ---
cat("Dimensions of the final user metrics dataset:", dim(user_network_metrics), "\n")
glimpse(user_network_metrics)

# Optional: save for a new session -------------------------------------------

# saveRDS(user_network_metrics, "data/user_network_metrics.rds")
