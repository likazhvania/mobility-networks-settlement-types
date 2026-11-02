# =============================================================================
# 04  Network construction
#
# Builds the communication network from the filtered communication events, in
# two forms:
#   - directed (g_dir): keeps who initiated the contact (sender -> receiver),
#     needed for in-degree, out-degree and reciprocity
#   - undirected (g_und): one tie per pair of users, whoever initiated
# Tie weight is the number of communication events.
#
# Input:  edges_pass from script 03
# Output: directed_edges_weighted, undirected_edges_weighted, g_dir, g_und;
#         both graphs are also exported as GraphML for Gephi
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(igraph)

# edges_pass is taken from script 03 in the same R session; to start from a
# saved copy instead:
# edges_pass <- readRDS("data/final_edge_list.rds")

# Directed graph -------------------------------------------------------------

## Directed edge list with weights -------------------------------------------

# One row per sender -> receiver pair; weight is the number of communication
# events from the sender to the receiver.
directed_edges_weighted <- edges_pass %>%
  group_by(caller, callee) %>%
  summarise(
    weight = n(), # number of communication events
    .groups = "drop"
  )

# First rows of the edge list
head(directed_edges_weighted)

## Graph object --------------------------------------------------------------

# The first two columns (caller and callee: sender and receiver) become the
# source and target of each edge; the remaining column (weight) becomes an
# edge attribute.
g_dir <- graph_from_data_frame(
  d = directed_edges_weighted,
  directed = TRUE
)

# --- Check the graph ---
print(g_dir)
cat("\nIs the graph directed?", is_directed(g_dir))
cat("\nIs the graph weighted?", is_weighted(g_dir))

# Undirected graph -----------------------------------------------------------

## Undirected edge list ------------------------------------------------------

# Built by collapsing the directed edge list: A -> B and B -> A become one
# pair (u, v), sorted so that u is the smaller ID, and the weight is the sum
# of communication events in both directions.
undirected_edges_weighted <- directed_edges_weighted %>%
  mutate(
    u = pmin(caller, callee),
    v = pmax(caller, callee)
  ) %>% 
  group_by(u, v) %>%
  summarise(weight = sum(weight), .groups = "drop")

# First rows of the edge list
head(undirected_edges_weighted)

## Graph object --------------------------------------------------------------

g_und <- graph_from_data_frame(
  d = undirected_edges_weighted,
  directed = FALSE
)

# --- Check the graph ---
print(g_und)
cat("\nIs the graph directed?", is_directed(g_und))

# Summary and export ---------------------------------------------------------

# --- Summary ---
cat("--- Directed Graph Summary ---\n")
summary(g_dir)

cat("\n--- Undirected Graph Summary ---\n")
summary(g_und)

# --- Optional: save the graphs for a new session ---
#saveRDS(g_dir, "data/graph_directed.rds")
#saveRDS(g_und, "data/graph_undirected.rds")

## Export for Gephi ----------------------------------------------------------

# GraphML keeps the edge weights and opens directly in Gephi.

# Export the directed graph
write_graph(
  g_dir,
  "network_directed.graphml",
  format = "graphml"
)

# Export the undirected graph
write_graph(
  g_und,
  "network_undirected.graphml",
  format = "graphml"
)

cat("\nGraph objects have been saved as .graphml files for use in Gephi.")
