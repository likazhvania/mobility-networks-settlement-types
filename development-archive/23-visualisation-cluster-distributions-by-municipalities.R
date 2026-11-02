# =============================================================================
# Development script - not part of the final analysis
#
# Users and ties of each cluster by dominant municipality.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)

# Clusters

# Total sample
clusters_total

# Cluster 1 : Profile 1
cluster_1

# Cluster 2 : Profile 2
cluster_2

# Cluster 3 : Profile 3
cluster_3

# Cluster 4 : Profile 4
cluster_4

# Select the user IDs

# Cluster 1 : Profile 1
cluster_1_users <- cluster_1 %>%
  select(user_id)

# Cluster 2 : Profile 2
cluster_2_users <- cluster_2 %>%
  select(user_id)

# Cluster 3 : Profile 3
cluster_3_users <- cluster_3 %>%
  select(user_id)

# Cluster 4 : Profile 4
cluster_4_users <- cluster_4 %>%
  select(user_id)

# --- Dominant municipality per caller (based on where they place most calls)
# Group by caller and municipality
users_settl <- edges_pass %>%
  group_by(caller, municipality) %>%
  summarise(n_calls = n()) %>%
  ungroup()

# Choose ONE dominant settlement per caller:
users_settl_dom <- users_settl %>%
  arrange(caller, desc(n_calls), municipality) %>%
  group_by(caller) %>%
  slice(1) %>%
  ungroup() %>%
  select(caller, municipality)

# --- Tie check ---

ties <- users_settl %>%
  group_by(caller) %>%
  summarise(max_n = max(n_calls), t = sum(n_calls == max_n), .groups = "drop") %>%
  filter(t > 1)

n_ties <- nrow(ties)
share_t <- n_ties / n_distinct(users_settl$caller)
cat("ties:", n_ties, " (", scales::percent(share_t), " of users)\n", sep = "")

cluster_1_users <- cluster_1_users %>%
  mutate(caller = user_id)

cluster_2_users <- cluster_2_users %>%
  mutate(caller = user_id)

cluster_3_users <- cluster_3_users %>%
  mutate(caller = user_id)

cluster_4_users <- cluster_4_users %>%
  mutate(caller = user_id)

cluster_1_users_settl <- cluster_1_users %>%
  left_join(users_settl_dom, by = "caller")

cluster_2_users_settl <- cluster_2_users %>%
  left_join(users_settl_dom, by = "caller")

cluster_3_users_settl <- cluster_3_users %>%
  left_join(users_settl_dom, by = "caller")

cluster_4_users_settl <- cluster_4_users %>%
  left_join(users_settl_dom, by = "caller")

cluster_1_users_settl <- cluster_1_users_settl %>%
  select(-caller)

cluster_2_users_settl <- cluster_2_users_settl %>%
  select(-caller)

cluster_3_users_settl <- cluster_3_users_settl %>%
  select(-caller)

cluster_4_users_settl <- cluster_4_users_settl %>%
  select(-caller)

glimpse(undirected_edges_weighted)

users_settl_dom <- users_settl_dom %>%
  mutate(user_id = caller) %>%
  select(-caller)

glimpse(users_settl_dom)

# Cluster networks
cluster_1_network <- undirected_edges_weighted %>%
  filter(u %in% cluster_1_users_settl$user_id | v %in% cluster_1_users_settl$user_id)

cluster_2_network <- undirected_edges_weighted %>%
  filter(u %in% cluster_2_users_settl$user_id | v %in% cluster_2_users_settl$user_id)

cluster_3_network <- undirected_edges_weighted %>%
  filter(u %in% cluster_3_users_settl$user_id | v %in% cluster_3_users_settl$user_id)

cluster_4_network <- undirected_edges_weighted %>%
  filter(u %in% cluster_4_users_settl$user_id | v %in% cluster_4_users_settl$user_id)

# Join the cluster networks with "users_settl_dom" to assign the dominant settlements to callers and callees
cluster_1_network_muni <- cluster_1_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_dommuni = municipality) %>%
  select(-c(municipality, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_dommuni = municipality) %>%
  select(-c(municipality, user_id))

cluster_2_network_muni <- cluster_2_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_dommuni = municipality) %>%
  select(-c(municipality, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_dommuni = municipality) %>%
  select(-c(municipality, user_id))

cluster_3_network_muni <- cluster_3_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_dommuni = municipality) %>%
  select(-c(municipality, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_dommuni = municipality) %>%
  select(-c(municipality, user_id))

cluster_4_network_muni <- cluster_4_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_dommuni = municipality) %>%
  select(-c(municipality, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_dommuni = municipality) %>%
  select(-c(municipality, user_id))

write_csv(cluster_1_network_muni,     "data/clusters/cluster_1_network_muni.csv")
write_csv(cluster_2_network_muni,     "data/clusters/cluster_2_network_muni.csv")
write_csv(cluster_3_network_muni,     "data/clusters/cluster_3_network_muni.csv")
write_csv(cluster_4_network_muni,     "data/clusters/cluster_4_network_muni.csv")

write_csv(directed_edges_weighted,   "data/clusters/directed_edges_weighted.csv")
write_csv(undirected_edges_weighted, "data/clusters/undirected_edges_weighted.csv")
write_csv(cluster_1_users_settl,     "data/clusters/cluster_1_users_settl.csv")
write_csv(cluster_2_users_settl,     "data/clusters/cluster_2_users_settl.csv")
write_csv(cluster_3_users_settl,     "data/clusters/cluster_3_users_settl.csv")
write_csv(cluster_4_users_settl,     "data/clusters/cluster_4_users_settl.csv")
