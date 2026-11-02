# =============================================================================
# Development script - not part of the final analysis
#
# Users and ties of each cluster by dominant settlement. Its dominant-
# settlement blocks are reused unchanged in script 16.
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
  group_by(caller, settlement, settlement_hierarchy) %>%
  summarise(n_calls = n()) %>%
  ungroup()

# Choose ONE dominant settlement per caller:
users_settl_dom <- users_settl %>%
  arrange(caller, desc(n_calls), settlement, settlement_hierarchy) %>%
  group_by(caller) %>%
  slice(1) %>%
  ungroup() %>%
  select(caller, settlement, settlement_hierarchy)

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
cluster_1_network <- cluster_1_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  # rename 'settlement_hierarchy.x' and 'settlement_hierarchy.y'
  # order columns
  rename(caller_domset_hier = settlement_hierarchy.x,
         callee_domset_hier = settlement_hierarchy.y) %>%
  select(u, v, weight, caller, caller_domset, caller_domset_hier, callee, callee_domset, callee_domset_hier)

cluster_2_network <- cluster_2_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  # rename 'settlement_hierarchy.x' and 'settlement_hierarchy.y'
  # order columns
  rename(caller_domset_hier = settlement_hierarchy.x,
         callee_domset_hier = settlement_hierarchy.y) %>%
  select(u, v, weight, caller, caller_domset, caller_domset_hier, callee, callee_domset, callee_domset_hier)

cluster_3_network <- cluster_3_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  # rename 'settlement_hierarchy.x' and 'settlement_hierarchy.y'
  # order columns
  rename(caller_domset_hier = settlement_hierarchy.x,
         callee_domset_hier = settlement_hierarchy.y) %>%
  select(u, v, weight, caller, caller_domset, caller_domset_hier, callee, callee_domset, callee_domset_hier)

cluster_4_network <- cluster_4_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  # rename 'settlement_hierarchy.x' and 'settlement_hierarchy.y'
  # order columns
  rename(caller_domset_hier = settlement_hierarchy.x,
         callee_domset_hier = settlement_hierarchy.y) %>%
  select(u, v, weight, caller, caller_domset, caller_domset_hier, callee, callee_domset, callee_domset_hier)

glimpse(cluster_1_network)

# Create a networks of settlements by the aggregated ties
cluster_1_network_settl <- cluster_1_network %>%
  group_by(caller_domset, caller_domset_hier, callee_domset, callee_domset_hier) %>%
  summarise(n_ties = n(),
            n_calls = sum(weight)) %>%
  ungroup()

cluster_2_network_settl <- cluster_2_network %>%
  group_by(caller_domset, caller_domset_hier, callee_domset, callee_domset_hier) %>%
  summarise(n_ties = n(),
            n_calls = sum(weight)) %>%
  ungroup()

cluster_3_network_settl <- cluster_3_network %>%
  group_by(caller_domset, caller_domset_hier, callee_domset, callee_domset_hier) %>%
  summarise(n_ties = n(),
            n_calls = sum(weight)) %>%
  ungroup()

cluster_4_network_settl <- cluster_4_network %>%
  group_by(caller_domset, caller_domset_hier, callee_domset, callee_domset_hier) %>%
  summarise(n_ties = n(),
            n_calls = sum(weight)) %>%
  ungroup()

glimpse(cluster_1_network_settl)

cluster_1_network_settl <- cluster_1_network_settl %>%
  mutate(Source = caller_domset,
         Target = callee_domset,
         weight = n_ties) %>%
  select(Source, Target, weight, caller_domset, caller_domset_hier, callee_domset, callee_domset_hier, n_ties, n_calls)

cluster_2_network_settl <- cluster_2_network_settl %>%
  mutate(Source = caller_domset,
         Target = callee_domset,
         weight = n_ties) %>%
  select(Source, Target, weight, caller_domset, caller_domset_hier, callee_domset, callee_domset_hier, n_ties, n_calls)

cluster_3_network_settl <- cluster_3_network_settl %>%
  mutate(Source = caller_domset,
         Target = callee_domset,
         weight = n_ties) %>%
  select(Source, Target, weight, caller_domset, caller_domset_hier, callee_domset, callee_domset_hier, n_ties, n_calls)

cluster_4_network_settl <- cluster_4_network_settl %>%
  mutate(Source = caller_domset,
         Target = callee_domset,
         weight = n_ties) %>%
  select(Source, Target, weight, caller_domset, caller_domset_hier, callee_domset, callee_domset_hier, n_ties, n_calls)

write_csv(cluster_1_network_settl,  "data/clusters/cluster_1_network_settl.csv")
write_csv(cluster_2_network_settl,  "data/clusters/cluster_2_network_settl.csv")
write_csv(cluster_3_network_settl,  "data/clusters/cluster_3_network_settl.csv")
write_csv(cluster_4_network_settl,  "data/clusters/cluster_4_network_settl.csv")

# Export for Gephi -----------------------------------------------------------

cluster_users <- DT %>%
  select(user_id, cluster)

users_settl_dom <- users_settl_dom %>%
  mutate(user_id = caller)

glimpse(users_settl_dom)

cluster_users <- cluster_users %>%
  left_join(users_settl_dom, by = "user_id")

glimpse(cluster_users)

cluster_users <- cluster_users %>%
  rename(ID = user_id) %>% 
  select(-caller)

glimpse(cluster_users)

write_csv(cluster_1_network,     "data/clusters/cluster_1_network.csv")
write_csv(cluster_2_network,     "data/clusters/cluster_2_network.csv")
write_csv(cluster_3_network,     "data/clusters/cluster_3_network.csv")
write_csv(cluster_4_network,     "data/clusters/cluster_4_network.csv")

undirected_edges_weighted_graph <- undirected_edges_weighted %>%
  rename(Source = u,
         Target = v)

#write_csv(directed_edges_weighted,        "data/clusters/directed_edges_weighted.csv")
write_csv(undirected_edges_weighted_graph, "data/clusters/undirected_edges_weighted.csv")
write_csv(cluster_1_users_settl,           "data/clusters/cluster_1_users_settl.csv")
write_csv(cluster_2_users_settl,           "data/clusters/cluster_2_users_settl.csv")
write_csv(cluster_3_users_settl,           "data/clusters/cluster_3_users_settl.csv")
write_csv(cluster_4_users_settl,           "data/clusters/cluster_4_users_settl.csv")

write_csv(users_settl_dom,     "data/clusters/users_settl_dom.csv")

write_csv(cluster_users,     "data/clusters/cluster_users.csv")
