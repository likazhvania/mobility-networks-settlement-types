# =============================================================================
# Development script - not part of the final analysis
#
# Users of each cluster by dominant municipality.
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
callers_muni <- edges_pass %>%
  group_by(caller, municipality) %>%
  summarise(n_calls = n()) %>%
  ungroup()

# Choose ONE dominant municipality per caller:
callers_muni_dom <- callers_muni %>%
  arrange(caller, desc(n_calls), municipality) %>%
  group_by(caller) %>%
  slice(1) %>%
  ungroup() %>%
  select(caller, municipality)

cluster_1_users <- cluster_1_users %>%
  mutate(caller = user_id)

cluster_2_users <- cluster_2_users %>%
  mutate(caller = user_id)

cluster_3_users <- cluster_3_users %>%
  mutate(caller = user_id)

cluster_4_users <- cluster_4_users %>%
  mutate(caller = user_id)

cluster_1_users_joined <- cluster_1_users %>%
  left_join(callers_muni_dom, by = "caller")

cluster_2_users_joined <- cluster_2_users %>%
  left_join(callers_muni_dom, by = "caller")

cluster_3_users_joined <- cluster_3_users %>%
  left_join(callers_muni_dom, by = "caller")

cluster_4_users_joined <- cluster_4_users %>%
  left_join(callers_muni_dom, by = "caller")

# Group by municipalities for each cluster

cluster_1_muni <- cluster_1_users_joined %>%
  group_by(municipality) %>%
  summarise(n_samples = n()) %>%
  ungroup()

cluster_2_muni <- cluster_2_users_joined %>%
  group_by(municipality) %>%
  summarise(n_samples = n()) %>%
  ungroup()

cluster_3_muni <- cluster_3_users_joined %>%
  group_by(municipality) %>%
  summarise(n_samples = n()) %>%
  ungroup()

cluster_4_muni <- cluster_4_users_joined %>%
  group_by(municipality) %>%
  summarise(n_samples = n()) %>%
  ungroup()

write_csv(cluster_1_muni, "data/cluster_1_muni.csv")
write_csv(cluster_2_muni, "data/cluster_2_muni.csv")
write_csv(cluster_3_muni, "data/cluster_3_muni.csv")
write_csv(cluster_4_muni, "data/cluster_4_muni.csv")
