# =============================================================================
# Development script - not part of the final analysis
#
# Settlement-level networks built from the aggregated ties, exported for
# Gephi.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)

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

glimpse(cluster_users)

domsettl_users <- cluster_users %>%
  group_by(settlement, settlement_hierarchy) %>%
  summarise(n_users = n()) %>%
  ungroup()

glimpse(domsettl_users)

sum(domsettl_users$n_users)

glimpse(cluster_users)

cluster_users <- cluster_users %>%
  select(user_id) %>%
  mutate(caller = user_id)

cluster_users_settl <- cluster_users %>%
  left_join(users_settl_dom, by = "caller")

cluster_users_settl <- cluster_users_settl %>%
  select(-caller)

cluster_users_settl <- cluster_users_settl %>%
  rename(user_id = user_id.x) %>%
  select(-user_id.y)

glimpse(cluster_users_settl)

users_settl_dom <- users_settl_dom %>%
  mutate(user_id = caller) %>%
  select(-caller)

glimpse(users_settl_dom)

# Cluster networks
cluster_users_network <- undirected_edges_weighted %>%
  filter(u %in% cluster_users_settl$user_id | v %in% cluster_users_settl$user_id)

# Join the cluster networks with "users_settl_dom" to assign the dominant settlements to callers and callees
cluster_users_network <- cluster_users_network %>%
  mutate(user_id = as.character(u)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(caller = user_id,
         caller_domset = settlement) %>%
  select(-c(settlement, user_id)) %>%

  mutate(user_id = as.character(v)) %>%
  left_join(users_settl_dom, by = "user_id") %>%
  mutate(callee = user_id,
         callee_domset = settlement) %>%
  select(-c(settlement, user_id))

cluster_users_network <- cluster_users_network %>%
  select(-c(caller_settl_hier, callee_settl_hier))

netw <- cluster_users_network %>%
  group_by(caller_domset, callee_domset) %>%
  summarise(n_ties = n())

netw <- netw %>%
  rename(Source = caller_domset,
         Target = callee_domset,
         weight = n_ties)

glimpse(netw)

glimpse(domsettl_users)

domsettl_users <- domsettl_users %>%
  mutate(ID = settlement)

glimpse(domsettl_users)

write_csv(domsettl_users,     "data/clusters/domsettl_users.csv")
write_csv(netw,     "data/clusters/netw.csv")
