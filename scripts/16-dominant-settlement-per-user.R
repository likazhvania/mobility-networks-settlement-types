# =============================================================================
# 16  Dominant settlement per user
#
# Finds, for every user, the single settlement from which they send most
# communication events, together with its settlement type. Tallinn and
# Kohtla-Järve are recorded by city district, so for their users the result is
# a district. Used by script 17 to split the big-city group into individual
# cities.
#
# This is based on single settlements, whereas cg_domset (script 07) is based
# on settlement types. The two can differ for users whose communication events
# are spread over several settlements.
#
# The code is taken unchanged from a development script:
# development-archive/23-visualisation-cluster-distributions-by-settlements.R
#
# Input:  edges_pass (script 03)
# Output: users_settl_dom - user_id, settlement, settlement_hierarchy
# =============================================================================

# Number of communication events per user and settlement
users_settl <- edges_pass %>%
  group_by(caller, settlement, settlement_hierarchy) %>%
  summarise(n_calls = n()) %>%
  ungroup()

# The settlement with most communication events; ties go to the first name
# alphabetically
users_settl_dom <- users_settl %>%
  arrange(caller, desc(n_calls), settlement, settlement_hierarchy) %>%
  group_by(caller) %>%
  slice(1) %>%
  ungroup() %>%
  select(caller, settlement, settlement_hierarchy)

# Same key name as in analysis_data
users_settl_dom <- users_settl_dom %>%
  mutate(user_id = caller) %>%
  select(-caller)

glimpse(users_settl_dom)
