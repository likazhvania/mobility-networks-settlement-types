# =============================================================================
# 17  Big-city robustness check
#
# The big-city group combines Tallinn with five much smaller cities. To check
# that the big-city results are not driven by Tallinn alone, big-city users
# are split by city and the medians of the main mobility and network
# measures are compared across the six cities.
#
# Results are reported in Table S19.
#
# Input:  users_settl_dom (script 16); analysis_data (script 08)
# Output: city_medians - medians per city
# =============================================================================

library(tidyverse)

# users_settl_dom: user_id, settlement, settlement_hierarchy
# analysis_data:   user_id and all measures

# Big-city settlement names as recorded in the data
users_settl_dom |>
  filter(settlement_hierarchy == "Urban: big city") |>
  count(settlement, sort = TRUE)

# City districts (linnaosa) are merged into their city. Tallinn and
# Kohtla-Järve are recorded by district in 'settlement'; the other big
# cities appear directly as "... linn".
big_city_lookup <- users_settl_dom %>%
  filter(settlement_hierarchy == "Urban: big city") %>%
  mutate(
    city = case_when(
      # Tallinn districts
      settlement %in% c("Haabersti linnaosa",
                        "Kesklinna linnaosa",
                        "Kristiine linnaosa",
                        "Lasnamäe linnaosa",
                        "Mustamäe linnaosa",
                        "Nõmme linnaosa",
                        "Pirita linnaosa",
                        "Põhja-Tallinna linnaosa"
      ) ~ "Tallinn",
      # Kohtla-Järve districts
      settlement %in% c("Ahtme linnaosa",
                        "Järve linnaosa",
                        "Kukruse linnaosa",
                        "Oru linnaosa",
                        "Sompa linnaosa") ~ "Kohtla-Järve",
      # Cities recorded directly
      settlement == "Tartu linn" ~ "Tartu",
      settlement == "Narva linn" ~ "Narva",
      settlement == "Pärnu linn" ~ "Pärnu",
      settlement == "Jõhvi linn" ~ "Jõhvi",
      TRUE ~ settlement  # unmatched names are kept and show up in the count below
    )
  )

# Every big-city user should fall into one of the six cities
big_city_lookup %>% count(city, sort = TRUE)

# 1. Big-city users and their city
big_city_users <- big_city_lookup %>%
  select(user_id, city)

# 2. Their measures from analysis_data; the inner join keeps users present in
#    both
big_city_data <- analysis_data %>%
  inner_join(big_city_users, by = "user_id")

# Users per city
big_city_data %>% count(city, name = "n_users") %>% arrange(desc(n_users))

# 3. Medians of the main mobility and network measures by city (Table S19)
city_medians <- big_city_data %>%
  group_by(city) %>%
  summarise(
    n_users = n(),
    across(
      c(sm_entr_total, sm_rog_total, sm_nul_total, sm_active_days_total,
        sn_totaldegree, sn_betweenness, sn_clustering, sn_reciprocity),
      \(x) median(x, na.rm=TRUE),
      .names = "med_{.col}"
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(n_users))

city_medians

# The same in long format, for comparing Tallinn with the other cities
# measure by measure
city_medians_long <- city_medians %>%
  pivot_longer(starts_with("med_"),
               names_to = "metric", values_to = "median") %>%
  arrange(metric, desc(median))

city_medians_long
