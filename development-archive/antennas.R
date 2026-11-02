# =============================================================================
# Development script - not part of the final analysis
#
# Exploration of the cell tower data: events per site and temporal patterns.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Reading the data of antennas -----------------------------------------------

# In this chapter, we will work on antennas.

# Load required libraries
library(tidyverse)

#
options(scipen = 999)

# aggregate by antennas (site)
antennas_summary <- mob_data %>% 
  group_by(site_id, settlement, municipality, county, x, y) %>% 
  summarise(
    n_calls = n(), # total call volume per antenna
    first_call = min(timestamp, na.rm = TRUE), # earliest call on record
    last_call = max(timestamp, na.rm = TRUE) # latest call in record
  )

# plot antennas
antennas_plot <- 
  ggplot(antennas_summary, aes(x, y, size = n_calls)) + 
  geom_point()

antennas_plot

# Temporal analysis at the antenna level -------------------------------------

# Time series patterns

antennas_time <- mob_data %>% 
  mutate(hour = lubridate::hour(timestamp), 
         #hour = floor_date(timestamp, 'hour')
  ) %>% 
  group_by(site_id, hour) %>% 
  summarise(n_calls = n())

# 'floor_date' takes a date_time object and rounds it down to the nearest integer value of the specified time unit. Users can specify whether to round down to the nearest second, minute, hour, day, week, month, or year.

# Here it is round down to the nearest hour. However it still has stamptime format.
antennas_time <- antennas_time %>% 
  mutate(hour = lubridate::hour(hour)) %>% 
  group_by(site_id, hour) %>% 
  summarise(n_calls = sum(n_calls))

# plot trends
ggplot(antennas_time, aes(hour, n_calls, color = as.factor(site_id))) + 
  geom_path(alpha=0.2) + 
  scale_x_continuous(breaks = c(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24
  )) + 
  labs(title = 'Hourly Call Volume per Antenna') + 
  theme(
    legend.position = 'none'
  )

# save antennas data
write_csv(antennas_summary, 'data/antennas_summary.csv')
