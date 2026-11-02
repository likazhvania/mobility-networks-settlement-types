# =============================================================================
# 02  CDR data cleaning
#
# Reads the raw call detail records, fixes data types, assigns each
# communication event to the settlement unit of its serving cell tower, and
# removes self-loops.
#
# The extract contains outbound communication events - voice calls and text
# messages, not distinguished by type. Throughout the code, "call" refers to
# any such communication event.
#
# Input:  data.csv - raw CDR extract (not public); settlements from script 01
# Output: mob_data - cleaned event-level data with settlement, municipality
#         and county attached; mob_data_bu - an unchanged copy of it
# =============================================================================

# Setup and data loading -----------------------------------------------------

# tidyverse: general data manipulation and plotting (dplyr, ggplot2, etc.)
# sf: for handling spatial vector data (Simple Features)
# data.table: for fast and efficient data reading and aggregation

library(tidyverse)
library(sf)
library(data.table)

# --- Raw data ---
# fread() from data.table is used for its speed with large files.
mob_data <- fread('.../data/data.csv') %>%  # path to the raw CDR extract
  as_tibble() # Convert to tibble for easier viewing and manipulation

# Cleaning and type conversion -----------------------------------------------

# --- Rename the core columns ---
# Sender, receiver and time of each communication event get readable names.
mob_data <- mob_data %>% 
  rename(
    caller = op_pos_usr_id,
    callee = op_pos_usr_id_rec,
    timestamp = pos_time
  )

# --- Coordinates to numeric ---
# 'lon' and 'lat' are read as text and use commas as decimal marks. In this
# extract the two columns are swapped, so they are reassigned here.
mob_data <- mob_data %>% 
  mutate(
    # 'lat' holds the longitude and 'lon' the latitude.
    longitude = as.numeric(gsub(',', '.', lat)), # commas to decimal points
    latitude = as.numeric(gsub(',', '.', lon))
  )

# --- Timestamp to date-time ---
mob_data <- mob_data %>% 
  mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S"))

# --- Missing values after conversion ---
sapply(mob_data, function(x) sum(is.na(x)))

# --- Drop communication events missing any essential field ---
mob_data <- mob_data %>% 
  drop_na(caller, callee, timestamp, longitude, latitude)

# Attach settlements ---------------------------------------------------------

# Each communication event is placed in the settlement unit that contains the
# coordinates of its serving cell tower, which adds the settlement,
# municipality, county and settlement type.

# --- Events as a spatial object ---
# The coordinates are in WGS84 (EPSG:4326).
mob_data_sf <- st_as_sf(mob_data, coords = c('longitude', 'latitude'), crs = 4326)

# --- Project to L-EST97 (EPSG:3301) ---
# Same metric system as the settlement layer, needed for joins and distances.
mob_data_sf <- st_transform(mob_data_sf, crs = 3301)

# --- Spatial join: settlement attributes for every communication event ---
joined_data <- st_join(mob_data_sf, settlements,
                       join = st_intersects,
                       left = TRUE)

# --- Back to a plain table ---
# Projected coordinates become x_coord / y_coord, unneeded EHAK code columns
# are dropped, and the Estonian unit names are renamed.
joined_data <- joined_data %>% 
  mutate(
    longitude = as.numeric(gsub(',', '.', lat)), # WGS84 coordinates restored as columns
    latitude = as.numeric(gsub(',', '.', lon)),

    x_coord = st_coordinates(.)[,1],
    y_coord = st_coordinates(.)[,2]
  ) %>% 
  st_drop_geometry() %>% # Return to a standard data frame
  select(-c(AKOOD, TYYP, OKOOD, MKOOD)) %>% # EHAK code columns not needed
  rename( # ANIMI, ONIMI, MNIMI: settlement, municipality, county
    settlement = ANIMI,
    municipality = ONIMI,
    county = MNIMI
  )

# The joined table replaces the original.
mob_data <- joined_data

# Self-loops and backup ------------------------------------------------------

# --- Remove self-loops ---
# Events where sender and receiver are the same user are not social
# interactions.
initial_rows <- nrow(mob_data)
mob_data <- mob_data %>% 
  filter(caller != callee)
print(paste("Removed", initial_rows - nrow(mob_data), "self-loop calls."))

# --- Backup ---
# An unchanged copy of the cleaned event-level data, used later by the
# cell-tower density check (script 18).
mob_data_bu <- mob_data

# --- Structure of the cleaned data ---
glimpse(mob_data)
