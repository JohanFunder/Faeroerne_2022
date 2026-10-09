# Aggregation and analysis of ringing data (Ringm_samlet_D)
# Requires the data frame `Ringm_samlet_D` (catching method "D") to exist, with
# the columns SpeciesReported, Lokalitet, DataType, Dato ("dd-mm-yyyy") and Validering.

library(dplyr)
library(lubridate)

#------------------------------------------------------------------------------
# 1. Species overview
#------------------------------------------------------------------------------
# Unique species and their number
unique_species   <- unique(Ringm_samlet_D$SpeciesReported)
n_unique_species <- length(unique_species)
unique_species
n_unique_species

# Observations per species
table(Ringm_samlet_D$SpeciesReported)

#------------------------------------------------------------------------------
# 2. Counts (ringings + recoveries)
#------------------------------------------------------------------------------
# By data type
count_by_type <- Ringm_samlet_D %>%
  group_by(DataType) %>%
  summarise(count = n())

# By species and data type
count_by_species_type <- Ringm_samlet_D %>%
  group_by(SpeciesReported, DataType) %>%
  summarise(count = n(), .groups = "drop_last")

# By species, location and data type
count_by_species_location_type <- Ringm_samlet_D %>%
  group_by(SpeciesReported, Lokalitet, DataType) %>%
  summarise(count = n(), .groups = "drop_last")

# Total count by location
count_by_location <- Ringm_samlet_D %>%
  group_by(Lokalitet) %>%
  summarise(count = n())

#------------------------------------------------------------------------------
# 3. Capture days
#------------------------------------------------------------------------------
# Number of capture days per species
days_per_species <- Ringm_samlet_D %>%
  group_by(SpeciesReported) %>%
  summarise(days_per_species = n_distinct(Dato))

# Number of unique species per capture day
species_per_day <- Ringm_samlet_D %>%
  group_by(Dato) %>%
  summarise(species_per_day = n_distinct(SpeciesReported))

# Total number of birds per capture day
birds_per_day <- Ringm_samlet_D %>%
  group_by(Dato) %>%
  summarise(Validering = n())

# Birds per species per capture day (named species and dates)
birds_per_species_day <- Ringm_samlet_D %>%
  group_by(SpeciesReported, Dato) %>%
  summarise(Validering = n(), .groups = "drop")

# Number of capture dates per species (from the table above)
dates_per_species <- birds_per_species_day %>%
  group_by(SpeciesReported) %>%
  summarise(Dato = n())

#------------------------------------------------------------------------------
# 4. Species and locations
#------------------------------------------------------------------------------
# Species by location
species_by_location <- Ringm_samlet_D %>%
  group_by(SpeciesReported, Lokalitet) %>%
  summarise(no_species_location = n_distinct(SpeciesReported), .groups = "drop_last")

# Unique species per capture date
unique_dates <- Ringm_samlet_D %>%
  group_by(Dato) %>%
  summarise(Fangstdatoer = n_distinct(SpeciesReported))

# Unique locations per capture date
locations_per_date <- Ringm_samlet_D %>%
  group_by(Dato) %>%
  summarise(Lokaliteter = n_distinct(Lokalitet))

#------------------------------------------------------------------------------
# 5. Monthly aggregation of capture dates
#------------------------------------------------------------------------------
Ringm_samlet_D$Date <- as.Date(Ringm_samlet_D$Dato, "%d-%m-%Y")
str(Ringm_samlet_D)

# Number of distinct capture dates per month
capture_days_per_month <- Ringm_samlet_D %>%
  distinct(Date) %>%
  mutate(month = floor_date(Date, "month")) %>%
  count(month)
