# Stinknet Analyses by Valerie Ramirez
## Part 1: cleaning up rain data, ESCA 2023 database, pairing stations to 
# sample sites, and calculating annual rainfall totals by stations and sites

# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(bannerCommenter)

#Load datasets
updated_rainfall <- read_excel("Vr_Copy of Cleaned_Rainfall (Official).xlsx")

write.csv(
  updated_rainfall,
  "Cleaned_Rainfall_Official.csv",
  row.names = FALSE
)

names(updated_rainfall)
head(updated_rainfall)
str(updated_rainfall)

#Remove trailing .00s from from station column names
names(updated_rainfall) <- sub("\\.0$", "", names(updated_rainfall))

# Convert Date to Date format and to numeric
updated_rainfall <- updated_rainfall %>%
  mutate(
    across(-Date, ~ as.numeric(trimws(as.character(.x))))
  ) %>%
  mutate(Date = as.Date(Date))

# Convert wide to long format
rain_long <- updated_rainfall %>%
  pivot_longer(
    cols = -Date,
    names_to = "station_id",
    values_to = "rainfall"
  ) %>%
  mutate(
    station_id = as.character(station_id)
  )

# Check result, should be columns:192 and rows: 9,497
glimpse(updated_rainfall)
glimpse(rain_long)
summary(rain_long$rainfall)

#Check number of NAs, should be 195371
sum(is.na(updated_rainfall))

# Converting rainfall from inches to centimeters
# Keep orignal long format table and create new tables for long format in centimeters
rain_long_cm <- rain_long %>%
  mutate(rainfall_cm = rainfall * 2.54) %>%
  select(Date, station_id, rainfall_cm)

#Transforming original wide rainfall data (updated_rainfall) to cm (rain_clean_cm)
rain_clean_cm <- updated_rainfall %>%
  mutate(across(-Date, ~ .x * 2.54))

# Check NA count in rainfall cells only
sum(is.na(updated_rainfall[, -1]))

# Rebuild long table from current cleaned wide table
rain_long <- updated_rainfall %>%
  pivot_longer(
    cols = -Date,
    names_to = "station_id",
    values_to = "rainfall"
  ) %>%
  mutate(station_id = as.character(station_id))

# Rebuild cm table from current long table
rain_long_cm <- rain_long %>%
  mutate(rainfall_cm = rainfall * 2.54) %>%
  select(Date, station_id, rainfall_cm)

# Recheck counts
sum(is.na(rain_long$rainfall))
sum(is.na(rain_long_cm$rainfall_cm))

# A separate site lookup table was created to link ESCA site codes with their 
# associated FCDMC rain station IDs. The lookup table is standalone table for 
# cross-referencing between the ESCA and rainfall datasets
# Adding look-up table with ESCA site codes and fcdmc station IDs
## Make sure X13 is corrected (from S13 to X13) before importing
## Make sure V14 is corrected, only has 2 stations and V15 has 2
## Make sure U17, U18, and V17 site codes are omitted from the look up table, 
# sample sites and its data are not included on the ESCA database, rain data is 
# available
##Make sure for site code AE07, rain station 50827 is deleted

site_station_lookup_raw <- read_excel("Site_LookUp_Table.xlsx")

#Change both to characters
site_station_lookup_raw <- site_station_lookup_raw %>%
  mutate(
    ESCA_site_code = as.character(ESCA_site_code),
    fcdmc_station_id = as.character(fcdmc_station_id)
  )

#Change format to one row per ESCA site code and rename to match ESCA database 
#site_code column
##If site only has one fcdmc station it will be listed as NA

site_station_lookup <- site_station_lookup_raw %>%
  group_by(ESCA_site_code) %>%
  mutate(station_num = row_number()) %>%
  pivot_wider(
    names_from = station_num,
    values_from = fcdmc_station_id,
    names_prefix = "station"
  ) %>%
  distinct() %>%
  ungroup() %>%
  rename(site_code = ESCA_site_code)

#Import ESCA database, use most updated with 2023 land use updates
ESCA_2023<- read_excel("VR_DB_ESCA_2023LU_elevation.xlsx")

#Check site codes that are in ESCA_2023 and site_station_lookup, should be 203
sum(site_station_lookup$site_code %in% ESCA_2023$site_code)

#Find any unmatched site codes to make sure
site_station_lookup %>%
  filter(!site_code %in% ESCA_2023$site_code)

#Convert site lookup table to long format
site_station_long <- site_station_lookup %>%
  pivot_longer(
    cols = c(station1, station2),
    names_to = "station_label",
    values_to = "station_id"
  ) %>%
  filter(!is.na(station_id)) %>%
  mutate(station_id = as.character(station_id))

##----------------------------------------------------------------
# Prepare rainfall data for stations used by ESCA sites
##----------------------------------------------------------------

used_stations <- site_station_lookup %>%
  select(station1, station2) %>%
  pivot_longer(
    cols = everything(),
    values_to = "station_id"
  ) %>%
  filter(!is.na(station_id)) %>%
  distinct(station_id) %>%
  pull(station_id) %>%
  as.character()
# Pull all station IDs used in the site lookup table
used_stations <- site_station_lookup %>%
  select(station1, station2) %>%
  pivot_longer(
    cols = everything(),
    values_to = "station_id"
  ) %>%
  filter(!is.na(station_id)) %>%
  distinct(station_id) %>%
  pull(station_id) %>%
  as.character()

# Check number of unique stations used
length(used_stations)

# Step 2. Create a wide rainfall dataset of containing only used stations

# Original units in inches
rain_used_stations <- updated_rainfall %>%
  select(Date, all_of(used_stations))

# Convert to centimeters
rain_used_stations_cm <- rain_clean_cm %>%
  select(Date, all_of(used_stations))


# Step 3. Create long-format datasets containing only used stations

# Original units in inches
rain_long_used <- rain_long %>%
  filter(station_id %in% used_stations)

# Convert unit to centimeters
rain_long_used_cm <- rain_long_cm %>%
  filter(station_id %in% used_stations)

# Step 4. Save filtered rainfall datasets
write.csv(
  rain_used_stations,
  "rain_used_stations.csv",
  row.names = FALSE
)

write.csv(
  rain_used_stations_cm,
  "rain_used_stations_cm.csv",
  row.names = FALSE
)

##----------------------------------------------------------------
# Calculate annual rainfall by station and site
##----------------------------------------------------------------

# Step 5. Calculate annual rainfall totals for each station
station_year_rain <- rain_long_used_cm %>%
  mutate(year = year(Date)) %>%
  group_by(station_id, year) %>%
  summarise(
    annual_rainfall_cm = if_else(
      all(is.na(rainfall_cm)),
      NA_real_,
      sum(rainfall_cm, na.rm = TRUE)
    ),
    .groups = "drop"
  )
# Step 6. Calculate annual rainfall for each ESCA sample site
# Each site may be linked to one or two rainfall stations
# When two station values are available, their annual totals are averaged

site_year_rain <- site_station_long %>%
  left_join(
    station_year_rain,
    by = "station_id",
    relationship = "many-to-many"
  ) %>%
  group_by(site_code, year) %>%
  summarise(
    site_annual_rainfall_cm = if_else(
      all(is.na(annual_rainfall_cm)),
      NA_real_,
      mean(annual_rainfall_cm, na.rm = TRUE)
    ),
    n_stations_used = sum(!is.na(annual_rainfall_cm)),
    .groups = "drop"
  )


# Save site-level annual rainfall
write.csv(
  site_year_rain,
  "site_year_rain.csv",
  row.names = FALSE
)

# Check an individual site manually, example: AA09
site_station_lookup %>%
  filter(site_code == "AA09")

# Check annual rainfall totals for AA09 stations in 2000
station_year_rain %>%
  filter(
    station_id %in% c("76200", "61200"),
    year == 2000
  )

# Manually verify site rainfall
mean(c(26.4, 23.4))

# Compare with final site-year rainfall value
site_year_rain %>%
  filter(
    site_code == "AA09",
    year == 2000
  )


