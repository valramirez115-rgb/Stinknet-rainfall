# Stinknet Analyses by Valerie Ramirez
# Part 3: Workflow for Quality Checks, Combining ESCA and rain average, graphs, 
# and other Visualizations

# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(bannerCommenter)

##------------------------------------------------
# Testing Summaries
# Quality Check the 3 rainfall periods
# 1. Basic Summaries
# 2. Any missing windows
# 3. Checking for outliers
# 4. Check with plots
#
##-----------------------------------------------

# Multi-year Annual Rainfall Period
summary(combined_site_annual_windows_wide)
colSums(is.na(combined_site_annual_windows_wide))

combined_site_annual_windows %>%
  arrange(desc(site_sample_window_avg_rainfall_cm))

combined_site_annual_windows %>%
  arrange(site_sample_window_avg_rainfall_cm)

combined_site_annual_windows %>%
  group_by(sample_year) %>%
  summarise(
    min_rain = min(site_sample_window_avg_rainfall_cm, na.rm = TRUE) ,
    max_rain = max(site_sample_window_avg_rainfall_cm, na.rm = TRUE),
    mean_rain = mean(site_sample_window_avg_rainfall_cm, na.rm = TRUE),
    median_rain = median(site_sample_window_avg_rainfall_cm, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(combined_site_annual_windows, aes(x = factor(sample_year), y = site_sample_window_avg_rainfall_cm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Average Annual Rainfall by Sample Period",
    x = "Sample Year",
    y = "Average Annual Rainfall (cm)"
  )


##-----------------------------------------------


# Multi-year Winter Rainfall Period
summary(combined_site_winter_windows_wide)
colSums(is.na(combined_site_winter_windows_wide))

combined_site_winter_windows %>%
  arrange(desc(site_winter_window_rainfall_cm))

combined_site_winter_windows %>%
  arrange(site_winter_window_rainfall_cm)

# The rain station used for sample sites F12, G12, and H13 were set up in August 
# 2005 therefore winter 2005 average is listed as NA
winter_rain_by_site %>%
  filter(site_code == "F12", winter_year %in% 2001:2005)

combined_site_winter_windows %>%
  group_by(sample_year) %>%
  summarise(
    min_rain = min(site_winter_window_rainfall_cm, na.rm = TRUE),
    max_rain = max(site_winter_window_rainfall_cm, na.rm = TRUE),
    mean_rain = mean(site_winter_window_rainfall_cm, na.rm = TRUE),
    median_rain = median(site_winter_window_rainfall_cm, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(
  combined_site_winter_windows,
  aes(
    x = factor(sample_year),
    y = site_winter_window_rainfall_cm
  )
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Average Winter Rainfall by Sample Period",
    x = "Sample year",
    y = "Average winter rainfall (cm)"
  )



##-----------------------------------------------


# Recent Winter Rainfall Period
summary(recent_winter_rain_wide)
colSums(is.na(recent_winter_rain_wide)) 

recent_winter_rain %>%
  arrange(recent_year_rainfall_cm)

recent_winter_rain %>%
  arrange(desc(recent_year_rainfall_cm))

# The rain station used for sample sites F12, G12, and H13 were set up in August 
# 2005 therefore winter 2005 average is listed as NA
winter_rain_by_site %>%
  filter(site_code == "F12", winter_year %in% 2001:2005)


recent_winter_rain %>%
  group_by(sample_year) %>%
  summarise(
    n_sites = n(), 
    n_sites_with_rain = sum(!is.na(recent_year_rainfall_cm)),
    min_rain = min(recent_year_rainfall_cm, na.rm = TRUE),
    max_rain = max(recent_year_rainfall_cm, na.rm = TRUE),
    mean_rain = mean(recent_year_rainfall_cm, na.rm = TRUE),
    median_rain = median(recent_year_rainfall_cm, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(
  recent_winter_rain,
  aes(
    x = factor(sample_year),
    y = recent_year_rainfall_cm
  )
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Recent Winter Rainfall by Sample Year",
    x = "Sample year",
    y = "Recent winter rainfall (cm)"
  )

##-----------------------------------------------



###------------------------------------------------
# Combine 3 rainfall tables
###------------------------------------------------

combined_three_rainfall <- combined_site_annual_windows %>%
  left_join(
    combined_site_winter_windows,
    by = c("site_code", "sample_year")
  ) %>%
  left_join(
    recent_winter_rain,
    by = c("site_code", "sample_year")
  ) %>%
  select(-contains("n_stations_used"))

write.csv(combined_three_rainfall, "combined_three_rainfall", row.names = FALSE)



# Combine all 3 into ESCA database aligning with sample years and removing 
# sampling data prior to 2005
ESCA_rain_merged <- ESCA_2023 %>%
  mutate(
    sample_year = year(sample_date)
  ) %>%
  filter(sample_year >= 2005) %>%
  left_join(
    combined_three_rainfall,
    by = c("site_code", "sample_year")
  ) %>%
  relocate(
    sample_year,
    .after = sample_date
  ) %>%
  relocate(
    ONCPIL,
    .after = sample_year
  ) %>%
  relocate(
    site_sample_window_avg_rainfall_cm,
    site_winter_window_rainfall_cm,
    recent_year_rainfall_cm,
    .after = ONCPIL
  )

# Rename rain columns
ESCA_rain_merged <- ESCA_rain_merged %>%
  rename(
    annual_period_avg_rainfall_cm = site_sample_window_avg_rainfall_cm,
    winter_period_avg_rainfall_cm = site_winter_window_rainfall_cm,
    recent_winter_rainfall_cm = recent_year_rainfall_cm
  )
write.csv(ESCA_rain_merged, "ESCA_rain_merged", row.names = FALSE)



#Next Part 
##-----------------------------------------------------------------------
# Comparing the 3 rainfall periods and stinknet presence
##-----------------------------------------------------------------------




