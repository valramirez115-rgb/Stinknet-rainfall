# Stinknet Analyses by Valerie Ramirez
# Part 4: Analyses of 3 rainfall periods stinknet presence, sample years, and 
# land use categories


# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(bannerCommenter)


##------------------------------------------------
# Comparing the 3 rainfall periods by
# Simple mean and median annual and winter mulit-year rainfall averages  
# Start comparing rainfall and stinknet presence
##-----------------------------------------------

##Work on titles

# Annual Rainfall Averages by Stinknet Presence

ggplot(ESCA_rain_merged,
       aes(x = factor(ONCPIL), y = annual_period_avg_rainfall_cm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Annual Rainfall Average by Stinknet Presence",
    x = "Stinknet Presence (ONCPIL)",
    y = "Rainfall (cm)"
  )

# Winter Rainfall Averages by Stinknet Presence
ggplot(ESCA_rain_merged,
       aes(x = factor(ONCPIL), y = winter_period_avg_rainfall_cm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Winter Period Rainfall by Stinknet Presence",
    x = "Stinknet Presence (ONCPIL)",
    y = "Rainfall (cm)"
  )


# Recent Winter Rainfall Averages by Stinknet Presence
## Avg rename?
ggplot(ESCA_rain_merged,
       aes(x = factor(ONCPIL), y = recent_winter_rainfall_cm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Recent Winter Period Rainfall by Stinknet Presence",
    x = "Stinknet Presence (ONCPIL)",
    y = "Rainfall (cm)"
  )


## Mean Rain by Sample Year and Proportion Invaded for annual and winter, recent winter
# multi-year rainfall averages
sample_periods_rain_by_year <- ESCA_rain_merged %>%
  group_by(sample_year) %>%
  summarise(
    mean_annual_rain = mean(annual_period_avg_rainfall_cm, na.rm = TRUE),
    mean_winter_rain = mean(winter_period_avg_rainfall_cm, na.rm = TRUE),
    mean_recent_winter_rain = mean(recent_winter_rainfall_cm, na.rm = TRUE),
    prop_invaded = mean(ONCPIL, na.rm = TRUE),
    .groups = "drop"
  )

sample_periods_rain_by_year

#need to save? 


##------------------------------------------------
# Comparing the 3 rainfall periods by
# land-use categories
## Removing samples with NA land-use categories
##-----------------------------------------------

# boxplots summarize the distribution of the rainfall values already in the dataframe for each land-use category.
# Annual Rainfall Sampling Period
ggplot(
  ESCA_rain_merged %>%
    filter(broad_classes != "NA"),
  aes(x = broad_classes, y = annual_period_avg_rainfall_cm)
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Annual Rainfall Average by Broad Land-Use Category",
    x = "Broad Land Use",
    y = "Average annual rainfall (cm)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# Winter Period
ggplot(
  ESCA_rain_merged %>%
    filter(broad_classes != "NA"),
  aes(x = broad_classes, y = winter_period_avg_rainfall_cm)
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Average Winter Rainfall by Broad Land-Use Category",
    x = "Broad Land Use",
    y = "Average winter rainfall (cm)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Recent Winter
ggplot(
  ESCA_rain_merged %>%
    filter(broad_classes != "NA"),
  aes(x = broad_classes, y = recent_winter_rainfall_cm)
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Recent Winter Rainfall by Broad Land-Use Category",
    x = "Broad Land Use",
    y = "Recent winter rainfall (cm)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



##--------------------------------------------------
# Annual by sample year
ggplot(
  ESCA_rain_merged %>%
    filter(broad_classes != "NA"),
  aes(
    x = broad_classes,
    y = annual_period_avg_rainfall_cm
  )
) +
  geom_boxplot() +
  facet_wrap(~ sample_year, ncol = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Annual Rainfall by Land Use and Sample Year",
    x = "Broad Land Use",
    y = "Average annual rainfall (cm)"
  )


# Winter by sample year
ggplot(
  ESCA_rain_merged %>%
    filter(broad_classes != "NA"),
  aes(
    x = broad_classes,
    y = winter_period_avg_rainfall_cm
  )
) +
  geom_boxplot() +
  facet_wrap(~ sample_year, ncol = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Winter Rainfall by Land Use and Sample Year",
    x = "Broad Land Use",
    y = "Average winter rainfall (cm)"
  )

#Recent Winter by sample year
ggplot(
  ESCA_rain_merged %>%
    filter(broad_classes != "NA"),
  aes(
    x = broad_classes,
    y = recent_winter_rainfall_cm
  )
) +
  geom_boxplot() +
  facet_wrap(~ sample_year, ncol = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Recent Winter Rainfall by Land Use and Sample Year",
    x = "Broad Land Use",
    y = "Recent winter rainfall (cm)"
  )

##--------------------------------------------------
# Rainfall periods and Stinknet presence 
##--------------------------------------------------


rain_plot_data <- ESCA_rain_merged %>%
  mutate(
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  )

## For Appendix?
# Annual Rainfall by Stinknet Presence and Land Use
ggplot(
  rain_plot_data %>%
    filter(broad_classes != "NA"),
  aes(
    x = stinknet_presence,
    y = annual_period_avg_rainfall_cm
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1.3
  ) +
  facet_wrap(~ broad_classes) +
  theme_minimal() +
  labs(
    title = "Average Annual Rainfall by Stinknet Presence and Land Use",
    x = "Stinknet presence",
    y = "Average annual rainfall (cm)"
  )


# Winter Rainfall by Stinknet Presence and Land Use

ggplot(
  rain_plot_data %>%
    filter(broad_classes != "NA"),
  aes(
    x = stinknet_presence,
    y = winter_period_avg_rainfall_cm
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1.3
  ) +
  facet_wrap(~ broad_classes) +
  theme_minimal() +
  labs(
    title = "Average Winter Rainfall by Stinknet Presence and Land Use",
    x = "Stinknet presence",
    y = "Average winter rainfall (cm)"
  )

# Recent Winter Rainfall by Stinknet Presence and Land Use
ggplot(
  rain_plot_data %>%
    filter(broad_classes != "NA"),
  aes(
    x = stinknet_presence,
    y = recent_winter_rainfall_cm
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1.3
  ) +
  facet_wrap(~ broad_classes) +
  theme_minimal() +
  labs(
    title = "Recent Winter Rainfall by Stinknet Presence and Land Use",
    x = "Stinknet presence",
    y = "Recent winter rainfall (cm)"
  )



# Tables
# Annual Rainfall Period Averages
annual_landuse_year_summary <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  mutate(
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  ) %>%
  group_by(sample_year, broad_classes, stinknet_presence) %>%
  summarise(
    n_sites = n(),
    mean_rain = mean(annual_period_avg_rainfall_cm, na.rm = TRUE),
    median_rain = median(annual_period_avg_rainfall_cm, na.rm = TRUE),
    .groups = "drop"
  )


# Winter Rainfall Period Averages
winter_landuse_year_summary <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  mutate(
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  ) %>%
  group_by(sample_year, broad_classes, stinknet_presence) %>%
  summarise(
    n_sites = n(),
    mean_rain = mean(winter_period_avg_rainfall_cm, na.rm = TRUE),
    median_rain = median(winter_period_avg_rainfall_cm, na.rm = TRUE),
    .groups = "drop"
  )



# Recent Winter Rainfall 
recent_winter_landuse_year_summary <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  mutate(
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  ) %>%
  group_by(sample_year, broad_classes, stinknet_presence) %>%
  summarise(
    n_sites = n(),
    mean_rain = mean(recent_winter_rainfall_cm, na.rm = TRUE),
    median_rain = median(recent_winter_rainfall_cm, na.rm = TRUE),
    .groups = "drop"
  )

# Save all
write.csv(
  annual_landuse_year_summary,
  "annual_landuse_year_summary.csv",
  row.names = FALSE
)

write.csv(
  winter_landuse_year_summary,
  "winter_landuse_year_summary.csv",
  row.names = FALSE
)

write.csv(
  recent_winter_landuse_year_summary,
  "recent_winter_landuse_year_summary.csv",
  row.names = FALSE
)

# Dot plot of mean rainfall based on summary tables

# Annual rainfall period averages dot plot

ggplot(
  annual_landuse_year_summary,
  aes(
    x = mean_rain,
    y = broad_classes,
    shape = stinknet_presence,
    size = n_sites
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    alpha = 0.7
  ) +
  facet_wrap(~ sample_year, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Average Annual Rainfall by Land Use and Stinknet Presence",
    x = "Mean annual-period rainfall (cm)",
    y = "Broad Land Use",
    shape = "Stinknet presence",
    size = "Number of sites"
  )




# Winter rainfall period averages dot plot
ggplot(
  winter_landuse_year_summary,
  aes(
    x = mean_rain,
    y = broad_classes,
    shape = stinknet_presence,
    size = n_sites
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    alpha = 0.7
  ) +
  facet_wrap(~ sample_year, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Average Winter Rainfall by Land Use and Stinknet Presence",
    x = "Mean winter-period rainfall (cm)",
    y = "Broad Land Use",
    shape = "Stinknet presence",
    size = "Number of sites"
  )


# Recent winter rainfall period dot plot

ggplot(
  recent_winter_landuse_year_summary,
  aes(
    x = mean_rain,
    y = broad_classes,
    shape = stinknet_presence,
    size = n_sites
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    alpha = 0.7
  ) +
  facet_wrap(~ sample_year, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Recent Winter Rainfall by Land Use and Stinknet Presence",
    x = "Mean recent winter rainfall (cm)",
    y = "Broad Land Use",
    shape = "Stinknet presence",
    size = "Number of sites"
  )

# Save and export:



# ------------------------------------------------------------------------------
#Land-use counts
# All defined land-use categories were retained in the analyses. 
#Categories with small sample sizes in individual sampling years were 
#interpreted cautiously because their rainfall distributions and model estimates
# were based on fewer observations.
# ------------------------------------------------------------------------------

landuse_sample_counts <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  count(
    sample_year,
    broad_classes,
    name = "n_samples"
  ) %>%
  arrange(sample_year, broad_classes)

landuse_sample_counts

landuse_sample_counts_wide <- landuse_sample_counts %>%
  pivot_wider(
    names_from = sample_year,
    values_from = n_samples,
    values_fill = 0
  )

landuse_sample_counts_wide



landuse_presence_counts <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  mutate(
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  ) %>%
  count(
    sample_year,
    broad_classes,
    stinknet_presence,
    name = "n_samples"
  ) %>%
  arrange(sample_year, broad_classes, stinknet_presence)

landuse_presence_counts


landuse_presence_counts_wide <- landuse_presence_counts %>%
  pivot_wider(
    names_from = stinknet_presence,
    values_from = n_samples,
    values_fill = 0
  )

landuse_presence_counts_wide



## End























