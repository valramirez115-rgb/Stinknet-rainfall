# Stinknet Analyses by Valerie Ramirez

# Part 4: Stinknet occurrence trends and analyses of 3 rainfall periods Stinknet 
# presence, sample years, and land use categories


# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gt)
library(bannerCommenter)

##------------------------------------------------------------------------------
# Stinknet Occurrence Trends
##------------------------------------------------------------------------------

# Proportion of sites invaded by Stinknet over time
stinknet_year_summary <- ESCA_rain_merged %>%
  group_by(sample_year) %>%
  summarise(
    Percent_Invaded = mean(ONCPIL, na.rm = TRUE) * 100
  )

ggplot(stinknet_year_summary,
       aes(x = sample_year, y = Percent_Invaded)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(2005, 2010, 2015, 2023)
  ) +
  labs(
    x = "Sampling year",
    y = "Sites invaded by stinknet (%)",
    title = "Stinknet Occurrence Over Time"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.5)
  )


# First Detection 
first_detection <- ESCA_rain_merged %>%
  filter(ONCPIL == TRUE) %>%
  group_by(site_code) %>%
  summarise(
    First_Detection = min(sample_year)
  )

ggplot(first_detection,
       aes(x = factor(First_Detection))) +
  geom_bar() +
  labs(
    x = "Sampling year",
    y = "Number of newly detected sites",
    title = "First Detection of Stinknet Across Sampling Years"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

##------------------------------------------------------------------------------
# Occurrence Trend by Land Use Type
##------------------------------------------------------------------------------

# Percent of Sites Invaded by Stinknet Across Sampling Years

broad_landuse_by_year <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  group_by(sample_year, broad_classes) %>%
  summarise(
    Total_Sites = n_distinct(site_code),
    Sites_Invaded = n_distinct(site_code[ONCPIL == TRUE]),
    Percent_Invaded = round(
      Sites_Invaded / Total_Sites * 100, 1
    ),
    .groups = "drop"
  )

broad_landuse_by_year


broad_landuse_table <- broad_landuse_by_year %>%
  select(
    broad_classes,
    sample_year,
    Percent_Invaded
  ) %>%
  pivot_wider(
    names_from = sample_year,
    values_from = Percent_Invaded
  )

broad_landuse_table


broad_landuse_table_gt <- broad_landuse_table %>%
  gt() %>%
  cols_label(
    broad_classes = "Land-use category"
  ) %>%
  tab_header(
    title = "Percent of Sites Invaded by Stinknet Across Sampling Years"
  )

broad_landuse_table_gt

gtsave(
  broad_landuse_table_gt,
  "broad_landuse_by_year_table.png"
)


# Stinknet Occurrence by Broad Land-Use Category
broad_site_summary <- ESCA_rain_merged %>%
  filter(broad_classes != "NA") %>%
  group_by(broad_classes) %>%
  summarise(
    Total_Sites = n_distinct(site_code),
    Sites_Ever_Invaded = n_distinct(site_code[ONCPIL == TRUE]),
    Percent_Sites_Invaded = round(
      Sites_Ever_Invaded / Total_Sites * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(Percent_Sites_Invaded))

broad_site_summary

broad_site_summary_table <- broad_site_summary %>%
  gt() %>%
  cols_label(
    broad_classes = "Land-use category",
    Total_Sites = "Total sites",
    Sites_Ever_Invaded = "Sites ever invaded",
    Percent_Sites_Invaded = "Sites ever invaded (%)"
  ) %>%
  tab_header(
    title = "Stinknet Occurrence by Broad Land-Use Category"
  )

broad_site_summary_table

gtsave(
  broad_site_summary_table,
  "broad_site_summary_table.png"
)



##------------------------------------------------------------------------------
# Comparing the 3 rainfall periods 
##------------------------------------------------------------------------------

# Annual Rainfall Averages by Stinknet Presence
# Annual-period average = annual rainfall totals averaged across the years in 
# the sampling period (2001–2005, 2006–2010, etc.)
ggplot(
  ESCA_rain_merged,
  aes(
    x = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    ),
    y = annual_period_avg_rainfall_cm
  )
) +
  geom_boxplot() +
  labs(
    title = "Multi-Year Annual Rainfall Average by Stinknet Presence",
    x = "Stinknet Presence",
    y = "Rainfall (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Winter Rainfall Averages by Stinknet Presence
# October–February winter totals averaged across the winters in the sampling period

ggplot(
  ESCA_rain_merged,
  aes(
    x = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    ),
    y = winter_period_avg_rainfall_cm
  )
) +
  geom_boxplot() +
  labs(
    title = "Multi-Year Winter Rainfall Average by Stinknet Presence",
    x = "Stinknet Presence",
    y = "Rainfall (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Recent Winter Rainfall Averages by Stinknet Presence
# only the single October–February season associated with that sampling year
ggplot(
  ESCA_rain_merged,
  aes(
    x = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    ),
    y = recent_winter_rainfall_cm
  )
) +
  geom_boxplot() +
  labs(
    title = "Recent Winter Rainfall by Stinknet Presence",
    x = "Stinknet Presence",
    y = "Rainfall (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )




##------------------------------------------------------------------------------
# Rainfall periods and Stinknet presence 
##------------------------------------------------------------------------------

rain_plot_data <- ESCA_rain_merged %>%
  mutate(
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  )


##------------------------------------------------------------------------------
# Dot plot of mean rainfall based on summary tables
##------------------------------------------------------------------------------
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
# Land-use counts
# All defined land-use categories were retained in the analyses
# Categories with small sample sizes in individual sampling years were 
# interpreted cautiously because their rainfall distributions estimates
# were based on fewer observations
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


#Next Part 
##------------------------------------------------------------------------------
# Logistic regression analyses of the 3 rainfall periods Stinknet 
# presence, sample years, and land use categories
##------------------------------------------------------------------------------


