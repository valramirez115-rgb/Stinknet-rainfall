# Stinknet Analyses by Valerie Ramirez
# Part 5: Logistic regression analyses of the 3 rainfall periods stinknet 
# presence, sample years, and land use categories


# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(bannerCommenter)
library(emmeans)
library(broom)

# Looking at the 3 different periods > choosing best model fit > further analyses
# that better represents stinknet growth

# ==============================================================================
# Workflow: 
# 3 models for the three rainfall periods and compare AIC
# Use best fit rain model
# Compare further with reduced land use categories
# Post-hoc analysis
# ==============================================================================

# ---------------------------------------------------------------------------
# Comparing the three rainfall period approaches
# Prepare datatset for equal comparison among the three rainfall periods
# ---------------------------------------------------------------------------

rainfall_comparison_data <- ESCA_rain_merged %>%
  filter(
    !is.na(ONCPIL),
    !is.na(annual_period_avg_rainfall_cm),
    !is.na(winter_period_avg_rainfall_cm),
    !is.na(recent_winter_rainfall_cm)
  )


glm_annual_compare <- glm(
  ONCPIL ~ annual_period_avg_rainfall_cm + factor(sample_year),
  data = rainfall_comparison_data,
  family = binomial
)

glm_winter_compare <- glm(
  ONCPIL ~ winter_period_avg_rainfall_cm + factor(sample_year),
  data = rainfall_comparison_data,
  family = binomial
)

glm_recent_compare <- glm(
  ONCPIL ~ recent_winter_rainfall_cm + factor(sample_year),
  data = rainfall_comparison_data,
  family = binomial
)

# Compare AIC
AIC(
  glm_annual_compare,
  glm_winter_compare,
  glm_recent_compare
)

# Among the three rainfall metrics, recent winter rainfall produced the lowest 
# AIC (448.02), compared with winter-period average rainfall (453.46) and 
# annual-period average rainfall (453.76), indicating stronger relative support 
# for rainfall during the winter immediately preceding sampling.

# Inspect Recent Winter Rainfall
summary(glm_recent_compare)

# ==============================================================================
# Recent Winter Rainfall Analysis
# ==============================================================================

# ---------------------------------------------------------------------------
# 1. Prepare data sets: full land-use data set and reduced land-use category data set
# ---------------------------------------------------------------------------

# Full land-use dataset, keeps all land-use categories and excludes only "NA" 
# land use category, sets Desert category and year 2005 as reference

recent_full_data <- ESCA_rain_merged %>%
  filter(
    broad_classes != "NA",
    !is.na(ONCPIL),
    !is.na(recent_winter_rainfall_cm)
  ) %>%
  mutate(
    broad_classes_model = relevel(
      factor(broad_classes),
      ref = "Desert"
    ),
    sample_year_model = factor(sample_year),
    stinknet_presence = factor(
      ONCPIL,
      levels = c(FALSE, TRUE),
      labels = c("Absent", "Present")
    )
  )


# Reduced land-use dataset
# Excludes GolfCourse/park, Transportation, and Vacant

reduced_landuse_data <- recent_full_data %>%
  filter(
    broad_classes %in% c(
      "AgriculturalLand",
      "Desert",
      "Residential",
      "Urban"
    )
  ) %>%
  droplevels()


# ---------------------------------------------------------------------------
# 2. Recent Winter Rainfall Analysis with Full Land-Use Categories
# ---------------------------------------------------------------------------


# Logistic Regression: full additive model
# Stinknet presence and rainfall, all years pooled

glm_recent_simple <- glm(
  ONCPIL ~ recent_winter_rainfall_cm,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_simple)


# Logistic Regression: full additive model
# All land use categories, stinknet presence, rainfall, factoring sample years 

glm_recent_full <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    sample_year_model +
    broad_classes_model,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_full)



# Logistic Regression: full additive model
# Stinknet presence, rainfall, land-use without controlling for sample years

glm_recent_pooled <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    broad_classes_model,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_pooled)


# ---------------------------------------------------------------------------
# 3. Recent Winter Rainfall Analysis with Reduced Land-Use Categories
# (AgriculturalLand, Desert, Residential, Urban only)
# ---------------------------------------------------------------------------


# Use "landuse_main_data" dataframe that excludes GolfCourse/park, 
# Transportation, and Vacant land-use categories due to low sampling counts


# Logistic Regression: additive model 
# Stinknet presence, land-use, factoring sample years

glm_recent_reduced_landuse <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    sample_year_model +
    broad_classes_model,
  data = reduced_landuse_data,
  family = binomial
)

summary(glm_recent_reduced_landuse)

# Logistic Regression: additive model
# Stinknet presence, land-use, not controlling for sample years 

glm_recent_reduced_pooled <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    broad_classes_model,
  data = reduced_landuse_data,
  family = binomial
)

summary(glm_recent_reduced_pooled)




# ---------------------------------------------------------------------------
# 4. Testing Sample Year
# ---------------------------------------------------------------------------

anova(
  glm_recent_reduced_pooled,
  glm_recent_reduced_landuse,
  test = "Chisq"
)




# ---------------------------------------------------------------------------
# 5. Rainfall x Land-use Interaction 
# Reduced land-use dataset
# ---------------------------------------------------------------------------

# Reduced land-use dataset
# Logistic Regression: interaction model for reduced land use catgory dataset

glm_recent_reduced_interaction <- glm(
  ONCPIL ~ recent_winter_rainfall_cm *
    broad_classes_model +
    sample_year_model,
  data = reduced_landuse_data,
  family = binomial
)

summary(glm_recent_reduced_interaction)

# Compare additive vs interaction model
anova(
  glm_recent_reduced_landuse,
  glm_recent_reduced_interaction,
  test = "Chisq"
)


# ---------------------------------------------------------------------------
# Full land-use interaction model
# ---------------------------------------------------------------------------

# Full land-use interaction model

glm_recent_full_interaction <- glm(
  ONCPIL ~ recent_winter_rainfall_cm *
    broad_classes_model +
    sample_year_model,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_full_interaction)

anova(
  glm_recent_full,
  glm_recent_full_interaction,
  test = "Chisq"
)



# ============================================================
# 6. POST-HOC LAND-USE COMPARISONS
# ============================================================

# Full land-use analysis

recent_full_emmeans <- emmeans(
  glm_recent_full,
  ~ broad_classes_model,
  type = "response"
)

recent_full_emmeans

pairs(
  recent_full_emmeans,
  adjust = "tukey"
)


# ---------------------------------------------------------------------------
# Reduced four-category analysis

recent_reduced_emmeans <- emmeans(
  glm_recent_reduced_landuse,
  ~ broad_classes_model,
  type = "response"
)

recent_reduced_emmeans

pairs(
  recent_reduced_emmeans,
  adjust = "tukey"
)

# ============================================================
# 7. PRIMARY MODEL RESULTS AS ODDS RATIOS
# ============================================================

primary_results <- tidy(
  glm_recent_full,
  exponentiate = TRUE,
  conf.int = TRUE
)

primary_results


reduced_results <- tidy(
  glm_recent_reduced_landuse,
  exponentiate = TRUE,
  conf.int = TRUE
)

reduced_results

# ============================================================
# Data Counts
# ============================================================


sampling_summary <- data.frame(
  Measure = c(
    "Unique sampling sites",
    "Total site-year records",
    "Unique rainfall stations used",
    "Total station-to-site assignments"
  ),
  Count = c(
    n_distinct(ESCA_rain_merged$site_code),
    nrow(ESCA_rain_merged),
    n_distinct(site_station_long$station_id),
    nrow(site_station_long)
  )
)

sampling_summary



recent_full_data %>%
       group_by(sample_year, broad_classes) %>%
       summarise(
             total_site_year_samples = n(),
             .groups = "drop"
         )

recent_full_data %>%
  count(broad_classes, name = "n_samples")

recent_full_data %>%
       group_by(broad_classes) %>%
       summarise(
             unique_sites = n_distinct(site_code),
             total_site_year_samples = n(),
             .groups = "drop"
         )




reduced_landuse_data %>%
       group_by(sample_year, broad_classes) %>%
       summarise(
             total_site_year_samples = n(),
             .groups = "drop"
       )

reduced_landuse_data %>%
  count(broad_classes, name = "n_samples")

reduced_landuse_data %>%
  group_by(broad_classes) %>%
  summarise(
    unique_sites = n_distinct(site_code),
    total_site_year_samples = n(),
    .groups = "drop"
  )