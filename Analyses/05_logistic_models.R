# Stinknet Analyses by Valerie Ramirez

# Part 5: Logistic regression analyses of the 3 rainfall periods stinknet 
# presence, sample years, and land use categories


# Load Libraries
library(readxl)
library(tibble)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(bannerCommenter)
library(emmeans)
library(broom)
library(gt)
library(gtsummary)


# Looking at the 3 different periods > choosing best model fit > further analyses
# that better represents stinknet growth

# ==============================================================================
# WORKFLOW: 
# 3 models for the three rainfall periods and compare AIC
# Use best fit rain model
# Compare further with reduced land use categories
# Post-hoc analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# Comparing the three rainfall period approaches
# Prepare datatset for equal comparison among the three rainfall periods
# ------------------------------------------------------------------------------

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
# annual-period average rainfall (453.76), indicating stronger support 
# for rainfall during the winter immediately preceding sampling.

# Inspect Recent Winter Rainfall
summary(glm_recent_compare)


# Save results
aic_rainfall_comparison <- tibble(
  Rainfall_Metric = c(
    "Annual-period average rainfall",
    "Winter-period average rainfall",
    "Recent-winter rainfall"
  ),
  AIC = c(
    AIC(glm_annual_compare),
    AIC(glm_winter_compare),
    AIC(glm_recent_compare)
  )
) %>%
  mutate(
    Delta_AIC = AIC - min(AIC)
  )

aic_rainfall_comparison


aic_rainfall_comparison %>%
  gt() %>%
  tab_header(
    title = "AIC Comparison of Rainfall Models"
  ) %>%
    cols_label(
              Rainfall_Metric = "Rainfall metric",
              AIC = "AIC",
              Delta_AIC = "ΔAIC"
    ) %>%
  fmt_number(
    columns = c(AIC, Delta_AIC),
    decimals = 2
  )%>%
  gtsave("aic_rainfall_comparison.png")


# ==============================================================================
# RECENT WINTER RAINFALL ANALYSIS
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Prepare data sets: full land-use data set and reduced land-use category data set
# ------------------------------------------------------------------------------

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


# Reduced land-use dataset:excludes GolfCourse/park, Transportation, and Vacant

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


# ------------------------------------------------------------------------------
# 2. Recent Winter Rainfall Analysis with Full Land-Use Categories
# ------------------------------------------------------------------------------


# Logistic Regression: full additive model
# Stinknet presence and rainfall, all years pooled

glm_recent_simple <- glm(
  ONCPIL ~ recent_winter_rainfall_cm,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_simple)


# Logistic Regression: full additive model
# Stinknet presence, rainfall, factoring sample years
glm_recent_year_adjusted <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    sample_year_model,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_year_adjusted)


# Logistic Regression: full additive model
# Stinknet presence, rainfall, land-use without factoring sample years

glm_recent_pooled <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    broad_classes_model,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_pooled)



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



# ------------------------------------------------------------------------------
# 3. Recent Winter Rainfall Analysis with Reduced Land-Use Categories
# (AgriculturalLand, Desert, Residential, Urban only)
# ------------------------------------------------------------------------------


# Use "reduced_landuse_data" dataframe that excludes GolfCourse/park, 
# Transportation, and Vacant land-use categories due to low sampling counts

# Logistic Regression: additive model
# Stinknet presence, rainfall, land-use, not controlling for sample years 

glm_recent_reduced_pooled <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    broad_classes_model,
  data = reduced_landuse_data,
  family = binomial
)

summary(glm_recent_reduced_pooled)


# Logistic Regression: additive model 
# Stinknet presence, rainfall, land-use, factoring sample years

glm_recent_reduced_landuse <- glm(
  ONCPIL ~ recent_winter_rainfall_cm +
    sample_year_model +
    broad_classes_model,
  data = reduced_landuse_data,
  family = binomial
)

summary(glm_recent_reduced_landuse)





# ------------------------------------------------------------------------------
# 4. Testing Sample Year with ANOVA
# ------------------------------------------------------------------------------

anova(
  glm_recent_simple,
  glm_recent_year_adjusted,
  test = "Chisq"
)

anova(
  glm_recent_reduced_pooled,
  glm_recent_reduced_landuse,
  test = "Chisq"
)




# ------------------------------------------------------------------------------
# 5. Rainfall x Land-use Interaction 
# ------------------------------------------------------------------------------

# Full Land-use Interaction Model

glm_recent_full_interaction <- glm(
  ONCPIL ~ recent_winter_rainfall_cm *
    broad_classes_model +
    sample_year_model,
  data = recent_full_data,
  family = binomial
)

summary(glm_recent_full_interaction)

# Compare Additive vs Interaction model

anova(
  glm_recent_full,
  glm_recent_full_interaction,
  test = "Chisq"
)


# Save Results
full_interaction_results <- tidy(
  glm_recent_full_interaction,
  exponentiate = TRUE,
  conf.int = TRUE
)

full_interaction_results


full_interaction_results_clean <- full_interaction_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "recent_winter_rainfall_cm" ~ "Recent winter rainfall (Desert slope)",
      term == "sample_year_model2010" ~ "2010 vs 2005",
      term == "sample_year_model2015" ~ "2015 vs 2005",
      term == "sample_year_model2023" ~ "2023 vs 2005",
      
      term == "broad_classes_modelAgriculturalLand" ~ "AgriculturalLand vs Desert",
      term == "broad_classes_modelGolfCourse/park" ~ "GolfCourse/park vs Desert",
      term == "broad_classes_modelResidential" ~ "Residential vs Desert",
      term == "broad_classes_modelTransportation" ~ "Transportation vs Desert",
      term == "broad_classes_modelUrban" ~ "Urban vs Desert",
      term == "broad_classes_modelVacant" ~ "Vacant vs Desert",
      
      term == "recent_winter_rainfall_cm:broad_classes_modelAgriculturalLand" ~
        "Rainfall × AgriculturalLand",
      term == "recent_winter_rainfall_cm:broad_classes_modelGolfCourse/park" ~
        "Rainfall × GolfCourse/park",
      term == "recent_winter_rainfall_cm:broad_classes_modelResidential" ~
        "Rainfall × Residential",
      term == "recent_winter_rainfall_cm:broad_classes_modelTransportation" ~
        "Rainfall × Transportation",
      term == "recent_winter_rainfall_cm:broad_classes_modelUrban" ~
        "Rainfall × Urban",
      term == "recent_winter_rainfall_cm:broad_classes_modelVacant" ~
        "Rainfall × Vacant",
      
      TRUE ~ term
    )
  ) %>%
  select(
    Predictor,
    OR = estimate,
    conf.low,
    conf.high,
    p.value
  )

full_interaction_results_clean

# Save Tables

write.csv(
  full_interaction_results_clean,
  "full_interaction_results_clean.csv",
  row.names = FALSE
)



# ------------------------------------------------------------------------------
# Reduced Land-use Interaction Model

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

# Save Results

reduced_interaction_results <- tidy(
  glm_recent_reduced_interaction,
  exponentiate = TRUE,
  conf.int = TRUE
)

reduced_interaction_results



reduced_interaction_results_clean <- reduced_interaction_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "recent_winter_rainfall_cm" ~
        "Recent winter rainfall (Desert slope)",
      
      term == "sample_year_model2010" ~ "2010 vs 2005",
      term == "sample_year_model2015" ~ "2015 vs 2005",
      term == "sample_year_model2023" ~ "2023 vs 2005",
      
      term == "broad_classes_modelAgriculturalLand" ~
        "AgriculturalLand vs Desert",
      term == "broad_classes_modelResidential" ~
        "Residential vs Desert",
      term == "broad_classes_modelUrban" ~
        "Urban vs Desert",
      
      term == "recent_winter_rainfall_cm:broad_classes_modelAgriculturalLand" ~
        "Rainfall × AgriculturalLand",
      term == "recent_winter_rainfall_cm:broad_classes_modelResidential" ~
        "Rainfall × Residential",
      term == "recent_winter_rainfall_cm:broad_classes_modelUrban" ~
        "Rainfall × Urban",
      
      TRUE ~ term
    )
  ) %>%
  select(
    Predictor,
    OR = estimate,
    conf.low,
    conf.high,
    p.value
  )

reduced_interaction_results_clean

# Save Table
write.csv(
  reduced_interaction_results_clean,
  "reduced_interaction_results_clean.csv",
  row.names = FALSE
)






# ============================================================
# 6. POST-HOC LAND-USE COMPARISONS
# ============================================================

# Full Land-use Analysis

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


# ------------------------------------------------------------------------------
# Reduced Four-Category Analysis

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

# ==============================================================================
# 7. MODEL RESULTS AS ODDS RATIOS
# ==============================================================================

#Full Category Model Results
primary_results <- tidy(
  glm_recent_full,
  exponentiate = TRUE,
  conf.int = TRUE
)

primary_results

# Table format
primary_results_clean <- primary_results %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Predictor = term,
    OR = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  )

primary_results_clean

primary_results_clean <- primary_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "recent_winter_rainfall_cm" ~ "Recent winter rainfall (per 1 cm)",
      term == "sample_year_model2010" ~ "2010 vs 2005",
      term == "sample_year_model2015" ~ "2015 vs 2005",
      term == "sample_year_model2023" ~ "2023 vs 2005",
      term == "broad_classes_modelAgriculturalLand" ~ "AgriculturalLand vs Desert",
      term == "broad_classes_modelGolfCourse/park" ~ "GolfCourse/park vs Desert",
      term == "broad_classes_modelResidential" ~ "Residential vs Desert",
      term == "broad_classes_modelTransportation" ~ "Transportation vs Desert",
      term == "broad_classes_modelUrban" ~ "Urban vs Desert",
      term == "broad_classes_modelVacant" ~ "Vacant vs Desert",
      TRUE ~ term
    )
  ) %>%
  select(
    Predictor,
    OR = estimate,
    conf.low,
    conf.high,
    p.value
  )
primary_results_clean



write.csv(
  primary_results_clean,
  "primary_results_clean.csv",
  row.names = FALSE
)


primary_results_clean %>%
  gt() %>%
  gtsave("primary_results_table.png")

primary_results_clean %>%
  gt() %>%
  gtsave("primary_results_table.pdf")


# Logistic Regression: full additive model
# Stinknet presence and rainfall, all years pooled

# Save results

pooled_simple_results <- tidy(
  glm_recent_simple,
  exponentiate = TRUE,
  conf.int = TRUE
)

pooled_simple_results


pooled_simple_results_clean <- pooled_simple_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "recent_winter_rainfall_cm" ~
        "Recent winter rainfall (per 1 cm)",
      TRUE ~ term
    )
  ) %>%
  select(
    Predictor,
    OR = estimate,
    conf.low,
    conf.high,
    p.value
  )

pooled_simple_results_clean






# ------------------------------------------------------------------------------

# Reduced Category Results
reduced_results <- tidy(
  glm_recent_reduced_landuse,
  exponentiate = TRUE,
  conf.int = TRUE
)

reduced_results


reduced_results_clean <- reduced_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "recent_winter_rainfall_cm" ~ "Recent winter rainfall (per 1 cm)",
      term == "sample_year_model2010" ~ "2010 vs 2005",
      term == "sample_year_model2015" ~ "2015 vs 2005",
      term == "sample_year_model2023" ~ "2023 vs 2005",
      term == "broad_classes_modelAgriculturalLand" ~ "AgriculturalLand vs Desert",
      term == "broad_classes_modelResidential" ~ "Residential vs Desert",
      term == "broad_classes_modelUrban" ~ "Urban vs Desert",
      TRUE ~ term
    )
  ) %>%
  select(
    Predictor,
    OR = estimate,
    conf.low,
    conf.high,
    p.value
  )

reduced_results_clean


write.csv(
  reduced_results_clean,
  "reduced_results_clean.csv",
  row.names = FALSE
)






# ==============================================================================
# DATA COUNTS
# ==============================================================================


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


#Next Part 
##------------------------------------------------------------------------------
# Saving and exporting the results of logistic regression models
##------------------------------------------------------------------------------

