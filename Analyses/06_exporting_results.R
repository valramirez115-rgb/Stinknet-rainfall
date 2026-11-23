
# Stinknet Analysis by Valerie Ramirez 
# Part 6: Saving tables 


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

## ==============================================================================
# 7. MODEL RESULTS AS ODDS RATIOS AND SAVING RESULTS
# ==============================================================================

#Full Land-Use Category Model Results


# Logistic Regression: full additive model
# Stinknet presence and rainfall, all years pooled

simple_pooled_gtsummary <- glm_recent_simple %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (per 1 cm)"
    )
  ) %>%
  bold_labels()

simple_pooled_gtsummary

# Save results
simple_pooled_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_simple_table.png")



# Logistic Regression: full additive model
# Stinknet presence and rainfall, factoring sample years

year_adjusted_gtsummary <- glm_recent_year_adjusted %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (per 1 cm)",
      sample_year_model ~ "Sample year"
    )
  ) %>%
  bold_labels()

year_adjusted_gtsummary

# Save results
year_adjusted_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_year_adjusted_table.png")


# Logistic Regression: full additive model
# Stinknet presence, rainfall, land-use without controlling for sample years
pooled_full_gtsummary <- glm_recent_pooled %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (per 1 cm)",
      broad_classes_model ~ "Land-use category"
    )
  ) %>%
  
  bold_labels()

pooled_full_gtsummary

# Save table 
pooled_full_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_pooled_table.png")




# Logistic Regression: full additive model
# Stinknet presence, rainfall, land-use, and factoring sample years

primary_gtsummary <- glm_recent_full %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (per 1 cm)",
      sample_year_model ~ "Sample year",
      broad_classes_model ~ "Land-use category"
    )
  ) %>%
  bold_labels()

primary_gtsummary

#or

#Add from previous iteration

# Save table
primary_gtsummary %>%
  as_gt() %>%
  gtsave("primary_full_additive_model_table.png")



# ------------------------------------------------------------------------------

# Reduced Land-Use Category Results
# Logistic Regression: reduced land use additive model
# Stinknet presence, rainfall, land-use without controlling for sample years
# glm_recent_reduced_pooled

reduced_pooled_gtsummary <- glm_recent_reduced_pooled %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (per 1 cm)",
      broad_classes_model ~ "Land-use category"
    )
  ) %>%
  bold_labels()

reduced_pooled_gtsummary

# Save table
reduced_pooled_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_reduced_pooled_table.png")



# Reduced Land-Use Category Results
# # Logistic Regression: reduced land use additive model
# Stinknet presence, rainfall, land-use, factoring sample years
# glm_recent_reduced_landuse

reduced_landuse_gtsummary <- glm_recent_reduced_landuse %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (per 1 cm)",
      sample_year_model ~ "Sample year",
      broad_classes_model ~ "Land-use category"
    )
  ) %>%
  bold_labels()

reduced_landuse_gtsummary

# Save table
reduced_landuse_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_reduced_landuse_table.png")



# ------------------------------------------------------------------------------
# 5. Rainfall x Land-use Interaction 
# ------------------------------------------------------------------------------

# Full Land-use Interaction Model
# glm_recent_full_interaction

full_interaction_gtsummary <- glm_recent_full_interaction %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (Desert slope)",
      sample_year_model ~ "Sample year",
      broad_classes_model ~ "Land-use category"
    )
  ) %>%
  bold_labels()

full_interaction_gtsummary



# Save table
full_interaction_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_full_interaction_table.png")

# ------------------------------------------------------------------------------
# Reduced Land-use Interaction Model
# glm_recent_reduced_interaction

reduced_interaction_gtsummary <- glm_recent_reduced_interaction %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      recent_winter_rainfall_cm ~ "Recent winter rainfall (Desert slope)",
      sample_year_model ~ "Sample year",
      broad_classes_model ~ "Land-use category"
    )
  ) %>%
  bold_labels()

reduced_interaction_gtsummary


# Save table
reduced_interaction_gtsummary %>%
  as_gt() %>%
  gtsave("glm_recent_reduced_interaction_table.png")



# ==============================================================================
# END
# ==============================================================================










