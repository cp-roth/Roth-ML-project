## ---- packages --------
#load needed packages. make sure they are installed.
library(tidyverse) #for data processing/cleaning; includes ggplot2, tidyr, readr, dplyr, stringr, purr, forcats
library(skimr) #for nice visualization of data 
library(here) #to set paths
library(renv) #for package management
library(knitr) #for nice tables
library(kableExtra) #for nice tables
library(gt) #for nice tables
library(gtsummary) #for summary tables
library(ggplot2) #for plotting
library(tidymodels) #for modeling

#rm(list=ls()) #clear the environment

## ---- loaddata --------
#Path to summary data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","ML_linear.rds")

#load data
ML_linear <- readRDS(data_location)

## ---- Model1 --------
# Model using recipes and workflows
# Define recipe for model
ML_recipe1 <- recipe(Weightgrams ~ ModifiedColor, data = ML_linear) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(ModifiedColor) # Convert ModifiedColor to dummy variable

# Specify model
ML_spec1 <- linear_reg() %>% 
  set_engine("lm")

# Create work flow
ML_wf1 <- workflow() %>%
  add_recipe(ML_recipe1) %>%
  add_model(ML_spec1)

# Fit the work flow
ML_fit1 <- fit(ML_wf1, data = ML_linear)

# Summarize model fit with tidiers
ML_summary1 <- summary(ML_fit1) # Summary of fit
tidy(ML_fit1, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit1) # Model-level statistics

# Produce predictions for R-squared
ML_fit1_pred <- predict(ML_fit1, new_data = ML_linear %>% select(-Weightgrams))
ML_fit1_pred

# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit1_pred <- bind_cols(ML_fit1_pred, ML_linear %>% select(Weightgrams))
ML_fit1_pred

# Plot observed versus predicted
plot1 <- ggplot(ML_fit1_pred, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot1

# Create metric set including RMSE and R-squared
metrics_fit1 <- metric_set(rmse, rsq)
metrics_fit1(ML_fit1_pred, truth = Weightgrams, estimate = .pred)

# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit1 <- as.data.frame(metrics_fit1(ML_fit1_pred, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit1

# Create a gtsummary table directly from the model
table2 <-
  tbl_regression(ML_fit1, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table2

#Print table
table2

# Save regression output into table
saveRDS(table2, file = here::here("results", "tables", "table2_final.rds"))

## ---- Model2 --------
# Using recipes and workflows
# Define recipe for model
ML_recipe2 <- recipe(Weightgrams ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors

# Define model specification
ML_spec2 <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

# Create workflow
ML_wf2 <- workflow() %>%
  add_recipe(ML_recipe2) %>%
  add_model(ML_spec2)

# Fit the workflow
ML_fit2 <- fit(ML_wf2, data = ML_linear)

# Summarize model fit with tidiers
ML_lm2_summary <- summary(ML_fit2) # Summary of fit
tidy(ML_fit2, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_fit2) # Model-level statistics

# Create a gtsummary table directly from the model
table3 <- pull_workflow_fit(ML_fit2) %>%
  tbl_regression(exponentiate = FALSE, intercept = TRUE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>%
  bold_p()
table3

# Save regression output into table
saveRDS(table3, file = here::here("results", "tables", "table3_final.rds"))

## ---- Model3 --------
# Using recipes and workflows
# Simple linear model
# Specify recipe
ML_recipe3 <- recipe(Weightgrams ~ Color, data = ML_linear) %>%
  step_relevel(Color, ref_level = "White")

# Specify model
ML_spec3 <- linear_reg() %>%
  set_engine("lm")

# Create workflow
ML_wf3 <- workflow() %>%
  add_recipe(ML_recipe3) %>%
  add_model(ML_spec3)

# Fit the workflow
ML_fit3 <- fit(ML_wf3, data = ML_linear)

# Summarize model fit with tidiers and create a gtsummary table directly from the model
ML_summary3 <- summary(ML_fit3)
tidy(ML_fit3, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit3) # Model-level statistics

# Create a gtsummary table from model
table4 <- tbl_regression(ML_fit3, exponentiate = FALSE, intercept = TRUE) %>%
  bold_p() %>%
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table4

# Save regression output into table
saveRDS(table4, file = here("results", "tables", "table4_final.rds"))

## ---- Model4 --------
# Using reipes and workflows
# Complex linear model
# Specify recipe
ML_recipe4 <- recipe(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>%
  step_relevel(Color, ref_level = "White") %>% # Set reference level for Color
  step_relevel(ModifiedStatus, ref_level = "Multipara") %>% # Set reference level for ModifiedStatus
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors

# Specify model
ML_spec4 <- linear_reg() %>%
  set_engine("lm")

# Create workflow
ML_wf4 <- workflow() %>%
  add_recipe(ML_recipe4) %>%
  add_model(ML_spec4)

# Fit the workflow
ML_fit4 <- fit(ML_wf4, data = ML_linear)

# Summarize model fit with tidiers and create a gtsummary table directly from the model
ML_summary4 <- summary(ML_fit4)
tidy(ML_fit4, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit4) # Model-level statistics

# Create a gtsummary table from model
table5 <- tbl_regression(ML_fit4, exponentiate = FALSE, intercept = TRUE) %>%
  bold_p() %>%
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table5

# Save regression output into table
saveRDS(table5, file = here("results", "tables", "table5_final.rds"))