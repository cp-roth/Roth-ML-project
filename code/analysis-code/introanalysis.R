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

## ---- lm1 --------
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
  modify_column_unhide(columns = c(statistic, std.error))

#Print table
table2

# Save regression output into table
saveRDS(table2, file = here::here("results", "tables", "table2.rds"))

## ---- lmage --------
# Model using recipes and workflows
# Create recipe
ML_recipe_age <- recipe(Weightgrams ~ Age, data = ML_linear)

# Specify model
ML_spec_age <- linear_reg() %>% 
  set_engine("lm")

# Create work flow
ML_wf_age <- workflow() %>%
  add_recipe(ML_recipe_age) %>%
  add_model(ML_spec_age)

# Fit the work flow
ML_fit_age <- fit(ML_wf_age, data = ML_linear)

# Summarize model fit with tidiers
ML_summary_age <- summary(ML_fit_age) # Summary of fit
tidy(ML_fit_age, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit_age) # Model-level statistics

# Produce predictions for RMSE and R-squared
ML_age_pred <- predict(ML_fit_age, new_data = ML_linear %>% select(-Weightgrams))
ML_age_pred

# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_age_pred <- bind_cols(ML_age_pred, ML_linear %>% select(Weightgrams))
ML_age_pred

# Plot observed versus predicted
plot_age <- ggplot(ML_age_pred, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot_age

# Create metric set including RMSE and R-squared
metrics_fit_age <- metric_set(rmse, rsq)
metrics_fit_age(ML_age_pred, truth = Weightgrams, estimate = .pred)

# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit_age <- as.data.frame(metrics_fit_age(ML_age_pred, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit_age

# Create a gtsummary table directly from the model
table_age <-
  tbl_regression(ML_fit_age, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))

#Print table
table_age

# Save regression output into table
saveRDS(table_age, file = here::here("results", "tables", "tableage_final.rds"))

## ---- lm3 --------
# Set reference level for ModifiedColor
ML_linear$ModifiedColor <- relevel(ML_linear$ModifiedColor, ref = "Euro-Descent")

# Specify and fit model using linear_reg() (default engine, OLS) from tidymodels
ML_lm3 <- linear_reg() %>% set_engine("lm") %>% fit(Weightgrams ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = ML_linear)

# Summarize model fit with tidiers
ML_lm3_summary <- summary(ML_lm3) # Summary of fit
tidy(ML_lm3, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_lm3) # Model-level statistics

# Create a gtsummary table directly from the model
table3 <- tbl_regression(ML_lm3, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table3

# Save regression output into table
saveRDS(table3, file = here::here("results", "tables", "table3.rds"))

## ---- lm4-5 --------
# Simple linear model
# Set reference level for Color
ML_linear$Color <- relevel(ML_linear$Color, ref = "White")

# Specify and fit model using linear_reg() (default engine, OLS) from tidymodels
ML_lm4 <- linear_reg() %>% set_engine("lm") %>% fit(Weightgrams ~ Color, data = ML_linear)

# Summarize model fit with tidiers
ML_lm4_summary <- summary(ML_lm4) # Summary of fit
tidy(ML_lm4, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_lm4) # Model-level statistics

# Create a gtsummary table directly from the model
table4 <- tbl_regression(ML_lm4, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table4

# Save regression output into table
saveRDS(table4, file = here::here("results", "tables", "table4.rds"))

# Complex linear model
# Set reference level for Color
ML_linear$Color <- relevel(ML_linear$Color, ref = "White")

# Specify and fit model using linear_reg() (default engine, OLS) from tidymodels
ML_lm5 <- linear_reg() %>% set_engine("lm") %>% fit(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = ML_linear)

# Summarize model fit with tidiers
ML_lm5_summary <- summary(ML_lm5) # Summary of fit
tidy(ML_lm5, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_lm5) # Model-level statistics

# Create a gtsummary table directly from the model
table5 <- tbl_regression(ML_lm5, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table5

# Save regression output into table
saveRDS(table5, file = here::here("results", "tables", "table5.rds"))