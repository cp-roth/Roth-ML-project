## ---- packages --------
#load needed packages. make sure they are installed.
#install.packages("glue")
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
library(glue)
rm(list=ls()) #clear the environment

## ---- loaddata --------
#Path to summary data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","ML_linear.rds")
#load data
ML_linear <- readRDS(data_location)

## ---- traintest --------
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
rngseed = 1234
set.seed(rngseed)
# Create training data
# Put 3/4 of the data into the training set 
data_split <- initial_split(ML_linear, prop = 3/4)
# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

## ---- Model1 --------
# Model 1: Final model for paper using all data
# Model using recipes and workflows
# Define recipe for model
ML_recipe1 <- recipe(Weightgrams ~ ModifiedColor, data = ML_linear) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") #%>% # Set reference level for ModifiedColor
#step_dummy(ModifiedColor) # Convert ModifiedColor to dummy variable. Cannot do this if I am merging tables
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
table_model1 <-
  tbl_regression(ML_fit1, exponentiate = FALSE, intercept = TRUE) %>% 
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>%
  add_glance_table(include = c(r.squared, nobs))
table_model1
#Print table
table_model1
# Save regression output into table
saveRDS(table_model1, file = here::here("results", "tables", "table_model1_final.rds"))

## ---- Model1train --------
# Training model 1
# Model using recipes and workflows
# Define recipe for model
ML_recipe1_train <- recipe(Weightgrams ~ ModifiedColor, data = train_data) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(ModifiedColor) # Convert ModifiedColor to dummy variable
# Specify model
ML_spec1_train <- linear_reg() %>% 
  set_engine("lm")
# Create work flow
ML_wf1_train <- workflow() %>%
  add_recipe(ML_recipe1_train) %>%
  add_model(ML_spec1_train)
# Fit the work flow
ML_fit1_train <- fit(ML_wf1_train, data = train_data)
# Summarize model fit with tidiers
ML_summary1_train <- summary(ML_fit1_train) # Summary of fit
tidy(ML_fit1_train, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit1_train) # Model-level statistics
# Produce predictions for R-squared
ML_fit1_pred_train <- predict(ML_fit1_train, new_data = train_data %>% select(-Weightgrams))
ML_fit1_pred_train
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit1_pred_train <- bind_cols(ML_fit1_pred_train, train_data %>% select(Weightgrams))
ML_fit1_pred_train
# Plot observed versus predicted
plot1_train <- ggplot(ML_fit1_pred_train, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot1_train
# Create metric set including RMSE and R-squared
metrics_fit1_train <- metric_set(rmse, rsq)
metrics_fit1_train(ML_fit1_pred_train, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit1_train <- as.data.frame(metrics_fit1_train(ML_fit1_pred_train, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit1_train
# Create a gtsummary table directly from the model
table_model1_train <-
  tbl_regression(ML_fit1_train, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table_model1_train
# Save regression output into table
saveRDS(table_model1_train, file = here::here("results", "tables", "table_model1_train.rds"))

## ---- Model1cv --------
# CV model 1
# Set the seed for reproducibility
set.seed(rngseed)
# Create 10-fold cross-validation
folds <- vfold_cv(train_data, v = 10)
folds
# Define recipe for model
ML_recipe1_cv <- recipe(Weightgrams ~ ModifiedColor, data = train_data) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(ModifiedColor) # Convert ModifiedColor to dummy variable
# Set model specification
ML_spec1_cv <- linear_reg() %>% set_engine("lm")
# Create and initialize workflow that bundles together model specification and formula.
ML_wf1_cv <- workflow() %>% 
  add_model(ML_spec1_cv) %>% 
  add_recipe(ML_recipe1_cv)
fit_resamples(ML_wf1_cv, resamples = folds)
# Resample the model
ML_wf1_cv_rs <- fit_resamples(ML_wf1_cv, resamples = folds)
ML_wf1_cv_rs
# Get model metrics
ML_cv1_metrics <- collect_metrics(ML_wf1_cv_rs)
# Filter the data frame to include only rows with "rmse" and "rsq" in the ".metric" column
ML_cv1_metrics <- ML_cv1_metrics[ML_cv1_metrics$.metric %in% c("rmse", "rsq"), !(names(ML_cv1_metrics) %in% ".config")]
ML_cv1_metrics
# Save regression output into table
saveRDS(ML_cv1_metrics, file = here::here("results", "tables", "ML_cv1_metrics.rds"))

## ---- Model1test --------
# Testing model 1
# Model using recipes and workflows
# Define recipe for model
ML_recipe1_test <- recipe(Weightgrams ~ ModifiedColor, data = test_data) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(ModifiedColor) # Convert ModifiedColor to dummy variable
# Specify model
ML_spec1_test <- linear_reg() %>% 
  set_engine("lm")
# Create work flow
ML_wf1_test <- workflow() %>%
  add_recipe(ML_recipe1_test) %>%
  add_model(ML_spec1_test)
# Fit the work flow
ML_fit1_test <- fit(ML_wf1_test, data = test_data)
# Summarize model fit with tidiers
ML_summary1_test <- summary(ML_fit1_test) # Summary of fit
tidy(ML_fit1_test, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit1_test) # Model-level statistics
# Produce predictions for R-squared
ML_fit1_pred_test <- predict(ML_fit1_test, new_data = test_data %>% select(-Weightgrams))
ML_fit1_pred_test
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit1_pred_test <- bind_cols(ML_fit1_pred_test, test_data %>% select(Weightgrams))
ML_fit1_pred_test
# Plot observed versus predicted
plot1_test <- ggplot(ML_fit1_pred_test, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot1_test
# Create metric set including RMSE and R-squared
metrics_fit1_test <- metric_set(rmse, rsq)
metrics_fit1_test(ML_fit1_pred_test, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit1_test <- as.data.frame(metrics_fit1_test(ML_fit1_pred_test, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit1_test
# Create a gtsummary table directly from the model
table_model1_test <-
  tbl_regression(ML_fit1_test, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table_model1_test
# Save regression output into table
saveRDS(table_model1_test, file = here::here("results", "tables", "table_model1_test.rds"))

## ---- Model2 --------
# Final model 2 for paper using all data
# Using recipes and workflows
# Define recipe for model
ML_recipe2 <- recipe(Weightgrams ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") #%>% # Set reference level for ModifiedColor
#step_dummy(ModifiedColor) #%>% # Convert all nominal variables to dummy variables. Cannot do this if I am merging tables in gtsummary
#step_normalize(all_numeric_predictors()) # Normalize all numeric predictors. Cannot do this if I am merging tables in gtsummary
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
# Produce predictions for R-squared
ML_fit2_pred <- predict(ML_fit2, new_data = ML_linear %>% select(-Weightgrams))
ML_fit2_pred
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit2_pred <- bind_cols(ML_fit2_pred, ML_linear %>% select(Weightgrams))
ML_fit2_pred
# Plot observed versus predicted
plot2 <- ggplot(ML_fit2_pred, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot2
# Create metric set including RMSE and R-squared
metrics_fit2 <- metric_set(rmse, rsq)
metrics_fit2(ML_fit2_pred, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit2 <- as.data.frame(metrics_fit2(ML_fit2_pred, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit2
# Create a gtsummary table directly from the model
table_model2 <- tbl_regression(ML_fit2, exponentiate = FALSE, intercept = TRUE) %>%
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>%
  add_glance_table(include = c(r.squared, nobs))
table_model2
# Save regression output into table
saveRDS(table_model2, file = here::here("results", "tables", "table_model2_final.rds"))

## ---- Model2train --------
# Training model 2
# Using recipes and workflows
# Define recipe for model
ML_recipe2_train <- recipe(Weightgrams ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = train_data) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
# Define model specification
ML_spec2_train <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")
# Create workflow
ML_wf2_train <- workflow() %>%
  add_recipe(ML_recipe2_train) %>%
  add_model(ML_spec2_train)
# Fit the workflow
ML_fit2_train <- fit(ML_wf2_train, data = train_data)
# Summarize model fit with tidiers
ML_lm2_summary_train <- summary(ML_fit2_train) # Summary of fit
tidy(ML_fit2_train, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_fit2_train) # Model-level statistics
# Produce predictions for R-squared
ML_fit2_pred_train <- predict(ML_fit2_train, new_data = train_data %>% select(-Weightgrams))
ML_fit2_pred_train
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit2_pred_train <- bind_cols(ML_fit2_pred_train, train_data %>% select(Weightgrams))
ML_fit2_pred_train
# Plot observed versus predicted
plot2_train <- ggplot(ML_fit2_pred_train, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot2_train
# Create metric set including RMSE and R-squared
metrics_fit2_train <- metric_set(rmse, rsq)
metrics_fit2_train(ML_fit2_pred_train, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit2_train <- as.data.frame(metrics_fit2_train(ML_fit2_pred_train, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit2_train
# Create a gtsummary table directly from the model
table_model2_train <- pull_workflow_fit(ML_fit2_train) %>%
  tbl_regression(exponentiate = FALSE, intercept = TRUE) %>%
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>%
  bold_p()
table_model2_train
# Save regression output into table
saveRDS(table_model2_train, file = here::here("results", "tables", "table_model2_train.rds"))

## ---- Model2cv --------
# CV model 2
# Set the seed for reproducibility
set.seed(rngseed)
# Create 10-fold cross-validation
folds <- vfold_cv(train_data, v = 10)
folds
# Define recipe for model
ML_recipe2_cv <- recipe(Weightgrams ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = train_data) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
# Set model specification
ML_spec2_cv <- linear_reg() %>% set_engine("lm")
# Create and initialize workflow that bundles together model specification and formula.
ML_wf2_cv <- workflow() %>% 
  add_model(ML_spec2_cv) %>% 
  add_recipe(ML_recipe2_cv)
fit_resamples(ML_wf2_cv, resamples = folds)
# Resample the model
ML_wf2_cv_rs <- fit_resamples(ML_wf2_cv, resamples = folds)
ML_wf2_cv_rs
# Get model metrics
ML_cv2_metrics <- collect_metrics(ML_wf2_cv_rs)
# Filter the data frame to include only rows with "rmse" and "rsq" in the ".metric" column
ML_cv2_metrics <- ML_cv2_metrics[ML_cv2_metrics$.metric %in% c("rmse", "rsq"), !(names(ML_cv2_metrics) %in% ".config")]
ML_cv2_metrics
# Save regression output into table
saveRDS(ML_cv2_metrics, file = here::here("results", "tables", "ML_cv2_metrics.rds"))

## ---- Model2test --------
# Testing model 2
# Using recipes and workflows
# Define recipe for model
ML_recipe2_test <- recipe(Weightgrams ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = test_data) %>% 
  step_relevel(ModifiedColor, ref_level = "Euro-Descent") %>% # Set reference level for ModifiedColor
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
# Define model specification
ML_spec2_test <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")
# Create workflow
ML_wf2_test <- workflow() %>%
  add_recipe(ML_recipe2_test) %>%
  add_model(ML_spec2_test)
# Fit the workflow
ML_fit2_test <- fit(ML_wf2_test, data = test_data)
# Summarize model fit with tidiers
ML_lm2_summary_test <- summary(ML_fit2_test) # Summary of fit
tidy(ML_fit2_test, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_fit2_test) # Model-level statistics
# Produce predictions for R-squared
ML_fit2_pred_test <- predict(ML_fit2_test, new_data = test_data %>% select(-Weightgrams))
ML_fit2_pred_test
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit2_pred_test <- bind_cols(ML_fit2_pred_test, test_data %>% select(Weightgrams))
ML_fit2_pred_test
# Plot observed versus predicted
plot2_test <- ggplot(ML_fit2_pred_test, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot2_test
# Create metric set including RMSE and R-squared
metrics_fit2_test <- metric_set(rmse, rsq)
metrics_fit2_test(ML_fit2_pred_test, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit2_test <- as.data.frame(metrics_fit2_test(ML_fit2_pred_test, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit2_test
# Create a gtsummary table directly from the model
table_model2_test <- pull_workflow_fit(ML_fit2_test) %>%
  tbl_regression(exponentiate = FALSE, intercept = TRUE) %>%
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>%
  bold_p()
table_model2_test
# Save regression output into table
saveRDS(table_model2_test, file = here::here("results", "tables", "table_model2_test.rds"))

## ---- Merge1-2 --------
# Combine the two tables using tbl_merge
table3 <- tbl_merge(list(table_model1, table_model2), tab_spanner = c("**Model 1**", "**Model 2**")) %>%
  modify_table_body(fun = ~.x %>% arrange(variable))
table3
# Save regression output into table
saveRDS(table3, file = here::here("results", "tables", "table3_final.rds"))

## ---- Model3 --------
# Final model 3 for paper using all data
# Using recipes and workflows
# Simple linear model
# Specify recipe
ML_recipe3 <- recipe(Weightgrams ~ Color, data = ML_linear) %>%
  step_relevel(Color, ref_level = "White") #%>% # Set reference level for White
#step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
#step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
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
# Produce predictions for R-squared
ML_fit3_pred <- predict(ML_fit3, new_data = ML_linear %>% select(-Weightgrams))
ML_fit3_pred
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit3_pred <- bind_cols(ML_fit3_pred, ML_linear %>% select(Weightgrams))
ML_fit3_pred
# Plot observed versus predicted
plot3 <- ggplot(ML_fit3_pred, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot3
# Create metric set including RMSE and R-squared
metrics_fit3 <- metric_set(rmse, rsq)
metrics_fit3(ML_fit3_pred, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit3 <- as.data.frame(metrics_fit3(ML_fit3_pred, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit3
# Create a gtsummary table from model
table_model3 <- tbl_regression(ML_fit3, exponentiate = FALSE, intercept = TRUE) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>%
  add_glance_table(include = c(r.squared, nobs))
table_model3
# Save regression output into table
saveRDS(table_model3, file = here("results", "tables", "table_model3_final.rds"))

## ---- Model3train --------
# Training model 3
# Using recipes and workflows
# Specify recipe
ML_recipe3_train <- recipe(Weightgrams ~ Color, data = train_data) %>%
  step_relevel(Color, ref_level = "White")
# Specify model
ML_spec3_train <- linear_reg() %>%
  set_engine("lm")
# Create workflow
ML_wf3_train <- workflow() %>%
  add_recipe(ML_recipe3_train) %>%
  add_model(ML_spec3_train)
# Fit the workflow
ML_fit3_train <- fit(ML_wf3_train, data = train_data)
# Summarize model fit with tidiers and create a gtsummary table directly from the model
ML_summary3_train <- summary(ML_fit3_train)
tidy(ML_fit3_train, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit3_train) # Model-level statistics
# Produce predictions for R-squared
ML_fit3_pred_train <- predict(ML_fit3_train, new_data = train_data %>% select(-Weightgrams))
ML_fit3_pred_train
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit3_pred_train <- bind_cols(ML_fit3_pred_train, train_data %>% select(Weightgrams))
ML_fit3_pred_train
# Plot observed versus predicted
plot3_train <- ggplot(ML_fit3_pred_train, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot3_train
# Create metric set including RMSE and R-squared
metrics_fit3_train <- metric_set(rmse, rsq)
metrics_fit3_train(ML_fit3_pred_train, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit3_train <- as.data.frame(metrics_fit3_train(ML_fit3_pred_train, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit3_train
# Create a gtsummary table from model
table_model3_train <- tbl_regression(ML_fit3_train, exponentiate = FALSE, intercept = TRUE) %>%
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE)
table_model3_train
# Save regression output into table
saveRDS(table_model3_train, file = here("results", "tables", "table_model3_train.rds"))

## ---- Model3cv --------
# CV model 3
# Set the seed for reproducibility
set.seed(rngseed)
# Create 10-fold cross-validation
folds <- vfold_cv(train_data, v = 10)
folds
# Define recipe for model
ML_recipe3_cv <- recipe(Weightgrams ~ Color, data = train_data) %>%
  step_relevel(Color, ref_level = "White")
# Set model specification
ML_spec3_cv <- linear_reg() %>% set_engine("lm")
# Create and initialize workflow that bundles together model specification and formula.
ML_wf3_cv <- workflow() %>% 
  add_model(ML_spec3_cv) %>% 
  add_recipe(ML_recipe3_cv)
fit_resamples(ML_wf3_cv, resamples = folds)
# Resample the model
ML_wf3_cv_rs <- fit_resamples(ML_wf3_cv, resamples = folds)
ML_wf3_cv_rs
# Get model metrics
collect_metrics(ML_wf3_cv_rs)
# Get model metrics
ML_cv3_metrics <- collect_metrics(ML_wf3_cv_rs)
# Filter the data frame to include only rows with "rmse" and "rsq" in the ".metric" column
ML_cv3_metrics <- ML_cv3_metrics[ML_cv3_metrics$.metric %in% c("rmse", "rsq"), !(names(ML_cv3_metrics) %in% ".config")]
ML_cv3_metrics
# Save regression output into table
saveRDS(ML_cv3_metrics, file = here::here("results", "tables", "ML_cv3_metrics.rds"))

## ---- Model3test --------
# Testing model 3
# Using recipes and workflows
# Specify recipe
ML_recipe3_test <- recipe(Weightgrams ~ Color, data = test_data) %>%
  step_relevel(Color, ref_level = "White")
# Specify model
ML_spec3_test <- linear_reg() %>%
  set_engine("lm")
# Create workflow
ML_wf3_test <- workflow() %>%
  add_recipe(ML_recipe3_test) %>%
  add_model(ML_spec3_test)
# Fit the workflow
ML_fit3_test <- fit(ML_wf3_test, data = test_data)
# Summarize model fit with tidiers and create a gtsummary table directly from the model
ML_summary3_test <- summary(ML_fit3_test)
tidy(ML_fit3_test, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit3_test) # Model-level statistics
# Produce predictions for R-squared
ML_fit3_pred_test <- predict(ML_fit3_test, new_data = test_data %>% select(-Weightgrams))
ML_fit3_pred_test
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit3_pred_test <- bind_cols(ML_fit3_pred_test, test_data %>% select(Weightgrams))
ML_fit3_pred_test
# Plot observed versus predicted
plot3_test <- ggplot(ML_fit3_pred_test, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot3_test
# Create metric set including RMSE and R-squared
metrics_fit3_test <- metric_set(rmse, rsq)
metrics_fit3_test(ML_fit3_pred_test, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit3_test <- as.data.frame(metrics_fit3_test(ML_fit3_pred_test, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit3_test
# Create a gtsummary table from model
table_model3_test <- tbl_regression(ML_fit3_test, exponentiate = FALSE, intercept = TRUE) %>%
  bold_p() %>%
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table_model3_test
# Save regression output into table
saveRDS(table_model3_test, file = here("results", "tables", "table_model3_test.rds"))

## ---- Model4 --------
# Final model 4 for paper using all data
# Using recipes and workflows
# Complex linear model
# Specify recipe
ML_recipe4 <- recipe(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>%
  step_relevel(Color, ref_level = "White") %>% # Set reference level for Color
  step_relevel(ModifiedStatus, ref_level = "Multiparous") #%>% # Set reference level for ModifiedStatus
#step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
#step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
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
# Produce predictions for R-squared
ML_fit4_pred <- predict(ML_fit4, new_data = ML_linear %>% select(-Weightgrams))
ML_fit4_pred
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit4_pred <- bind_cols(ML_fit4_pred, ML_linear %>% select(Weightgrams))
ML_fit4_pred
# Plot observed versus predicted
plot4 <- ggplot(ML_fit4_pred, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot4
# Create metric set including RMSE and R-squared
metrics_fit4 <- metric_set(rmse, rsq)
metrics_fit4(ML_fit4_pred, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit4 <- as.data.frame(metrics_fit4(ML_fit4_pred, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit4
# Create a gtsummary table from model
table_model4 <- tbl_regression(ML_fit4, exponentiate = FALSE, intercept = TRUE) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>% # Hide confidence intervals and p-values
  add_glance_table(include = c(r.squared, nobs)) # Add R-squared and number of observations
table_model4
# Save regression output into table
saveRDS(table_model4, file = here("results", "tables", "table_model4_final.rds"))

## ---- Model4train --------
# Model 4 training
# Using recipes and workflows
# Complex linear model
# Specify recipe
ML_recipe4_train <- recipe(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = train_data) %>%
  step_relevel(Color, ref_level = "White") %>% # Set reference level for Color
  step_relevel(ModifiedStatus, ref_level = "Multiparous") %>% # Set reference level for ModifiedStatus
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
# Specify model
ML_spec4_train <- linear_reg() %>%
  set_engine("lm")
# Create workflow
ML_wf4_train <- workflow() %>%
  add_recipe(ML_recipe4_train) %>%
  add_model(ML_spec4_train)
# Fit the workflow
ML_fit4_train <- fit(ML_wf4_train, data = train_data)
# Summarize model fit with tidiers and create a gtsummary table directly from the model
ML_summary4_train <- summary(ML_fit4_train)
tidy(ML_fit4_train, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit4_train) # Model-level statistics
# Produce predictions for R-squared
ML_fit4_pred_train <- predict(ML_fit4_train, new_data = train_data %>% select(-Weightgrams))
ML_fit4_pred_train
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit4_pred_train <- bind_cols(ML_fit4_pred_train, train_data %>% select(Weightgrams))
ML_fit4_pred_train
# Plot observed versus predicted
plot4_train <- ggplot(ML_fit4_pred_train, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot4_train
# Create metric set including RMSE and R-squared
metrics_fit4_train <- metric_set(rmse, rsq)
metrics_fit4_train(ML_fit4_pred_train, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit4_train <- as.data.frame(metrics_fit4_train(ML_fit4_pred_train, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit4_train
# Create a gtsummary table from model
table_model4_train <- tbl_regression(ML_fit4_train, exponentiate = FALSE, intercept = TRUE) %>%
  bold_p() %>%
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table_model4_train
# Save regression output into table
saveRDS(table_model4_train, file = here("results", "tables", "table_model4_train.rds"))

## ---- Model4cv --------
# CV model 4
# Set the seed for reproducibility
set.seed(rngseed)
# Create 10-fold cross-validation
folds <- vfold_cv(train_data, v = 10)
folds
# Define recipe for model
ML_recipe4_cv <- recipe(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>%
  step_relevel(Color, ref_level = "White") %>% # Set reference level for Color
  step_relevel(ModifiedStatus, ref_level = "Multiparous") %>% # Set reference level for ModifiedStatus
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
# Set model specification
ML_spec4_cv <- linear_reg() %>% set_engine("lm")
# Create and initialize workflow that bundles together model specification and formula.
ML_wf4_cv <- workflow() %>% 
  add_model(ML_spec4_cv) %>% 
  add_recipe(ML_recipe4_cv)
fit_resamples(ML_wf4_cv, resamples = folds)
# Resample the model
ML_wf4_cv_rs <- fit_resamples(ML_wf4_cv, resamples = folds)
ML_wf4_cv_rs
# Get model metrics
collect_metrics(ML_wf4_cv_rs)
# Get model metrics
ML_cv4_metrics <- collect_metrics(ML_wf4_cv_rs)
# Filter the data frame to include only rows with "rmse" and "rsq" in the ".metric" column
ML_cv4_metrics <- ML_cv4_metrics[ML_cv4_metrics$.metric %in% c("rmse", "rsq"), !(names(ML_cv4_metrics) %in% ".config")]
ML_cv4_metrics
# Save regression output into table
saveRDS(ML_cv4_metrics, file = here::here("results", "tables", "ML_cv4_metrics.rds"))

## ---- Model4test --------
# Model 4 testing
# Using recipes and workflows
# Complex linear model
# Specify recipe
ML_recipe4_test <- recipe(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = test_data) %>%
  step_relevel(Color, ref_level = "White") %>% # Set reference level for Color
  step_relevel(ModifiedStatus, ref_level = "Multiparous") %>% # Set reference level for ModifiedStatus
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal variables to dummy variables
  step_normalize(all_numeric_predictors()) # Normalize all numeric predictors
# Specify model
ML_spec4_test <- linear_reg() %>%
  set_engine("lm")
# Create workflow
ML_wf4_test <- workflow() %>%
  add_recipe(ML_recipe4_test) %>%
  add_model(ML_spec4_test)
# Fit the workflow
ML_fit4_test <- fit(ML_wf4_test, data = test_data)
# Summarize model fit with tidiers and create a gtsummary table directly from the model
ML_summary4_test <- summary(ML_fit4_test)
tidy(ML_fit4_test, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_fit4_test) # Model-level statistics
# Produce predictions for R-squared
ML_fit4_pred_test <- predict(ML_fit4_test, new_data = test_data %>% select(-Weightgrams))
ML_fit4_pred_test
# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_fit4_pred_test <- bind_cols(ML_fit4_pred_test, test_data %>% select(Weightgrams))
ML_fit4_pred_test
# Plot observed versus predicted
plot4_test <- ggplot(ML_fit4_pred_test, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot4_test
# Create metric set including RMSE and R-squared
metrics_fit4_test <- metric_set(rmse, rsq)
metrics_fit4_test(ML_fit4_pred_test, truth = Weightgrams, estimate = .pred)
# Convert the output to a data frame and mutate the .estimate column to numeric
metrics_fit4_test <- as.data.frame(metrics_fit4_test(ML_fit4_pred_test, truth = Weightgrams, estimate = .pred)) %>%
  mutate(.estimate = as.numeric(.estimate))
options(scipen = 999) # set opens to avoid scientific notation
metrics_fit4_test
# Create a gtsummary table from model
table_model4_test <- tbl_regression(ML_fit4_test, exponentiate = FALSE, intercept = TRUE) %>%
  bold_p() %>%
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error)) %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
table_model4_test
# Save regression output into table
saveRDS(table_model4_test, file = here("results", "tables", "table_model4_test.rds"))

## ---- Merge3-4 --------
# Combine the two tables using tbl_merge
table4 <- tbl_merge(list(table_model3, table_model4), tab_spanner = c("**Model 3**", "**Model 4**")) %>%
  modify_table_body(fun = ~.x %>% arrange(variable))
table4
# Save regression output into table
saveRDS(table4, file = here("results", "tables", "table4_final.rds"))