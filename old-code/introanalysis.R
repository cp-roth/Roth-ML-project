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
rm(list=ls()) #clear the environment

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
table_fit1 <-
  tbl_regression(ML_fit1, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% # make p-values bold
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
#Print table
table_fit1
# Save regression output into table
saveRDS(table_fit1, file = here::here("results", "tables", "table_fit1.rds"))

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
table_fit2 <-
  tbl_regression(ML_fit_age, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
#Print table
table_fit2
# Save regression output into table
saveRDS(table_fit2, file = here::here("results", "tables", "table_fit2.rds"))

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
table_fit3 <- tbl_regression(ML_lm3, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table_fit3
# Save regression output into table
saveRDS(table_fit3, file = here::here("results", "tables", "table_fit3.rds"))

## ---- lm4 --------
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
table_fit4 <- tbl_regression(ML_lm4, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table_fit4
# Save regression output into table
saveRDS(table_fit4, file = here::here("results", "tables", "table_fit4.rds"))
# Complex linear model
# Set reference level for Color
ML_linear$Color <- relevel(ML_linear$Color, ref = "White")
# Specify and fit model using linear_reg() (default engine, OLS) from tidymodels
ML_lm4 <- linear_reg() %>% set_engine("lm") %>% fit(Weightgrams ~ Color + Age + ModifiedStatus + ModifiedNationality, data = ML_linear)
# Summarize model fit with tidiers
ML_lm4_summary <- summary(ML_lm4) # Summary of fit
tidy(ML_lm4, conf.int = TRUE) %>% mutate(p.value = round(p.value, 5)) # Coefficients and confidence intervals with p-values rounded to 5 decimal places
glance(ML_lm4) # Model-level statistics
# Create a gtsummary table directly from the model
table_fit4 <- tbl_regression(ML_lm4, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table_fit4
# Save regression output into table
saveRDS(table_fit4, file = here::here("results", "tables", "table_fit4.rds"))

## ---- log1 --------
#set seed
set.seed(123)
ML_log_split <- initial_split(ML_linear, prop = 0.75, strata = BirthweightCategory)
# Create training and test datasets
ML_log_train <- training(ML_log_split)
ML_log_test <- testing(ML_log_split)
# Simple logistic regression model with ModifiedColor as the predictor
# Check factor levels
levels(ML_linear$BirthweightCategory) # Reference is LBW
# Set the reference level explicitly to NBW
ML_linear <- ML_linear %>%
  mutate(BirthweightCategory = relevel(BirthweightCategory, ref = "NBW"))
# Create recipe
# Create recipe simple logistic model (creating dummy variables and standardizing continuous variables)
log1_recipe <- recipe(BirthweightCategory ~ ModifiedColor, data = ML_linear) #%>%
  #step_dummy(all_nominal(), -all_outcomes()) %>%
  #step_normalize(all_predictors())
# Define model specification
log1_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
# Create workflow
log1_wf <- workflow() %>%
  add_recipe(log1_recipe) %>%
  add_model(log1_spec)
# Fit model
log1_fit <- log1_wf %>%
  fit(data = ML_linear)
# Extract fitted model from workflow
log1_fitted <- extract_fit_parsnip(log1_fit)$fit
# Create a gtsummary table directly from the model
table_log1 <- tbl_regression(log1_fitted, exponentiate = TRUE) %>% # use exponentiate true for ORs
  bold_p(t = 0.05)
table_log1
# Save regression output into table
saveRDS(table_log1, file = here::here("results", "tables", "table_log1.rds"))
# Calculate performance metrics using pseudo-R2
# Extract model metrics using broom
model_metrics1 <- glance(log1_fitted)
# Calculate McFadden's R-squared
# McFadden's R-squared = 1 - (deviance(model) / deviance(null model))
mcfadden_r2_1 <- 1 - (model_metrics1$deviance / model_metrics1$null.deviance)
# Print McFadden's R-squared
print(mcfadden_r2_1)
# Calculate performance metrics using ROC_AUC
# Fit the model on the training data
log1_fit <- log1_wf %>% fit(data = ML_log_train)
# Predict probabilities on the test set
prob_preds1 <- predict(log1_fit, new_data = ML_log_test, type = "prob")
# Combine predictions with true values
results1 <- bind_cols(ML_log_test, prob_preds1)
# Calculate ROC-AUC
roc_auc_result1 <- roc_auc(results1, truth = BirthweightCategory, .pred_LBW)
# Print ROC-AUC
print(roc_auc_result1)
# Create and plot the ROC curve
roc_curve_data1 <- roc_curve(results1, truth = BirthweightCategory, .pred_LBW)
# Plotting the ROC curve
roc_plot1 <- ggplot(roc_curve_data1, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 2) +
  labs(title = "ROC Curve",
       x = "1 - Specificity",
       y = "Sensitivity")
# Print the plot
print(roc_plot1)

## ---- log2 --------
# Simple logistic regression model with Color as the predictor
# Check factor levels
levels(ML_linear$BirthweightCategory) # Reference is LBW
# Set the reference level explicitly to NBW
ML_linear <- ML_linear %>%
  mutate(BirthweightCategory = relevel(BirthweightCategory, ref = "NBW"))
# Create recipe
# Create recipe simple logistic model (creating dummy variables and standardizing continuous variables)
log2_recipe <- recipe(BirthweightCategory ~ Color, data = ML_linear) #%>%
#step_dummy(all_nominal(), -all_outcomes()) %>%
#step_normalize(all_predictors())
# Define model specification
log2_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
# Create workflow
log2_wf <- workflow() %>%
  add_recipe(log2_recipe) %>%
  add_model(log2_spec)
# Fit model
log2_fit <- log2_wf %>%
  fit(data = ML_linear)
# Extract fitted model from workflow
log2_fitted <- extract_fit_parsnip(log2_fit)$fit
# Create a gtsummary table directly from the model
table_log2 <- tbl_regression(log2_fitted, exponentiate = TRUE) %>% # use exponentiate true for ORs
  bold_p(t = 0.05)
table_log2
# Save regression output into table
saveRDS(table_log2, file = here::here("results", "tables", "table_log2.rds"))
# Calculate metrics using pseudo-R2
# Extract model metrics using broom
model_metrics2 <- glance(log2_fitted)
# Calculate McFadden's R-squared
# McFadden's R-squared = 1 - (deviance(model) / deviance(null model))
mcfadden_r2_2 <- 1 - (model_metrics2$deviance / model_metrics2$null.deviance)
# Print McFadden's R-squared
print(mcfadden_r2_2)
# Calculate performance metrics using ROC_AUC
# Fit the model on the training data
log2_fit <- log2_wf %>% fit(data = ML_log_train)
# Predict probabilities on the test set
prob_preds2 <- predict(log2_fit, new_data = ML_log_test, type = "prob")
# Combine predictions with true values
results2 <- bind_cols(ML_log_test, prob_preds2)
# Calculate ROC-AUC
roc_auc_result2 <- roc_auc(results2, truth = BirthweightCategory, .pred_LBW)
# Print ROC-AUC
print(roc_auc_result2)
# Create and plot the ROC curve
roc_curve_data2 <- roc_curve(results2, truth = BirthweightCategory, .pred_LBW)
# Plotting the ROC curve
roc_plot2 <- ggplot(roc_curve_data2, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 2) +
  labs(title = "ROC Curve",
       x = "1 - Specificity",
       y = "Sensitivity")
# Print the plot
print(roc_plot2)

## ---- log3 --------
# Multiple logistic regression model with ModifiedColor as the predictor
# Check factor levels
levels(ML_linear$BirthweightCategory) # Reference is LBW
levels(ML_linear$ModifiedColor) # Reference is Euro-descent
levels(ML_linear$ModifiedStatus) # Reference is multiparous
levels(ML_linear$ModifiedNationality) # Reference is European
# Set the reference level explicitly to NBW
ML_linear <- ML_linear %>%
  mutate(BirthweightCategory = relevel(BirthweightCategory, ref = "NBW"),
         ModifiedNationality = relevel(ModifiedNationality, ref = "Brazilian"))
# Create recipe
# Create recipe simple logistic model (creating dummy variables and standardizing continuous variables)
log3_recipe <- recipe(BirthweightCategory ~ ModifiedColor + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_predictors())
# Define model specification
log3_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
# Create workflow
log3_wf <- workflow() %>%
  add_recipe(log3_recipe) %>%
  add_model(log3_spec)
# Fit model
log3_fit <- log3_wf %>%
  fit(data = ML_linear)
# Extract fitted model from workflow
log3_fitted <- extract_fit_parsnip(log3_fit)$fit
# Create a gtsummary table directly from the model
table_log3 <- tbl_regression(log3_fitted, exponentiate = TRUE) # use exponentiate true for ORs
table_log3 <- bold_p(table_log3, t = 0.05)
table_log3
# Save regression output into table
saveRDS(table_log3, file = here::here("results", "tables", "table_log3.rds"))
# Calculate metrics using pseudo-R2
# Extract model metrics using broom
model_metrics3 <- glance(log3_fitted)
# Calculate McFadden's R-squared
# McFadden's R-squared = 1 - (deviance(model) / deviance(null model))
mcfadden_r2_3 <- 1 - (model_metrics3$deviance / model_metrics3$null.deviance)
# Print McFadden's R-squared
print(mcfadden_r2_3)
# Calculate performance metrics using ROC_AUC
# Fit the model on the training data
log3_fit <- log3_wf %>% fit(data = ML_log_train)
# Predict probabilities on the test set
prob_preds3 <- predict(log3_fit, new_data = ML_log_test, type = "prob")
# Combine predictions with true values
results3 <- bind_cols(ML_log_test, prob_preds3)
# Calculate ROC-AUC
roc_auc_result3 <- roc_auc(results3, truth = BirthweightCategory, .pred_LBW)
# Print ROC-AUC
print(roc_auc_result3)
# Create and plot the ROC curve
roc_curve_data3 <- roc_curve(results3, truth = BirthweightCategory, .pred_LBW)
# Plotting the ROC curve
roc_plot3 <- ggplot(roc_curve_data3, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 2) +
  labs(title = "ROC Curve",
       x = "1 - Specificity",
       y = "Sensitivity")
# Print the plot
print(roc_plot3)

## ---- log4 --------
# Multiple logistic regression model with Color as the predictor
# Check factor levels
levels(ML_linear$BirthweightCategory) # Reference is LBW
levels(ML_linear$Color) # Reference is White
levels(ML_linear$ModifiedStatus) # Reference is multiparous
levels(ML_linear$ModifiedNationality) # Reference is European

# Set the reference level explicitly to NBW
ML_linear <- ML_linear %>%
  mutate(BirthweightCategory = relevel(BirthweightCategory, ref = "NBW"),
         ModifiedNationality = relevel(ModifiedNationality, ref = "Brazilian"))
# Create recipe
# Create recipe simple logistic model (creating dummy variables and standardizing continuous variables)
log4_recipe <- recipe(BirthweightCategory ~ Color + Age + ModifiedStatus + ModifiedNationality, data = ML_linear) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_predictors())
# Define model specification
log4_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
# Create workflow
log4_wf <- workflow() %>%
  add_recipe(log4_recipe) %>%
  add_model(log4_spec)
# Fit model
log4_fit <- log4_wf %>%
  fit(data = ML_linear)
# Extract fitted model from workflow
log4_fitted <- extract_fit_parsnip(log4_fit)$fit
# Create a gtsummary table directly from the model
table_log4 <- tbl_regression(log4_fitted, exponentiate = TRUE) # exponentiate true for ORs
# Bold significant p-values
table_log4 <- bold_p(table_log4, t = 0.05)
table_log4
# Save regression output into table
saveRDS(table_log4, file = here::here("results", "tables", "table_log4.rds"))
# Calculate metrics using pseudo-R2
# Extract model metrics using broom
model_metrics4 <- glance(log4_fitted)
# Calculate McFadden's R-squared
# McFadden's R-squared = 1 - (deviance(model) / deviance(null model))
mcfadden_r2_4 <- 1 - (model_metrics4$deviance / model_metrics4$null.deviance)
# Print McFadden's R-squared
print(mcfadden_r2_4)
# Calculate performance metrics using ROC_AUC
# Fit the model on the training data
log4_fit <- log4_wf %>% fit(data = ML_log_train)
# Predict probabilities on the test set
prob_preds4 <- predict(log4_fit, new_data = ML_log_test, type = "prob")
# Combine predictions with true values
results4 <- bind_cols(ML_log_test, prob_preds4)
# Calculate ROC-AUC
roc_auc_result4 <- roc_auc(results4, truth = BirthweightCategory, .pred_LBW)
# Print ROC-AUC
print(roc_auc_result4)
# Create and plot the ROC curve
roc_curve_data4 <- roc_curve(results4, truth = BirthweightCategory, .pred_LBW)
# Plotting the ROC curve
roc_plot4 <- ggplot(roc_curve_data4, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 2) +
  labs(title = "ROC Curve",
       x = "1 - Specificity",
       y = "Sensitivity")
# Print the plot
print(roc_plot4)

## ---- Stacklogs --------
# Combine the two tables using tbl_stack for unadjusted ORs
table5 <- tbl_stack(list(table_log1, table_log2), group_header = c()) %>%
  as_gt() %>%
  gt::tab_style(style = gt::cell_text(weight = "bold"),
                locations = gt::cells_row_groups(groups = everything()))
table5
# Save regression output into table
saveRDS(table5, file = here("results", "tables", "table5_final.rds"))