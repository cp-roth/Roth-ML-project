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
data_location <- here::here("data","processed-data","ML_linear_processed.rds")

#load data
ML_linear <- readRDS(data_location)

## ---- lm1 --------
# Set reference level for ModifiedColor
ML_linear$ModifiedColor <- relevel(ML_linear$ModifiedColor, ref = "Euro-Descent")

# Specify and fit model using linear_reg() (default engine, OLS) from tidymodels
ML_lm1 <- linear_reg() %>% set_engine("lm") %>% fit(Weightgrams ~ ModifiedColor, data = ML_linear)

# Summarize model fit with tidiers
ML_lm1_summary <- summary(ML_lm1) # Summary of fit
tidy(ML_lm1, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_lm1) # Model-level statistics

# Produce predictions for RMSE and R-squared
ML_lm1_pred <- predict(ML_lm1, new_data = ML_linear %>% select(-Weightgrams))
ML_lm1_pred

# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_lm1_pred <- bind_cols(ML_lm1_pred, ML_linear %>% select(Weightgrams))
ML_lm1_pred

# Plot observed versus predicted
plot1 <- ggplot(ML_lm1_pred, aes(x = Weightgrams, y = .pred)) +
  geom_abline(lty = 2) + # Add a dashed line to represent the 1:1 line
  geom_point(alpha = 0.5) +
  labs(x = "Observed", y = "Predicted") + #Scale and size x- and y-axis uniformly
  coord_obs_pred()
plot1

# Create metric set including RMSE and R-squared
metrics_lm1 <- metric_set(rmse, rsq, mae) # MAE for fun
metrics_lm1(ML_lm1_pred, truth = Weightgrams, estimate = .pred)

# Create a gtsummary table directly from the model
table2 <- tbl_regression(ML_lm1, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table2

# Save regression output into table
saveRDS(table2, file = here::here("results", "tables", "table2_final.rds"))

## ---- lm2 --------
# Set reference level for ModifiedColor
ML_linear$ModifiedColor <- relevel(ML_linear$ModifiedColor, ref = "Euro-Descent")

# Specify and fit model using linear_reg() (default engine, OLS) from tidymodels
ML_lm2 <- linear_reg() %>% set_engine("lm") %>% fit(Weightgrams ~ Age, data = ML_linear)

# Summarize model fit with tidiers
ML_lm2_summary <- summary(ML_lm2) # Summary of fit
tidy(ML_lm2, conf.int = TRUE) # Coefficients and confidence intervals
glance(ML_lm2) # Model-level statistics

# Plot data
plot2 <- ggplot(ML_linear, aes(x = Age, y = Weightgrams)) +
  geom_point(alpha = 0.5) +
  labs(x = "Maternal Age", y = "Infant Birth Weight (grams)") + #Scale and size x- and y-axis uniformly
  geom_smooth(method = "lm", se = FALSE) # Add a linear trend line
theme_minimal() #set plot theme
plot2

# Produce predictions for RMSE and R-squared
ML_lm2_pred <- predict(ML_lm2, new_data = ML_linear %>% select(-Weightgrams))
ML_lm2_pred

# Predicted numeric outcome named .pred. Now we will match predicted values with corresponding observed outcome values
ML_lm2_pred <- bind_cols(ML_lm2_pred, ML_linear %>% select(Weightgrams))
ML_lm2_pred

# Create metric set including RMSE and R-squared
metrics_lm1 <- metric_set(rmse, rsq, mae) # MAE for fun
metrics_lm1(ML_lm2_pred, truth = Weightgrams, estimate = .pred)

# Create a gtsummary table directly from the model
table3 <- tbl_regression(ML_lm2, exponentiate = FALSE, intercept = TRUE, show_single_row = everything()) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table3

# Save regression output into table
saveRDS(table3, file = here::here("results", "tables", "table3_final.rds"))

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
table4 <- tbl_regression(ML_lm3, exponentiate = FALSE, intercept = TRUE) %>% 
  bold_p() %>% 
  add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%
  modify_column_unhide(columns = c(statistic, std.error))
table4

# Save regression output into table
saveRDS(table4, file = here::here("results", "tables", "table4_final.rds"))