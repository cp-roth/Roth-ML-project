###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder
#
# Note the ## ---- name ---- notation
# This is done so one can pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html


## ---- packages --------
#Load needed packages. Make sure they are installed.
library(readxl) #for loading Excel files
library(tidyverse) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here) #to set paths
library(renv) #for package management
library(knitr) #for nice tables
library(kableExtra) #for nice tables

## ---- loaddata --------
# path to data
# note the use of the here() package and not absolute paths
data_location <- here::here("data","raw-data","MaternidadeLaranjeiras.csv")
ML_raw <- read_csv(data_location)
# Structure of Data
glimpse(ML_raw)

## ---- checkdata --------
# Path to codebook
codebook_location <- here::here("data", "raw-data", "Codebook.csv")
codebook <- read_csv(codebook_location)
# Display the structure of the data frame
str(codebook)
# Display the first few rows of the data frame
head(codebook)
# Create table
codebook_table <- kable(codebook, caption = "Codebook") %>% kable_styling()
#Print table
print(codebook_table)

## ---- exploredata --------
# Looking at the data
dplyr::glimpse(ML_raw)
summary(ML_raw)
head(ML_raw)
skimr::skim(ML_raw)

## ---- cleandata --------
# Dataset created for multiple reasons. Need to narrow down relevant variables
# Remove unnecessary variables
ML_1 <- ML_raw %>% dplyr::select(-VolN, -Page, -Number, -Nationality_Notes, -Birth_Notes, -Maternal_Notes, -Fetal_Notes, -CauseofDeath, -PreviousHistory, -Notes)
#Summarize missing data using dplyr
missing_data_summary <- ML_1 %>% dplyr::summarise_all(funs(sum(is.na(.))))
# Remove variables with missing data not relevant to hypothesis
ML_2 <- ML_1 %>% dplyr::select(-CivilStatus, -GestationalAge_Months)
#Summarize missing data using dplyr
missing_data_summary <- ML_2 %>% dplyr::summarise_all(funs(sum(is.na(.))))
View(missing_data_summary)
# Transform character variables into factors
ML_3 <- ML_2 %>%
  mutate(across(c(Color, Status, Nationality, Birth, MaternalOutcome, FetalOutcome, Sex), as.factor))
# Check variables
glimpse(ML_3)
# Check levels of factor variables
map(ML_3[c('Color', 'Status', 'Nationality', 'Birth', 'MaternalOutcome', 'FetalOutcome', 'Sex')], levels)
# Define factor variables to check frequencies
factor_variables <- c('Color', 'Status', 'Nationality', 'Birth', 'MaternalOutcome', 'FetalOutcome', 'Sex')
# Check frequencies of factor variables
frequency_table <- ML_3 %>%
  count(across(all_of(factor_variables)))
print(frequency_table)

## ---- createdatasets --------
# Create dataset for summary statistics (includes missing values)
ML_summary <- ML_3
# Creating dataset for linear regression
ML_linear <- ML_3 %>% tidyr::drop_na() %>%
  filter(Weightgrams >= 500, FetalOutcome != "morto") # Drop observations of < 500 grams and all non-live births
# Check for no missing data in linear model
ML_Linear_summary <- ML_linear %>% dplyr::summarise_all(funs(sum(is.na(.))))
View(ML_linear_summary)

## ---- savedata --------
# Save ML_Linear as RDS
save_data_location <- here::here("data","processed-data","ML_linear_processed.rds")
saveRDS(ML_linear, file = save_data_location)
# Save ML_summary as RDS
save_data_location <- here::here("data","processed-data","ML_summary_processed.rds")
saveRDS(ML_summary, file = save_data_location)