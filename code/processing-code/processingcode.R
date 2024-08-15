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
rm(list=ls()) #clear the environment

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
ML_1 <- ML_raw %>% dplyr::select(-VolN, -Page, -Number, -Nationality_Notes, -Maternal_Notes, -CauseofDeath, -PreviousHistory, -Notes)
#Summarize missing data using dplyr
missing_data_summary <- ML_1 %>% dplyr::summarise_all(list(~sum(is.na(.))))
missing_data_summary
# See values for `FetalOutcome` and `Fetal_Notes` that are not missing any values for both variables
ML_1 %>% filter(!is.na(FetalOutcome) & !is.na(Fetal_Notes))
# Change 'FetalOutcome' to vivo if 'Fetal_Notes' indicates neonatal death (via Faleceu or faleceu in 'Fetal_Notes' column)
ML_2 <- ML_1 %>%
  mutate(FetalOutcome = case_when(
    FetalOutcome == "morto" & 
      str_detect(tolower(Fetal_Notes), "faleceu") ~ "vivo",
    TRUE ~ FetalOutcome
  ))
ML_2
# Check values
ML_2 %>% filter(!is.na(FetalOutcome) & !is.na(Fetal_Notes))
# See how many neonatal deaths were recorded
ML_neonatal <- ML_2 %>%
  filter(
    FetalOutcome == "vivo" &
      str_detect(tolower(Fetal_Notes), "faleceu")
  )
# View the filtered data
print(ML_neonatal)
# Extract information on gestation from Birth_Notes and create new variable gestation with singleton and twin births
ML_3 <- ML_2 %>%
  mutate(Birth_Notes = str_trim(Birth_Notes)) %>% # removes any leading or trailing white space from Birth_Notes column
  mutate(Gestation = if_else(is.na(Birth_Notes) | Birth_Notes == "", "singleton", # checks if Birth_Notes is NA or empty strings and assigns singleton to Gestation column
                             if_else(str_detect(tolower(Birth_Notes), "gemelar"), "twin", "singleton"))) # checks for gemelar in case-insensitive manner and assigns twin accordingly or singleton otherwise
ML_3
# Remove variables with missing data not relevant to hypothesis
ML_4 <- ML_3 %>% dplyr::select(-CivilStatus, -GestationalAge_Months, -Birth_Notes, -Fetal_Notes)
#Summarize missing data using dplyr
missing_data_summary <- ML_4 %>% dplyr::summarise_all(list(~sum(is.na(.))))
missing_data_summary
# Create new variable of NBW and LBW for future logistic regression
ML_5 <- ML_4 %>%
  mutate(BirthweightCategory = case_when(
    Weightgrams >= 500 & FetalOutcome != "morto" & Gestation != "twin" & Weightgrams <= 2500 ~ "LBW",
    Weightgrams >= 500 & FetalOutcome != "morto" & Gestation != "twin" & Weightgrams > 2500 ~ "NBW",
    TRUE ~ NA_character_
  ))
ML_5
# Transform character variables into factors
ML_6 <- ML_5 %>%
  mutate(across(c(Color, Status, Nationality, Birth, MaternalOutcome, FetalOutcome, Sex, Gestation, BirthweightCategory), as.factor))
# Check variables
glimpse(ML_6)
# Check levels of factor variables
map(ML_6[c('Color', 'Status', 'Nationality', 'Birth', 'MaternalOutcome', 'FetalOutcome', 'Sex', 'Gestation', 'BirthweightCategory')], levels)
# Define factor variables to check frequencies
factor_variables <- c('Color', 'Status', 'Nationality', 'Birth', 'MaternalOutcome', 'FetalOutcome', 'Sex', 'Gestation', 'BirthweightCategory')
# Check frequencies of factor variables
frequency_table <- ML_6 %>%
  count(across(all_of(factor_variables)))
print(frequency_table)

## ---- createdatasets --------
# Create dataset for summary statistics (includes missing values)
ML_summary <- ML_6
# Creating dataset for linear regression
ML_linear <- ML_6 %>% tidyr::drop_na() %>%
  filter(Weightgrams >= 500, FetalOutcome != "morto", Gestation != "twin") # Drop observations of < 500 grams, all non-live births, and all twin births for regression dataset
# Check for no missing data in linear model
ML_linear_summary <- ML_linear %>% dplyr::summarise_all(list(~sum(is.na(.))))
ML_linear_summary

## ---- savedata --------
# Save ML_Linear as RDS
save_data_location <- here::here("data","processed-data","ML_linear_processed.rds")
saveRDS(ML_linear, file = save_data_location)
# Save ML_summary as RDS
save_data_location <- here::here("data","processed-data","ML_summary_processed.rds")
saveRDS(ML_summary, file = save_data_location)

## ---- createtables --------
# path to data
# note the use of the here() package and not absolute paths
data_location <- here::here("data","raw-data","Mean_Birthweights.csv")
Mean_Birthweights <- read_csv(data_location)
# Structure of Data
glimpse(Mean_Birthweights)
# Remove median and maternal age variables given missing data
Mean_Birthweights <- Mean_Birthweights %>% dplyr::select(-Median, -`Maternal Age (mean)`, -"<2500gm (%)")
# Structure of data
glimpse(Mean_Birthweights)
# Save Mean_Birthweight as RDS
save_data_location <- here::here("data","processed-data","Mean_Birthweights_processed.rds")
saveRDS(Mean_Birthweights, file = save_data_location)