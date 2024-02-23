## ---- packages --------
#load needed packages. make sure they are installed.
library(tidyverse) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here) #to set paths
library(renv) #for package management
library(knitr) #for nice tables
library(kableExtra) #for nice tables
library(gt) #for nice tables
library(ggplot2) #for plotting
library(hrbrthemes) #for nice themes
hrbrthemes::import_roboto_condensed() #import needed fonts for hrbrthemes
rm(list=ls()) #clear the environment

## ---- loaddata --------
#Path to summary data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","ML_summary_processed.rds")
#load data
ML_summary <- readRDS(data_location)

## ---- table1 --------
# Create summary
summary = skimr::skim(ML_summary)
print(summary)
# save to file
summarytable_file = here("results", "tables", "summarytable.rds")
saveRDS(summary, file = summarytable_file)

## ---- age --------
# Create histogram plot
age_plot <- ML_summary %>% filter(!is.na(Age)) %>% # Filter out NAs
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 0.5, width= 0.8, fill="#69b3a2") +
  geom_vline(xintercept = c(15, 49), linetype = "dashed", color = "red") +  # Add vertical lines that mark reproductive age
  ggtitle("Maternal Age") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
plot(age_plot)
# Save plot to file
figure_file = here("results","figures", "age_distribution.png")
ggsave(filename = figure_file, plot=age_plot, bg="white") #background is white

## ---- Birthweight --------
# Create histogram plot
weight_plot <- ML_summary %>% filter(!is.na(Weightgrams)) %>% #filter out NAs
  ggplot(aes(x = Weightgrams)) +
  geom_histogram(binwidth = 25, fill="#69b3a2") +
  geom_vline(xintercept = c(1500, 4000), linetype = "dashed", color = "red") +  # Add vertical lines that mark normal birthweight
  ggtitle("Infant birthweight") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
plot(weight_plot)
# Save plot to file
figure_file = here("results","figures", "weight_distribution.png")
ggsave(filename = figure_file, plot=weight_plot, bg="white") #background is white

## ---- Birthlength --------
# Create histogram plot
length_plot <- ML_summary %>% filter(!is.na(Lengthcentimeters)) %>% #filter out NAs
  ggplot(aes(x = Lengthcentimeters)) +
  geom_histogram(binwidth = 1, fill="#69b3a2") +
  geom_vline(xintercept = c(49), linetype = "dashed", color = "red") +  # Add vertical lines that mark average birth length for both sexes
  ggtitle("Infant birth length") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
plot(length_plot)
# Save plot to file
figure_file = here("results","figures","length_distribution.png")
ggsave(filename = figure_file, plot=length_plot, bg="white") #background is white

## ---- Heightweight --------
#Create plot
length_weight <- ML_summary %>%
  filter(!is.na(Lengthcentimeters) & !is.na(Weightgrams)) %>% #filter out NAs
  ggplot(aes(x=Lengthcentimeters, y=Weightgrams)) + geom_point() + geom_smooth(method='lm')
plot(length_weight)
#Save file
figure_file = here("results","figures","length_weight.png")
ggsave(filename = figure_file, plot=length_weight, bg="white") #background is white

## ---- Heightweightsex --------
#Create plot
length_weight_sex <- ML_summary %>% filter(!is.na(Lengthcentimeters) & !is.na(Weightgrams) & !is.na(Sex)) %>% 
  ggplot(aes(x=Lengthcentimeters, y=Weightgrams, color = Sex)) + geom_point() + geom_smooth(method='lm')
plot(length_weight_sex)
#Save file
figure_file = here("results","figures","length_weight_sex.png")
ggsave(filename = figure_file, plot=length_weight_sex, bg="white") #background is white

## ---- Summarystatscont --------
# Summary stats of continuous variables
summary_stats <- ML_summary %>%
  summarize(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    mean_weight = mean(Weightgrams, na.rm = TRUE),
    sd_weight = sd(Weightgrams, na.rm = TRUE),
    mean_length = mean(Lengthcentimeters, na.rm = TRUE),
    sd_length = sd(Lengthcentimeters, na.rm = TRUE)
  )
print(summary_stats)
# Make into a table
# Create dataframe
summary_stats1 <- data.frame(
  Variable = c("Maternal Age", "Infant birthweight (grams)", "Infant birth length (cm)"),
  Mean = c(25.30435, 3087.341, 48.33493),
  StandardDev = c(5.759474, 566.5855, 3.92719)
)
# Create a gt table
summary_stats_table <- summary_stats1 %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics",
    subtitle = "Mean Values and Standard Deviations of Continuous Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    Mean = "Mean",
    StandardDev = "Standard Deviation"
  )
# Print the table
print(summary_stats_table)
#Save table
saveRDS(summary_stats_table, file = here::here("results", "tables", "summary_stats_table.rds"))

## ---- Summarystatscat1 --------
# Summary stats of categorical variables
# Calculate frequencies of color category (branca, parda, preta)
color_freq <- ML_summary %>%
  filter(!is.na(Color)) %>% # Exclude rows with NA in color variable
  count(Color, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(color_freq)

## ---- Summarystatscat2 --------
# Combine "Preta" and "Parda" into a new category Afro or EuroDescent and create dummy variable
ML_summary$ModifiedColor <- ifelse(ML_summary$Color == "Branca", "EuroDescent",
                                   ifelse(ML_summary$Color == "Preta", "AfroDescent",
                                          ifelse(ML_summary$Color == "Parda", "AfroDescent", NA)))
# Or using recode factor from dplyr (easier)
# ML_summary$ModifiedColor <- recode_factor(ML_summary$Color, "Parda" = "AfroDescent", "Preta" # = "AfroDescent", "Branca" = "EuroDescent")
# View the updated dataframe
print(head(ML_summary))
# Calculate frequencies of color category (branca, parda, preta)
color_freq2 <- ML_summary %>%
  filter(!is.na(ModifiedColor)) %>% # Exclude rows with NA in color variable
  count(ModifiedColor, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(color_freq2)

## ---- Summarystatscat3 --------
# Calculate frequencies of Status category
status_freq <- ML_summary %>%
  filter(!is.na(Status)) %>% # Exclude rows with NA in status variable
  count(Status, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(status_freq)
# Create dummy variables for status
ML_summary$ModifiedStatus <- recode_factor(ML_summary$Status, "Trigesta" = "Multigesta")
# Calculate frequencies of ModifiedStatus category
status_freq2 <- ML_summary %>%
  filter(!is.na(ModifiedStatus)) %>% # Exclude rows with NA in status variable
  count(ModifiedStatus, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(status_freq2)

## ---- Summarystatscat4 --------
# Calculate frequencies of Nationality category
nationality_freq <- ML_summary %>%
  filter(!is.na(Nationality)) %>% # Exclude rows with NA in nationality variable
  count(Nationality, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(nationality_freq)
# Create dummy variables for status
ML_summary$ModifiedNationality <- recode_factor(ML_summary$Nationality, "Alema" = "European", "Argentina" = "LatinAmerican", "Austriaca" = "European", "Brasileira" = "Brazilian", "Espanhola" = "European", "Francesa" = "European", "Italiana" = "European", "Paraguaya" = "LatinAmerican", "Polaca" = "European", "Portuguesa" = "European", "Rumania" = "European", "Russa" = "European", "Suiça" = "European", "Siria" = "MiddleEastern", "Uruguaya" = "LatinAmerican")
# Calculate frequencies of ModifiedStatus category
nationality_freq2 <- ML_summary %>%
  filter(!is.na(ModifiedNationality)) %>% # Exclude rows with NA in nationality variable
  count(ModifiedNationality, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(nationality_freq2)

## ---- Summarystatscat5 --------
# Calculate frequencies of birth category with abortion
birth_freq <- ML_summary %>%
  filter(!is.na(Birth)) %>% # Exclude rows with NA in birth variable
  count(Birth, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(birth_freq)
# Calculate frequencies of birth category without abortion
birth_freq2 <- ML_summary %>%
  filter(!is.na(Birth)) %>% # Exclude rows with NA in birth variable
  filter(Birth != "aborto") %>% # Exclude rows with abortion
  count(Birth, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(birth_freq2)

## ---- Summarystatscat6 --------
# Calculate frequencies of MaternalOutcome category
maternal_freq <- ML_summary %>%
  filter(!is.na(MaternalOutcome)) %>% # Exclude rows with NA in MaternalOutcome variable
  count(MaternalOutcome, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(maternal_freq)

## ---- Summarystatscat7 --------
# Calculate frequencies of FetalOutcome category excluding abortion
fetal_freq <- ML_summary %>%
  filter(!is.na(FetalOutcome)) %>% # Exclude rows with NA in FetalOutcome variable
  count(FetalOutcome, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  filter(FetalOutcome != "aborto") %>% # Exclude rows with abortion
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(fetal_freq)
#MMR Maternal mortality ratio
maternal_deaths <-23
live_births <- 2440
MMR <- (maternal_deaths / live_births) * 10000
#Stillbirth rate SBR
still_births <- 226
total_births <- 2666
SBR <- (still_births / total_births) * 1000

## ---- Summarystatscat8 --------
# Calculate frequencies of Sex category
sex_freq <- ML_summary %>%
  filter(!is.na(Sex)) %>% # Exclude rows with NA in Sex variable
  count(Sex, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(sex_freq)
