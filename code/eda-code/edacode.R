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

## ---- Dummyvariables --------
# Create dummy variables

# Recode factor using dplyr (easier)
ML_summary$ModifiedColor <- recode_factor(ML_summary$Color, "Parda" = "AfroDescent", "Preta" = "AfroDescent", "Branca" = "EuroDescent")

# Create dummy variables for status and change to English
ML_summary$ModifiedStatus <- recode_factor(ML_summary$Status, "Secundigesta" = "Multiparous", "Trigesta" = "Multiparous", "Multigesta" = "Multiparous", "Secundipara" = "Multiparous", "Multipara" = "Multiparous", "Primigesta" = "Nulliparous", "Nulipara" = "Nulliparous", "Primipara" = "Nulliparous")

# Create dummy variables for nationality and change to English
ML_summary$ModifiedNationality <- recode_factor(ML_summary$Nationality, "Alema" = "European", "Argentina" = "LatinAmerican", "Austriaca" = "European", "Brasileira" = "Brazilian", "Espanhola" = "European", "Francesa" = "European", "Italiana" = "European", "Paraguaya" = "LatinAmerican", "Polaca" = "European", "Portuguesa" = "European", "Rumania" = "European", "Russa" = "European", "Suiça" = "European", "Siria" = "MiddleEastern", "Uruguaya" = "LatinAmerican")

# View the updated dataframe
print(head(ML_summary))

## ---- LanguageChange --------
# Change observations from Portuguese to English
ML_summary <- ML_summary %>%
  mutate(
    ModifiedColor = recode_factor(ModifiedColor, "AfroDescent" = "Afro-Descent", "EuroDescent" = "Euro-Descent"),
    ModifiedNationality = recode_factor(ModifiedNationality, "European" = "European", "LatinAmerican" = "Latin American", "MiddleEastern" = "Middle Eastern", "Brazilian" = "Brazilian"),
    Color = recode_factor(Color, "Preta" = "Black", "Parda" = "Mixed Race", "Branca" = "White"),
    Nationality = recode_factor(Nationality, "Alema" = "German", "Argentina" = "Argentine", "Austriaca" = "Austrian", "Brasileira" = "Brazilian", "Espanhola" = "Spanish", "Francesa" = "French", "Italiana" = "Italian", "Paraguaya" = "Paraguayan", "Polaca" = "Polish", "Portuguesa" = "Portuguese", "Rumania" = "Romanian", "Russa" = "Russian", "Suiça" = "Swiss", "Siria" = "Syrian", "Uruguaya" = "Uruguayan"),
    Birth = recode_factor(Birth, "aborto" = "Abortion", "intervencionista" = "Interventionist", "natural" = "Natural", "operatório" = "Operative"),
    MaternalOutcome = recode_factor(MaternalOutcome, "alta" = "Discharged", "morta" = "Death", "transferência" = "Hospital transferal"),
    FetalOutcome = recode_factor(FetalOutcome, "vivo" = "Live Birth", "morto" = "Stillbirth or Neonatal Death"),
    Status = recode_factor(Status, "Multigesta" = "Multigravida", "Trigesta" = "Trigravida", "Secundigesta" = "Secundigravida", "Secundipara" = "Secundiparous", "Multipara" = "Multiparous", "Primigesta" = "Primigravida", "Nulipara" = "Nulliparous", "Primipara" = "Primiparous")
  )

# Save new summary data with recoded factor variables
saveRDS(ML_summary, file = here("data", "processed-data", "ML_summary.rds"))

## ---- Summarystats --------
# Create gtsummary table 
table1 <- 
  ML_summary %>%
  tbl_summary(
    include = c(Color, ModifiedColor, Status, ModifiedStatus, Age, Nationality, ModifiedNationality, Birth, MaternalOutcome, FetalOutcome, Sex, Weightgrams, Lengthcentimeters),
    missing = "no",
    label = list(ModifiedColor ~ "Ancestry", Status ~ "Parity or Gravidity", ModifiedStatus ~ "Parity", Age ~ "Maternal Age", ModifiedNationality ~ "Combined Nationality", Birth ~ "Birth Outcome", MaternalOutcome ~ "Maternal Outcome", FetalOutcome ~ "Fetal Outcome", Weightgrams ~ "Infant Birthweight (grms)", Lengthcentimeters ~ "Infant Birth Length (cms)")
  ) %>%
  add_n()
table1

#Save table
saveRDS(table1, file = here::here("results", "tables", "table1_final.rds"))

## ---- Summarystatscont --------
# Calculate frequencies of birth category without abortion
birth_freq2 <- ML_summary %>%
  filter(!is.na(Birth)) %>% # Exclude rows with NA in birth variable
  filter(Birth != "abortion") %>% # Exclude rows with abortion
  count(Birth, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(birth_freq2)

## ---- MMR/SBR/SRaB --------
#MMR Maternal mortality ratio
maternal_deaths <-23
live_births <- 2440
MMR <- (maternal_deaths / live_births) * 10000
MMR

#Stillbirth rate SBR
still_births <- 226
total_births <- 2666
SBR <- (still_births / total_births) * 1000
SBR

#Sex ratio at birth (live male births/live female births)*100
sex_ratio <- ML_summary %>%
  filter(!Birth %in% c("Abortion") & !FetalOutcome %in% c("Stillbirth or Neonatal Death")) %>% # Exclude rows with abortion and stillbirth
  filter(!is.na(Sex) & !is.na(Birth) & !is.na(FetalOutcome)) %>%
  group_by(Sex) %>%
  summarise(count = n()) %>%
  spread(key = Sex, value = count) %>%
  mutate(SexRatio = M / F) * 100
sex_ratio

##---- LinearData --------
#Path to summary data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","ML_linear_processed.rds")
#load data
ML_linear <- readRDS(data_location)

# Create dummy variables
# Create dummy variables

#Check pre-structure
str(ML_linear)

# Create dummy variables using recode factor from dplyr
ML_linear$ModifiedColor <- recode_factor(ML_linear$Color, "Preta" = "Afro-Descent", "Parda" = "Afro-Descent", "Branca" = "Euro-Descent")

# Create dummy variables for status and change to English
ML_linear$ModifiedStatus <- recode_factor(ML_linear$Status, "Secundigesta" = "Multiparous", "Trigesta" = "Multiparous", "Multigesta" = "Multiparous", "Secundipara" = "Multiparous", "Multipara" = "Multiparous", "Primigesta" = "Nulliparous", "Nulipara" = "Nulliparous", "Primipara" = "Nulliparous")

# Create dummy variables for nationality and change to English
ML_linear$ModifiedNationality <- recode_factor(ML_linear$Nationality, "Alema" = "European", "Argentina" = "LatinAmerican", "Austriaca" = "European", "Brasileira" = "Brazilian", "Espanhola" = "European", "Francesa" = "European", "Italiana" = "European", "Paraguaya" = "LatinAmerican", "Polaca" = "European", "Portuguesa" = "European", "Rumania" = "European", "Russa" = "European", "Suiça" = "European", "Siria" = "MiddleEastern", "Uruguaya" = "LatinAmerican")

# Change observations from Portuguese to English
ML_linear <- ML_linear %>%
  mutate(
    ModifiedNationality = recode_factor(ModifiedNationality, "European" = "European", "LatinAmerican" = "Latin American", "MiddleEastern" = "Middle Eastern", "Brazilian" = "Brazilian"),
    Color = recode_factor(Color, "Preta" = "Black", "Parda" = "Mixed Race", "Branca" = "White"),
    Nationality = recode_factor(Nationality, "Alema" = "German", "Argentina" = "Argentine", "Austriaca" = "Austrian", "Brasileira" = "Brazilian", "Espanhola" = "Spanish", "Francesa" = "French", "Italiana" = "Italian", "Paraguaya" = "Paraguayan", "Polaca" = "Polish", "Portuguesa" = "Portuguese", "Rumania" = "Romanian", "Russa" = "Russian", "Suiça" = "Swiss", "Siria" = "Syrian", "Uruguaya" = "Uruguayan"),
    Birth = recode_factor(Birth, "aborto" = "Abortion", "intervencionista" = "Interventionist", "natural" = "Natural", "operatório" = "Operative"),
    MaternalOutcome = recode_factor(MaternalOutcome, "alta" = "Discharged", "morta" = "Death", "transferência" = "Hospital transferal"),
    FetalOutcome = recode_factor(FetalOutcome, "vivo" = "Live Birth", "morto" = "Stillbirth or Neonatal Death")
  )

str(ML_linear)

# Save new summary data with recoded factor variables
saveRDS(ML_linear, file = here("data", "processed-data", "ML_linear.rds"))