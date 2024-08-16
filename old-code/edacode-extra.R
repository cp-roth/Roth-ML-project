## ---- Extracode --------
# Don't use this code in future
# Create summary stats for birth weight by maternal modifiedcolor
birthweight_color <- ML_summary %>%
  filter(!is.na(ModifiedColor) & !is.na(Weightgrams)) %>%
  group_by(ModifiedColor) %>%
  summarize(
    mean_weight = mean(Weightgrams, na.rm = TRUE),
    sd_weight = sd(Weightgrams, na.rm = TRUE)
  )
print(birthweight_color)

# Create summary stats for birth weight by fetal sex
birthweight_sex <- ML_summary %>%
  filter(!is.na(Sex) & !is.na(Weightgrams)) %>%
  group_by(Sex) %>%
  summarize(
    mean_weight = mean(Weightgrams, na.rm = TRUE),
    sd_weight = sd(Weightgrams, na.rm = TRUE)
  ) 
print(birthweight_sex)

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

#Create gtsummary table on infant birth weight stratified by sex and maternal skin color
stratified_color <- 
  ML_summary %>%
  filter(!is.na(Weightgrams) & !is.na(ModifiedColor)) %>% # Exclude rows with NA in weight & MC variable
  rename("Infant Weight (grms)" = Weightgrams) %>% # Rename Weightgrams to Infant Weight (grms)
  tbl_summary(
    by = ModifiedColor,  # Grouping by ModifiedColor
    include = "Infant Weight (grms)",
    statistic = list("Infant Weight (grms)" ~ "{mean} ({sd})")
  )
stratified_color

stratified_sex <- 
  ML_summary %>%
  filter(!is.na(Weightgrams) & !is.na(Sex)) %>%
  rename("Infant Weight (grms)" = Weightgrams) %>% # Rename Weightgrams to Infant Weight (grms)
  tbl_summary(
    by = Sex,  # Grouping by Sex
    include = "Infant Weight (grms)",
    statistic = list("Infant Weight (grms)" ~ "{mean} ({sd})")
  )
stratified_sex

# Combine the two tables
table2 <- tbl_merge(
  tbls = list(stratified_color, stratified_sex),
  tab_spanner = c("**Weight by Modified Color**", "**Weight by Sex**"))

# Display the modified table
table2

#Save table
saveRDS(table2, file = here::here("results", "tables", "table2_final.rds"))

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

# Summary stats of categorical variables
# Calculate frequencies of color category (branca, parda, preta)
color_freq <- ML_summary %>%
  filter(!is.na(Color)) %>% # Exclude rows with NA in color variable
  count(Color, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(color_freq)

# Calculate frequencies of color category (branca, parda, preta)
color_freq2 <- ML_summary %>%
  filter(!is.na(ModifiedColor)) %>% # Exclude rows with NA in color variable
  count(ModifiedColor, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(color_freq2)

# Calculate frequencies of Status category
status_freq <- ML_summary %>%
  filter(!is.na(Status)) %>% # Exclude rows with NA in status variable
  count(Status, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(status_freq)

# Calculate frequencies of ModifiedStatus category
status_freq2 <- ML_summary %>%
  filter(!is.na(ModifiedStatus)) %>% # Exclude rows with NA in status variable
  count(ModifiedStatus, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(status_freq2)

# Calculate frequencies of Nationality category
nationality_freq <- ML_summary %>%
  filter(!is.na(Nationality)) %>% # Exclude rows with NA in nationality variable
  count(Nationality, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(nationality_freq)

# Calculate frequencies of ModifiedNationality category
nationality_freq2 <- ML_summary %>%
  filter(!is.na(ModifiedNationality)) %>% # Exclude rows with NA in nationality variable
  count(ModifiedNationality, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(nationality_freq2)

# Calculate frequencies of MaternalOutcome category
maternal_freq <- ML_summary %>%
  filter(!is.na(MaternalOutcome)) %>% # Exclude rows with NA in MaternalOutcome variable
  count(MaternalOutcome, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(maternal_freq)

# Calculate frequencies of Sex category
sex_freq <- ML_summary %>%
  filter(!is.na(Sex)) %>% # Exclude rows with NA in Sex variable
  count(Sex, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(sex_freq)

# Calculate frequencies of birth category with abortion
birth_freq <- ML_summary %>%
  filter(!is.na(Birth)) %>% # Exclude rows with NA in birth variable
  count(Birth, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(birth_freq)

# Calculate frequencies of FetalOutcome category excluding abortion
fetal_freq <- ML_summary %>%
  filter(!is.na(FetalOutcome)) %>% # Exclude rows with NA in FetalOutcome variable
  count(FetalOutcome, na.rm = TRUE) %>% # Calculate frequencies while excluding NAs
  filter(FetalOutcome != "abortion") %>% # Exclude rows with abortion
  mutate(Total_Sample_Size = sum(n)) %>% # Calculate total sample size 
  mutate(Percent = n/sum(n)*100) # Calculate percentage of each category
print(fetal_freq)