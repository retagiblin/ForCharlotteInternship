library(tidycensus)
library(tidyverse)
library(ggplot2)

#census_api_key("fca1630b349c5bf5d2ea27c8de048e56c1150597", install=TRUE)

##############################################################################################################
##  Bellweather - Divorced by Generation 
##############################################################################################################

included_pumas <- c("02900", "03106", "03108", "03001", "03002", "03104", "03101", "3102", "3107", "05401",
                    "03103", "03105", "05301", "03200", "03300", "03001", "03002", "01900", "02900", "02700",
                    "03400", "05301", "01600", "00700", "00501", "00601", "00602") 

# Get PUMA records with married status of divorced between ages 15-99 in
divorced <- get_pums(
  variables = c("AGEP", "MAR", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MAR = c("3"),
    AGEP = 15:99
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
divorced <- divorced %>%
  filter(PUMA20 %in% included_pumas)

# Group divorced using ages into generations
divorced_generation <- divorced %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_divorced = sum(PWGTP))

view(divorced_generation)

# Calculated the number of currently divorced across all generations
grand_total_divorced <- sum(divorced_generation$total_divorced)
view(grand_total_divorced)

# Get the total number of people in each generations
totals_generations <- get_pums(
  variables = c("AGEP", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    AGEP = 15:99
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
totals_generations <- totals_generations %>%
  filter(PUMA20 %in% included_pumas)

# Group ages into generations
sum_totals_generations <- totals_generations %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_people = sum(PWGTP))

# Calculate grand total in all generations
grand_total_people <- sum(sum_totals_generations$total_people)

# Calculate percent of generation divorced
divorced_generation$percent_generation_divorced <- divorced_generation$total_divorced / sum_totals_generations$total_people

# Percent of all people currently divorced
percent_divorced <- grand_total_divorced / grand_total_people

cat("Percent of Adults Currently Divorced: ", percent_divorced, "\n")

ggplot(divorced_generation, aes(y = reorder(Generation, percent_generation_divorced), x = percent_generation_divorced * 100)) +  
  geom_col(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "% of Each Generation that is Currently Divorced in Charlotte",
       subtitle = "2022 5-year ACS estimates",
       y = "Generation",
       x = "Percent Divorced (%)") +  # Keep "%" in x-axis label
  scale_x_continuous(labels = scales::percent) +  
  # Add annotations with formatted percent values, adjust x position
  geom_text(aes(x = (percent_generation_divorced * 100) + 1.0,  # Add space to move numbers off bars
                y = reorder(Generation, percent_generation_divorced),
                label = scales::percent(percent_generation_divorced)),  
            vjust = -0.5, size = 3)

################################################################################
## Calculate the "Real/Refined" Divorce Rate
### Note:  These need to be looked at again and verified that the refined divorce rate is done correctly!

NCcounties = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
               "Rowan")
SCcounties = c("Chester ", "Lancaster", "York")

# convert divorce rates per 1000 to percent
divorce_rate_2018 <- 3.1/100
divorce_rate_2019 <- 3.1/100
divorce_rate_2020 <- 2.8/100
divorce_rate_2021 <- 3.2/100
divorce_rate_2022 <- 2.7/100

### 2002

NC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E", "B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2022,
  state = "NC",
  county = NCcounties,
  output = "wide"
)

SC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E","B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2022,
  state = "SC",
  county = SCcounties,
  output = "wide"
)

divorced_stat = rbind(NC_divorced_stat,SC_divorced_stat)
view(divorced_stat)

divorced_stat$number_of_divorces_2022 <- divorce_rate_2022 * divorced_stat$B06008_001E

divorced_stat$adjusted_divorce_rate_2022 <- divorced_stat$number_of_divorces_2022 /  
  ((divorced_stat$B06008_003E + divorced_stat$B06008_005E)/2)

# Calculate average divorce rate
average_divorce_rate_2022 <- mean(divorced_stat$adjusted_divorce_rate_2022)

# Print the average divorce rate
print(average_divorce_rate_2022)

### 2021

NC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E", "B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2021,
  state = "NC",
  county = NCcounties,
  output = "wide"
)

SC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E","B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2021,
  state = "SC",
  county = SCcounties,
  output = "wide"
)

divorced_stat_2021 = rbind(NC_divorced_stat,SC_divorced_stat)
view(divorced_stat_2021)

divorced_stat_2021$number_of_divorces_2021 <- divorce_rate_2021 * divorced_stat_2021$B06008_001E

divorced_stat_2021$adjusted_divorce_rate_2021 <- divorced_stat_2021$number_of_divorces_2021 /  
  ((divorced_stat_2021$B06008_003E + divorced_stat_2021$B06008_005E)/2)

# Calculate average divorce rate
average_divorce_rate_2021 <- mean(divorced_stat_2021$adjusted_divorce_rate_2021)

# Print the average divorce rate
print(average_divorce_rate_2021)

### 2020

NC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E", "B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2020,
  state = "NC",
  county = NCcounties,
  output = "wide"
)

SC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E","B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2020,
  state = "SC",
  county = SCcounties,
  output = "wide"
)

divorced_stat_2020 = rbind(NC_divorced_stat,SC_divorced_stat)
view(divorced_stat_2020)

divorced_stat_2020$number_of_divorces_2020 <- divorce_rate_2020 * divorced_stat_2020$B06008_001E

divorced_stat_2020$adjusted_divorce_rate_2020 <- divorced_stat_2020$number_of_divorces_2020 /  
  ((divorced_stat_2020$B06008_003E + divorced_stat_2020$B06008_005E)/2)

# Calculate average divorce rate
average_divorce_rate_2020 <- mean(divorced_stat_2020$adjusted_divorce_rate_2020)

# Print the average divorce rate
print(average_divorce_rate_2020)

### 2019

NC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E", "B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2019,
  state = "NC",
  county = NCcounties,
  output = "wide"
)

SC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E","B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2019,
  state = "SC",
  county = SCcounties,
  output = "wide"
)

divorced_stat_2019 = rbind(NC_divorced_stat,SC_divorced_stat)
view(divorced_stat_2019)

divorced_stat_2019$number_of_divorces_2019 <- divorce_rate_2019 * divorced_stat_2019$B06008_001E

divorced_stat_2019$adjusted_divorce_rate_2019 <- divorced_stat_2019$number_of_divorces_2019 /  
  ((divorced_stat_2019$B06008_003E + divorced_stat_2019$B06008_005E)/2)

# Calculate average divorce rate
average_divorce_rate_2019 <- mean(divorced_stat_2019$adjusted_divorce_rate_2019)

# Print the average divorce rate
print(average_divorce_rate_2019)

### 2018

NC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E", "B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2018,
  state = "NC",
  county = NCcounties,
  output = "wide"
)

SC_divorced_stat <- get_acs(
  geography = "county",
  variable = c("B06008_001E","B06008_003E", "B06008_004E", "B06008_005E"),
  year = 2018,
  state = "SC",
  county = SCcounties,
  output = "wide"
)

divorced_stat_2018 = rbind(NC_divorced_stat,SC_divorced_stat)
view(divorced_stat_2018)

divorced_stat_2018$number_of_divorces_2018 <- divorce_rate_2018 * divorced_stat_2018$B06008_001E

divorced_stat_2018$adjusted_divorce_rate_2018 <- divorced_stat_2018$number_of_divorces_2018 /  
  ((divorced_stat_2018$B06008_003E + divorced_stat_2018$B06008_005E)/2)

# Calculate average divorce rate
average_divorce_rate_2018 <- mean(divorced_stat_2018$adjusted_divorce_rate_2018)

# Print the average divorce rate
print(average_divorce_rate_2018)

##################################################################################################################
## BTN 1  -- Median Age of 1st Marriage - Overall Number Using PUMS data 
##################################################################################################################
## Median Age of 1st Marriages in the past year

pums_vars_2022 <- pums_variables %>% 
  filter(year == 2022, survey == "acs5")

included_pumas <- c("02900", "03106", "03108", "03001", "03002", "03104", "03101", "3102", "3107", "05401",
                    "03103", "03105", "05301", "03200", "03300", "03001", "03002", "01900", "02700",
                    "03400", "05301", "01600", "00700", "00501", "00601", "00602") 

age_first_marriage <- get_pums(
  variables = c("AGEP", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MARHM = "1",
    AGEP = 15:99,
    MARHT = "1"
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
filter_age_first_marriage <- age_first_marriage %>%
  filter(PUMA20 %in% included_pumas)

View(filter_age_first_marriage)

# Sort the data by age
sorted_data <- filter_age_first_marriage[order(filter_age_first_marriage$AGEP), ]

# Calculate the cumulative sum of weights
sorted_data$CumulativeWeight <- cumsum(sorted_data$PWGTP)

# Find the median index
total_weight <- sum(sorted_data$PWGTP)
median_index <- which.max(sorted_data$CumulativeWeight >= total_weight / 2)

# Calculate the weighted median age
weighted_median_age <- sorted_data$AGEP[median_index]

# Print the weighted median age
view(weighted_median_age)

# Median Age for Charlotte area = 29
#################################################################################
## Calculate Weighted MEAN Age of first marriage for each PUMA

# Group by AGEP and calculate the median of AGEP within each group
grouped_mean <- filter_age_first_marriage %>%
  group_by(PUMA20) %>%
  summarize(mean_AGE = round(weighted.mean(AGEP, PWGTP)))

# View the grouped median data
view(grouped_mean)

#############################################################################################################
# Bar Chart of Age of 1st Marriages this Year

grouped_age_data <- filter_age_first_marriage %>%
  group_by(AGEP) %>%
  summarize(total_PWGTP = sum(PWGTP))

# View the grouped data
view(grouped_age_data)

# Bar Chart
ggplot(grouped_age_data, aes(x = AGEP, y = total_PWGTP)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Age of Marriage", y = " # People Married", title = "1st Marriages by Age in 2022 in Charlotte Metro Area") +
  theme_minimal()

#################################################################################
## Calculate Age Count for 1st Marriage this year within each PUMA

# Group by PUMA20 and AGEP, and calculate the count of each AGEP value within each PUMA20 group
grouped_puma_age_data <- filter_age_first_marriage %>%
  group_by(PUMA20, AGEP) %>%
  summarize(total_PWGTP = sum(PWGTP))

view(grouped_puma_age_data)

# Create a bar chart for each PUMA20
bar_plot <- ggplot(grouped_puma_age_data, aes(x = AGEP, y = total_PWGTP)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Age of 1st Marriage", y = "Number Married", title = "Age of 1st Marriages in 2022 by PUMA") +
  facet_wrap(~ PUMA20, scales = "free_x") +  # Facet by PUMA20
  theme_minimal()

# View the bar plot
print(bar_plot)

################################################
# Calculate median age of marriage for each PUMA

grouped_puma_cumsum <- grouped_puma_age_data %>%
  group_by(PUMA20) %>%
  mutate(cum_PWGTP = cumsum(total_PWGTP))

view(grouped_puma_cumsum)

## INCOMPLETE:  WILL FINISH IF DECIDE TO USE ###

#################################################################################################################
## By the Numbers 2 - NEVER MARRIED VS. NEVER MARRIED BUT COHABITING BY GENERATION
#################################################################################################################

#included_pumas <- c("02900", "03106", "03108", "03001", "03002", "03104", "03101", "3102", "3107", "05401",
#                    "03103", "03105", "05301", "03200", "03300", "03001", "03002", "01900", "02900", "02700",
#                    "03400", "05301", "01600", "00700", "00501", "00601", "00602") 

##### Get total number of households

NCcounties = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
             "Rowan")
SCcounties = c("Chester ", "Lancaster", "York")

### get total NC households
NC_total_households <- get_acs(
  geography = "county",
  variable = "B09019_001E",
  year = 2022,
  state = "NC",
  county = NCcounties
)
view(NC_total_households)

total_number_NC_households <- sum(NC_total_households$estimate)
view(total_number_NC_households)

### get total SC households
SC_total_households <- get_acs(
  geography = "county",
  variable = "B09019_001E",
  year = 2022,
  state = "SC",
  county = SCcounties
)
view(SC_total_households)

total_number_SC_households <- sum(SC_total_households$estimate)
view(total_number_SC_households)

### Calculate total Charlotte Metro households in NC & SC
total_households = total_number_NC_households + total_number_SC_households
view(total_households)

#####
### Get never married and cohabitating numbers
never_marriage_and_cohab <- get_pums(
  variables = c("AGEP", "HHT2", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MAR = "5",
    AGEP = 15:99
  ),  
  year = 2022
)

# Remove FIPS not in Charlotte Region
never_marriage_and_cohab_clt <- never_marriage_and_cohab %>%
  filter(PUMA20 %in% included_pumas)

never_marriage_cohab_clt <- never_marriage_and_cohab_clt[never_marriage_and_cohab_clt$HHT2 %in% c("03", "04"), ]
#view(never_marriage_cohab_clt)
#view(sum(never_marriage_cohab_clt$PWGTP))

never_marriage_clt <- never_marriage_and_cohab_clt[never_marriage_and_cohab_clt$HHT2 %in% c("05", "06", "07", "08", "09", "10", "11", "12"), ]
#view(never_marriage_clt)
#view(sum(never_marriage_clt$PWGTP))

# For Cohabiting and Never Married ##################################
# Group ages into generations
generations_data_cohab <- never_marriage_cohab_clt %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_individuals_cohab = sum(PWGTP))

# View the grouped data
view(generations_data_cohab)

grand_total_cohabitating <- sum(generations_data_cohab$total_individuals_cohab)

generations_data_cohab$percent <- generations_data_cohab$total_individuals_cohab /
  grand_total_cohabitating

# Round to 2 decimal places using round.ties = "half up"
generations_data_cohab$rounded_value <- round(generations_data_cohab$percent, 3)

# Use scales::percent to format as a percentage string
generations_data_cohab$formatted_percent <- scales::percent(generations_data_cohab$rounded_value)


# For  Never Married ################################################
# Group ages into generations
generations_data_nevermarried <- never_marriage_clt %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_individuals_nevermarried = sum(PWGTP))

# View the grouped data
view(generations_data_nevermarried)

grand_total_nevermarried <- sum(generations_data_nevermarried$total_individuals_nevermarried)

generations_data_nevermarried$percent <- generations_data_nevermarried$total_individuals_nevermarried /
  grand_total_nevermarried

# Round to 2 decimal places using round.ties = "half up"
generations_data_nevermarried$rounded_value <- round(generations_data_nevermarried$percent, 3)

# Use scales::percent to format as a percentage string
generations_data_nevermarried$formatted_percent <- scales::percent(generations_data_nevermarried$rounded_value)

##############################################################################################################
## By the Numbers 3 - Now Married by Generation
##############################################################################################################

married <- get_pums(
  variables = c("AGEP", "MAR", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MAR = "1",
    AGEP = 15:99
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
married <- married %>%
  filter(PUMA20 %in% included_pumas)

#view(married)

# Group ages into generations
married_generation <- married %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_married = sum(PWGTP))

grand_total_married <- sum(married_generation$total_married)

# View the grouped data
view(married_generation)

#####
## Get total number of people in each generation so % can be calculated
totals_generations <- get_pums(
  variables = c("AGEP", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    AGEP = 15:99
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
totals_generations <- totals_generations %>%
  filter(PUMA20 %in% included_pumas)

# Group ages into generations
sum_totals_generations <- totals_generations %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_people = sum(PWGTP))

view(sum_totals_generations)

# Calculate grand total in all generations
grand_total_people <- sum(sum_totals_generations$total_people)

######
## Calculate percent married by taking married and dividing by 
##  total people in each generation

# Calculate percent of generation married
married_generation$percent_generation_married <- married_generation$total_married / sum_totals_generations$total_people
view(married_generation)

# Percent of all people currently married
percent_married <- grand_total_married / grand_total_people

cat("Percent of Adults Currently Married: ", percent_married, "\n")

ggplot(married_generation, aes(y = reorder(Generation, percent_generation_married), x = percent_generation_married * 100)) +  # Multiply by 100 for percentages
  geom_col(fill = "pink") +
  theme_minimal() +
  labs(title = "% of each Generation that is Married in Charlotte",
       subtitle = "2022 5-year ACS estimates",
       y = "Generation",
       x = "Percent Married (%)") +  # Keep "%" in x-axis label
  scale_x_continuous(labels = scales::percent) +  # Apply percent format
  # Add annotations with formatted percent values, adjust x position
  geom_text(aes(x = (percent_generation_married * 100) + 3.0,  # Increase x by 0.2
                y = reorder(Generation, percent_generation_married),
                label = scales::percent(percent_generation_married)),  # Use scales::percent
            vjust = -0.5, size = 3)

##############################################################################################################
## By the Numbers 4 - Now Married by Education Level

married_education <- get_pums(
  variables = c("AGEP", "MAR", "SCHL", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MAR = "1",
    AGEP = 15:99
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
married_education <- married_education %>%
  filter(PUMA20 %in% included_pumas)

view(married_education)

# Group into education levels
married_education_level <- married_education %>%
  mutate(Education = case_when(
    parse_number(SCHL) %in%  c(01:15) ~ "1- No Diploma",
    parse_number(SCHL) %in%  c(16:17) ~ "2- Diploma/GED",
    parse_number(SCHL) %in%  c(18:20) ~ "3- Associate Degree or some college",
#    parse_number(SCHL) %in%  21 ~ "4- Bachelor's Degree",
    parse_number(SCHL) %in%  c(21:24) ~ "4- Bachelors, Masters or Doctorate"
  )) %>%
  group_by(Education) %>%
  summarize(total_by_education_level = sum(PWGTP))

# View the grouped data
view(married_education_level)

ggplot(married_education_level, aes(y = reorder(Education, total_by_education_level), x = total_by_education_level)) +
  geom_col(fill = "blue") +
  theme_minimal() +
  labs(title = "# Married by Education Level in Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Education",
       x = "# Married")

##############################################################################################################
## **Probably will not use** - By the Numbers - Now Divorced & Separated by Generation
##############################################################################################################

divorced_separated <- get_pums(
  variables = c("AGEP", "MAR", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MAR = c("3", "4"),
    AGEP = 15:99
  ), 
  year = 2022
)

# Remove FIPS not in Charlotte Region
divorced_separated <- divorced_separated %>%
  filter(PUMA20 %in% included_pumas)

# Group ages into generations
divorced_separated_generation <- divorced_separated %>%
  mutate(Generation = case_when(
    between(AGEP, 0, 9) ~ "Gen Alpha",
    between(AGEP, 10, 27) ~ "Gen Z",
    between(AGEP, 28, 42) ~ "Millennials",
    between(AGEP, 43, 57) ~ "Gen X",
    between(AGEP, 58, 76) ~ "Baby Boomers",
    AGEP >= 77 ~ "Silent Generation"
  )) %>%
  group_by(Generation) %>%
  summarize(total_divorced_separated = sum(PWGTP))

view(divorced_separated_generation)

ggplot(divorced_separated_generation, aes(y = reorder(Generation, total_divorced_separated), x = total_divorced_separated)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Divorced & Separated in Charlotte Metro Area",
       subtitle = "2022 5-year ACS estimates",
       y = "Count",
       x = "Generation")

##############################################################################################################
## By the Numbers AMPTHER OPTION - Now DIVORCED by Education Level

divorced_education <- get_pums(
  variables = c("AGEP", "MAR", "SCHL", "PUMA20"),
  state = c("NC","SC"),
  survey = "acs5",
  variables_filter = list(
    MAR = "3",
    AGEP = 15:99
  ), 
  year = 2022
)

#view(divorced_education)

# Remove FIPS not in Charlotte Region
divorced_education <- divorced_education %>%
  filter(PUMA20 %in% included_pumas)

view(divorced_education)

# Group into education levels
divorced_education_level <- divorced_education %>%
  mutate(Education = case_when(
    parse_number(SCHL) %in%  c(01:15) ~ "1- No Diploma",
    parse_number(SCHL) %in%  c(16:17) ~ "2- Diploma/GED",
    parse_number(SCHL) %in%  c(18:20) ~ "3- Associate Degree or some college",
    parse_number(SCHL) %in%  21 ~ "4- Bachelor's Degree",
    parse_number(SCHL) %in%  c(22:24) ~ "5- Master's Degree or Doctorate"
  )) %>%
  group_by(Education) %>%
  summarize(total_by_education_level = sum(PWGTP))

# View the grouped data
view(divorced_education_level)

ggplot(divorced_education_level, aes(y = reorder(Education, total_by_education_level), x = total_by_education_level)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Divorced by Education in Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Education",
       x = "# Divorced")
       