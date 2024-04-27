### Upward Mobility Reality 

### This code retrieves the data for upward mobility 
### (mostly from the U.S. Census Bureau) at both the tract and county level
### and writes 2 .csv files, one for tract level data and one for county level data

library(tidyverse)
library(tidycensus)
library(dplyr)
library(stats)  
library(factoextra) 
library(tigris)
library(ggplot2)
library(sf)

file_path <- "c:/Users/retag/Desktop/ForCLT/UpwardMobility"
options(tigris_use_cache = TRUE)

NCcounties = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
               "Rowan")
SCcounties = c("Chester ", "Lancaster", "York")

counties = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
             "Rowan", "Chester ", "Lancaster", "York")

### Check level of availability of variables

v1 <- load_variables(2022, "acs5")
view(v1)

v2 <- load_variables(2022, "acs5/subject")
view(v2)

################################################################################
### Household Income at tract level

## get household income in NC
NC_household_income <- get_acs(
  geography = "tract",
  variable = "B19013_001E",
  year = 2022,
  state = "NC",
  county = NCcounties
)
#view(NC_household_income)

## get household income in SC
SC_household_income <- get_acs(
  geography = "tract",
  variable = "B19013_001E",
  year = 2022,
  state = "SC",
  county = SCcounties
)
#view(SC_household_income)

household_income <- rbind(NC_household_income, SC_household_income)
view(household_income)

################################################################################
### Household Income at county level

## get household income in NC
NC_household_income_county <- get_acs(
  geography = "county",
  variable = "B19013_001E",
  year = 2022,
  state = "NC",
  county = NCcounties,
  output = "wide"
)
#view(NC_household_income_county)

## get household income in SC
SC_household_income_county <- get_acs(
  geography = "county",
  variable = "B19013_001E",
  year = 2022,
  state = "SC",
  county = SCcounties,
  output = "wide"
)
#view(SC_household_income_county)

household_income_county <- rbind(NC_household_income_county, SC_household_income_county)
view(household_income_county)

# drop margins of error
household_income_county <- household_income_county[, c(-4)]

#Rename Columns
colnames(household_income_county)[3] <- "income"

################################################################################
### Employment Rate at tract level

## get Employment Rate in NC
NC_employment_rate <- get_acs(
  geography = "tract",
  variable = "S2301_C03_001E",
  year = 2022,
  state = "NC",
  county = NCcounties
)
#view(NC_employment_rate)

## get Employment Rate in SC
SC_employment_rate <- get_acs(
  geography = "tract",
  variable = "S2301_C03_001E",
  year = 2022,
  state = "SC",
  county = SCcounties
)
#view(SC_employment_rate)

employment_rate <- rbind(NC_employment_rate, SC_employment_rate)
view(employment_rate)

################################################################################
### Employment Rate at county level

## get Employment Rate in NC
NC_employment_rate_county <- get_acs(
  geography = "county",
  variable = "S2301_C03_001E",
  year = 2022,
  state = "NC",
  county = NCcounties,
  output = "wide"
)
#view(NC_employment_rate_county)

## get Employment Rate in SC
SC_employment_rate_county <- get_acs(
  geography = "county",
  variable = "S2301_C03_001E",
  year = 2022,
  state = "SC",
  county = SCcounties,
  output = "wide"
)
#view(SC_employment_rate_county)

employment_rate_county <- rbind(NC_employment_rate_county, SC_employment_rate_county)
view(employment_rate_county)

# drop margins of error
employment_rate_county <- employment_rate_county[, c(-4)]

#Rename Columns
colnames(employment_rate_county)[3] <- "employment_rate"


################################################################################
### Marriage Rate

nc_marriage_rates <- read.csv("c:/Users/retag/Desktop/ForCLT/UpwardMobility/NCMarriageRates.csv")

################################################################################
### Teen Birthrate

clt_teen_birthrate <- read.csv("c:/Users/retag/Desktop/ForCLT/UpwardMobility/NCCLTTeenBirthRate.csv")

# convert birthrate to percent(out of 100, instead of out of 1000)
clt_teen_birthrate$Birth.Rate <- clt_teen_birthrate$Birth.Rate/10
view(clt_teen_birthrate)

# Create new file with just the fields needed
clt_teen_birthrate_county <- clt_teen_birthrate[, c("Year", "Combined.FIPS.Code", "Birth.Rate")]

# Drop Union County South Carolina
clt_teen_birthrate_county <- clt_teen_birthrate_county[clt_teen_birthrate_county$Combined.FIPS.Code != 45087, ]

# Only keep birth rate for latest year, 2020
clt_teen_birthrate_county_2020 <- clt_teen_birthrate_county[clt_teen_birthrate_county == 2020, ]

# Rename some columns
colnames(clt_teen_birthrate_county_2020)[2] <- "GEOID"
colnames(clt_teen_birthrate_county_2020)[3] <- "birth_rate"

# Drop Year column
clt_teen_birthrate_county_2020 <- clt_teen_birthrate_county_2020[ , -(1)] 

################################################################################
### Educational Attainment at tract level

## get Educational Attainment in NC
NC_educational_attainment <- get_acs(
  geography = "tract",
  variable = c("B06009_001E","B06009_002E", "B06009_003E", "B06009_004E", "B06009_005E", "B06009_006E"), 
  year = 2022,
  state = "NC",
  county = NCcounties,
  output = "wide"
)
view(NC_educational_attainment)

## get Educational Attainment in SC
SC_educational_attainment <- get_acs(
  geography = "tract",
  variable = c("B06009_001E","B06009_002E", "B06009_003E", "B06009_004E", "B06009_005E", "B06009_006E"), 
  year = 2022,
  state = "SC",
  county = SCcounties,
  output = "wide"
)
view(SC_educational_attainment)

educational_attainment <- rbind(NC_educational_attainment, SC_educational_attainment)
view(educational_attainment)

# drop margins of error
educational_attainment <- educational_attainment[, c(-4, -6, -8, -10, -12, -14)]

#Rename Columns
colnames(educational_attainment)[3] <- "Total"
colnames(educational_attainment)[4] <- "Less_HS"
colnames(educational_attainment)[5] <- "HS_Equiv"
colnames(educational_attainment)[6] <- "Some_College"
colnames(educational_attainment)[7] <- "Bachelors"
colnames(educational_attainment)[8] <- "Graduate_Prof"

################################################################################
### Educational Attainment at County level

## get Educational Attainment in NC
NC_educational_attainment_county <- get_acs(
  geography = "county",
  variable = c("B06009_001E","B06009_002E", "B06009_003E", "B06009_004E", "B06009_005E", "B06009_006E"), 
  year = 2022,
  state = "NC",
  county = NCcounties,
  output = "wide"
)
#view(NC_educational_attainment)

## get Educational Attainment in SC
SC_educational_attainment_county <- get_acs(
  geography = "county",
  variable = c("B06009_001E","B06009_002E", "B06009_003E", "B06009_004E", "B06009_005E", "B06009_006E"), 
  year = 2022,
  state = "SC",
  county = SCcounties,
  output = "wide"
)
#view(SC_educational_attainment)

educational_attainment_county <- rbind(NC_educational_attainment_county, SC_educational_attainment_county)

# drop margins of error
educational_attainment_county <- educational_attainment_county[, c(-4, -6, -8, -10, -12, -14)]

#Rename Columns
colnames(educational_attainment_county)[3] <- "Total"
colnames(educational_attainment_county)[4] <- "Less_HS"
colnames(educational_attainment_county)[5] <- "HS_Equiv"
colnames(educational_attainment_county)[6] <- "Some_College"
colnames(educational_attainment_county)[7] <- "Bachelors"
colnames(educational_attainment_county)[8] <- "Graduate_Prof"

view(educational_attainment_county)

educational_attainment_county$Less_HS <- educational_attainment_county$Less_HS / educational_attainment_county$Total
educational_attainment_county$HS_Equiv <- educational_attainment_county$HS_Equiv / educational_attainment_county$Total
educational_attainment_county$Some_College <- educational_attainment_county$Some_College / educational_attainment_county$Total
educational_attainment_county$Bachelors <- educational_attainment_county$Bachelors / educational_attainment_county$Total
educational_attainment_county$Graduate_Prof <- educational_attainment_county$Graduate_Prof / educational_attainment_county$Total

################################################################################
### Combine files into one file 

# Merge the dataframes
combined_df <- merge(household_income_county, employment_rate_county, by = "GEOID", all = TRUE)
combined_df <- merge(combined_df, clt_teen_birthrate_county_2020, by = "GEOID", all = TRUE)
combined_df <- merge(combined_df, educational_attainment_county, by = "GEOID", all = TRUE)

# Drop duplicate rows
combined_df <- subset(combined_df, select = -c(NAME.x, NAME.y))
combined_df <- subset(combined_df, select = -c(Total))

# Write combined file to a .csv file to be used by model
write.csv(combined_df, file = "c:/Users/retag/Desktop/ForCLT/UpwardMobility/CombinedNewUpdwardMobilityData.csv", row.names = TRUE)

################################################################################
### Combine files into one file data modeling at TRACT level

colnames(employment_rate)[4] <- "employment_rate"
colnames(household_income)[4] <- "household_income"


# Look up teen birth rate by FIPS code
colnames(clt_teen_birthrate_county_2020)[1] <- "FIPS"
employment_rate$FIPS <- substr(employment_rate$GEOID, 1, 5)
employment_rate <- merge(employment_rate, 
                         clt_teen_birthrate_county_2020[, c("FIPS", "birth_rate")], 
                         by.x = "FIPS", by.y = "FIPS", all.x = TRUE)

# Continue to combine all data frames together to combine in one file
combined_df_tract <- merge(household_income, employment_rate, by = "GEOID", all = TRUE)
combined_df_tract <- merge(combined_df_tract, educational_attainment, by = "GEOID", all = TRUE)

# Drop duplicate rows
combined_df_tract <- subset(combined_df_tract, select = -c(NAME.x, variable.x, moe.x, NAME.y, variable.y, moe.y, FIPS))

combined_df_tract$Less_HS <- combined_df_tract$Less_HS / combined_df_tract$Total
combined_df_tract$HS_Equiv <- combined_df_tract$HS_Equiv / combined_df_tract$Total
combined_df_tract$Some_College <- combined_df_tract$Some_College / combined_df_tract$Total
combined_df_tract$Bachelors <- combined_df_tract$Bachelors / combined_df_tract$Total
combined_df_tract$Graduate_Prof <- combined_df_tract$Graduate_Prof / combined_df_tract$Total

combined_df_tract <- subset(combined_df_tract, select = -c(Total))

# Write combined file to a .csv file to be used by model
write.csv(combined_df_tract, file = "c:/Users/retag/Desktop/ForCLT/UpwardMobility/CombinedNewUpdwardMobilityTractData.csv", row.names = TRUE)
