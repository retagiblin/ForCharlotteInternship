### ForCharlotte
### Gatherind data for State of Our City
### Disabilities Reality

library(tidycensus)
library(tidyverse)
library(ggplot2)

acs2022 <- load_variables(2022, "acs5")
#view(acs2022)

acs2022_subject <- load_variables(2022, "acs5/subject")
view(acs2022_subject)

NCcounties = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
               "Rowan")
SCcounties = c("Chester ", "Lancaster", "York")

counties = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
             "Rowan", "Chester ", "Lancaster", "York")
#########################################################################################
## Bellweather 

## get percent disabled in NC
NC_percent_disabled <- get_acs(
  geography = "county",
  variable = "S1810_C03_001",
  year = 2022,
  state = "NC",
  county = NCcounties
)
#view(NC_percent_disabled)

## get percent disabled in SC
SC_percent_disabled <- get_acs(
  geography = "county",
  variable = "S1810_C03_001",
  year = 2022,
  state = "SC",
  county = SCcounties
)
#view(SC_percent_disabled)

percent_disabled <- rbind(NC_percent_disabled, SC_percent_disabled)
#view(percent_disabled)

## get number disabled in NC
NC_number_disabled <- get_acs(
  geography = "county",
  variable = "S1810_C02_001",
  year = 2022,
  state = "NC",
  county = NCcounties
)
#view(NC_number_disabled)

## get number disabled in SC
SC_number_disabled <- get_acs(
  geography = "county",
  variable = "S1810_C02_001",
  year = 2022,
  state = "SC",
  county = SCcounties
)
#view(SC_number_disabled)

number_disabled <- rbind(NC_number_disabled, SC_number_disabled)
view(number_disabled)

# Plotting
ggplot(number_disabled, aes(y = NAME, x = estimate)) +
  geom_col(fill = "lightblue") +
  geom_text(aes(label = estimate, ), hjust = 0.5) +  
  labs(title = "Disabilities by County",
       subtitle = "2022 5-year ACS estimates",
       y = "County",
       x = "Count") 

## To get data for NC tracts
NC_number_disabled_tract <- get_acs(
  geograph = "tract",
  variables = "S1810_C02_001",
  state = "NC",
  county = c("Mecklenburg", "Union", "Anson", "Cabarrus", "Gaston", "Iredell", "Lincoln",
             "Rowan"),
  year = 2022
)
view(NC_number_disabled_tract)

## To get data for SC tracts
SC_number_disabled_tract <- get_acs(
  geograph = "tract",
  variables = "S1810_C02_001",
  state = "SC",
  county = c("Chester ", "Lancaster", "York"),
  year = 2022
)
view(SC_number_disabled_tract)

number_disabled_tract <- rbind(NC_number_disabled, SC_number_disabled)

#########################################################################################
#### By the Number 1:  Disabily by Type
## To get data for NC counties
NC_disability_type <- get_acs(
  geography = "county",
  variable = c("S1810_C02_019","S1810_C02_029","S1810_C02_039","S1810_C02_047","S1810_C02_055","S1810_C02_063"),
  state = "NC",
  county = NCcounties,
  year = 2022,
  output = "wide"
)
view(NC_disability_type)

## To get data for SC counties
SC_disability_type <- get_acs(
  geography = "county",
  variable = c("S1810_C02_019","S1810_C02_029","S1810_C02_039","S1810_C02_047","S1810_C02_055","S1810_C02_063"),
  state = "SC",
  county = SCcounties,
  year = 2022,
  output = "wide"
)
view(SC_disability_type)

disability_type <- rbind(NC_disability_type, SC_disability_type)
view(disability_type)

# Plotting
disability_type %>%
  mutate(NAME = str_remove(NAME, " .*$")) %>%
    ggplot(aes(y = reorder(NAME, S1810_C02_019E), x = S1810_C02_019E)) +
     geom_col() +
     theme_minimal() +
      labs(title = "Hearing Difficulty Disability by County",
      subtitle = "2022 5-year ACS estimates",
      y = "County",
      x = "ACS Estimate")

## To get all disabilities for one county
disability_type_county <- get_acs(
  geography = "county",
  variable = c("S1810_C02_019","S1810_C02_029","S1810_C02_039","S1810_C02_047","S1810_C02_055","S1810_C02_063"),
  state = "NC",
  county = "Mecklenburg",
  year = 2022
)
view(disability_type_county)

# Plotting
desired_labels <- c("Hearing", "Vision", "Cognitive", "Ambulatory", "Self-care","Independence")
ggplot(disability_type_county, aes(y = variable, x = estimate)) +
  geom_col(fill = "lightpink") +
  geom_text(aes(label = estimate, vjust = -0.25), hjust = 1.1) + 
  theme_minimal() +
  labs(title = "Types of Disabilities in Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Disability",
       x = "Count") +
scale_y_discrete(labels = desired_labels)

#########################################################################################
#### By the Number 2:  Disability by Age

## To get data for NC counties
NC_disability_age <- get_acs(
  geography = "county",
  variable = c("S1810_C02_013","S1810_C02_014","S1810_C02_015","S1810_C02_016","S1810_C02_017","S1810_C02_018"),
  state = "NC",
  county = NCcounties,
  year = 2022,
  output = "wide"
)

## To get data for SC counties
SC_disability_age <- get_acs(
  geography = "county",
  variable = c("S1810_C02_013","S1810_C02_014","S1810_C02_015","S1810_C02_016","S1810_C02_017","S1810_C02_018"),
  state = "SC",
  county = SCcounties,
  year = 2022,
  output = "wide"
)
view(NC_disability_age)

disability_age <- rbind(NC_disability_age, SC_disability_age)
view(disability_age)

## To get all disabilities for one county
disability_age_county <- get_acs(
  geography = "county",
  variable = c("S1810_C02_013","S1810_C02_014","S1810_C02_015","S1810_C02_016","S1810_C02_017","S1810_C02_018"),
  state = "NC",
  county = "Mecklenburg",
  year = 2022
)
view(disability_age_county)

# Plotting
desired_labels <- c(" < 5", "5 to 17", "18 to 34","35 to 64","65 to 74"," > 75")

ggplot(disability_age_county, aes(y = variable, x = estimate)) +
  geom_col(fill = "blue") +
  theme_minimal() +
  labs(title = "Age of those with a Disability in Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Count",
       x = "Disability") + 
  scale_y_discrete(labels = desired_labels)

#########################################################################################
#### By the Number 3:  Commuting to Work with a Disability

## To get data for NC counties
NC_commuting_type <- get_acs(
  geography = "county",
  variable = c("S1811_C01_032","S1811_C01_033","S1811_C01_034","S1811_C01_035",
               "S1811_C01_036","S1811_C01_037", "S1811_C01_038" ),
  state = "NC",
  county = NCcounties,
  year = 2022,
  output = "wide"
)

## To get data for SC counties
SC_commuting_type <- get_acs(
  geography = "county",
  variable = c("S1811_C01_032","S1811_C01_033","S1811_C01_034","S1811_C01_035",
               "S1811_C01_036","S1811_C01_037", "S1811_C01_038" ),
  state = "SC",
  county = SCcounties,
  year = 2022,
  output = "wide"
)

commuting_type <- rbind(NC_commuting_type, SC_commuting_type)
view(commuting_type)

# Plotting
commuting_type %>%
  mutate(NAME = str_remove(NAME, " .*$")) %>%
  ggplot(aes(y = reorder(NAME, S1811_C01_033E), x = S1811_C01_033E)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Disabled who drive to work alone by County",
       subtitle = "2022 5-year ACS estimates",
       y = "County",
       x = "ACS Estimate")

## To get commuting methods for those with a disability for one county
disability_commuting_county <- get_acs(
  geography = "county",
  variable = c("S1811_C01_033","S1811_C01_034","S1811_C01_035",
               "S1811_C01_036","S1811_C01_037", "S1811_C01_038" ),
  state = "NC",
  county = "Mecklenburg",
  year = 2022
)
view(disability_commuting_county)

# Plotting
desired_labels <- c("Car, truck, van - drove alone", "Car, truck, van - carpooled", "Public Transporation", "Walked", "Taxicab, motorcycle, bicycle, other", "Worked from Home")
ggplot(disability_commuting_county, aes(y = variable, x = estimate)) +
  geom_col(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Commuting Methods with a Disability for Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Count",
       x = "Disability") + 
  scale_y_discrete(labels = desired_labels)

#########################################################################################
#### By the Number 4:  Poverty Level for those with a disability (18-65)

# Get poverty levels for NC
NC_disability_poverty <- get_acs(
  geography = "county",
  variable = c("C21007_005", "C21007_008", "C21007_012", "C21007_015"),
  state = "NC",
  county = NCcounties,
  year = 2022,
  output = "wide"
)

# Get poverty levels for SC
SC_disability_poverty <- get_acs(
  geography = "county",
  variable = c("C21007_005", "C21007_008", "C21007_012", "C21007_015"),
  state = "SC",
  county = SCcounties,
  year = 2022,
  output = "wide"
)

disability_poverty <- rbind(NC_disability_poverty, SC_disability_poverty)
view(disability_poverty)

## To get poverty levels for those with a disability for one county
disability_poverty_county <- get_acs(
  geography = "county",
  variable = c("C21007_005", "C21007_008", "C21007_012", "C21007_015"),
  state = "NC",
  county = "Mecklenburg",
  year = 2022
)
view(disability_poverty_county)

# Plotting
desired_labels <- c("Vet - below poverty line", "Vet - at or above poverty line", 
                    "Non-vet - below poverty line", "Non-vet - at or above poverty line")
ggplot(disability_poverty_county, aes(y = variable, x = estimate)) +
  geom_col(fill = "orange") +
  theme_minimal() +
  labs(title = "Poverty Levels for 18-64 year olds with a Disability in Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Count",
       x = "Disability") + 
  scale_y_discrete(labels = desired_labels)

#########################################################################################
#### By the Number 5:  Poverty Level for those with a disability >=65

# get NC state numbers
NC_disability_poverty_over65 <- get_acs(
  geography = "county",
  variable = c("C21007_020", "C21007_023", "C21007_027", "C21007_030"),
  state = "NC",
  county = NCcounties,
  year = 2022,
  output = "wide"
)

# get SC state numbers
SC_disability_poverty_over65 <- get_acs(
  geography = "county",
  variable = c("C21007_020", "C21007_023", "C21007_027", "C21007_030"),
  state = "SC",
  county = SCcounties,
  year = 2022,
  output = "wide"
)

disability_poverty_over65 <- rbind(NC_disability_poverty_over65, SC_disability_poverty_over65)
view(disability_poverty_over65)

## To get poverty levels for those with a disability for one county
disability_poverty_county_over65 <- get_acs(
  geography = "county",
  variable = c("C21007_020", "C21007_023", "C21007_027", "C21007_030"),
  state = "NC",
  county = "Mecklenburg",
  year = 2022
)
view(disability_poverty_county_over65)

# Plotting
desired_labels <- c("Vet - below poverty line", "Vet - at or above poverty line", 
                    "Non-vet - below poverty line", "Non-vet - at or above poverty line")
ggplot(disability_poverty_county_over65, aes(y = variable, x = estimate)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Poverty Levels with a  Disability over 65 for Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Count",
       x = "Disability")+ 
  scale_y_discrete(labels = desired_labels)

#########################################################################################
#########################################################################################
#########################################################################################
#### Not that interesting/decided not to do:  Insured with Disability
## To get data for counties

disability_insured <- get_acs(
  geography = "county",
  variable = c("S2701_C01_035","S2701_C02_035","S2701_C03_035",
               "S2701_C04_035", "S2701_C05_035"),
  state = "NC",
  county = counties,
  year = 2022,
  output = "wide"
)
view(disability_insured)

## To get all disabilities for one county
disability_insured_county <- get_acs(
  geography = "county",
  variable = c("S2701_C01_035","S2701_C02_035","S2701_C03_035",
               "S2701_C04_035", "S2701_C05_035"),
  state = "NC",
  county = "Gaston",
  year = 2022
)
view(disability_insured_county)

graph_disability_insured_county <- disability_insured_county %>%
  filter(variable %in% c("S2701_C01_035","S2701_C02_035", "S2701_C04_035"))

#view(graph_disability_insured_county)

# Plotting
desired_labels <- c("Total with Disability", "# Insured", "# Uninsured" )

ggplot(graph_disability_insured_county, aes(y = variable, x = estimate)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Insured by Disability in Mecklenburg County",
       subtitle = "2022 5-year ACS estimates",
       y = "Count",
       x = "Insured Status") +
  scale_y_discrete(labels = desired_labels)

# Pie chart of Percentages insured vs. uninsured

graph_percent_disability_insured_county <- disability_insured_county %>%
  filter(variable %in% c("S2701_C03_035","S2701_C05_035"))

# Create the pie chart
# Get variable labels and values
variable_labels <- graph_percent_disability_insured_county$variable
values <- graph_percent_disability_insured_county$value

# Create the pie chart
pie_chart <- ggplot(data = data.frame(variable_labels, values), aes(x = "", y = values, fill = variable_labels)) +
  geom_col(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Percent Disability Insured by Variable", fill = "Variable") +
  theme_void()

# Customize the chart (optional)
pie_chart <- pie_chart +
  scale_fill_brewer(palette = "Dark2") +   # Change fill color palette
  theme(plot.title = element_text(hjust = 0.5)) # Center title

# Print or save the chart
print(pie_chart)
