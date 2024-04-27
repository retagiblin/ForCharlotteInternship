###############################################################################
### Upward Mobility Model Development
### Modeling Upward Mobility data from Opportunity Insights

library(tidyverse)
library(dplyr)
library(stats)  
library(factoextra) 
library(tigris)
library(ggplot2)
library(sf)

file_path <- "c:/Users/retag/Desktop/ForCLT/UpwardMobility"
setwd("c:/Users/retag/Desktop/ForCLT/UpwardMobility")
options(tigris_use_cache = TRUE)

## Read in Files
EmploymentRate <- read_csv(file.path(file_path, "EmploymentRate.csv"))
IncarcerationRate <- read_csv(file.path(file_path, "IncarcerationRate.csv"))
MarriedAt35 <- read_csv(file.path(file_path, "MarriedAt35.csv"))
TeenBirthrate <- read_csv(file.path(file_path, "TeenBirthRate.csv"))
HouseholdIncome <- read_csv(file.path(file_path, "HouseholdIncome.csv"))

# Filter tracts for 'Charlotte, NC'
EmploymentRate <- EmploymentRate %>%
  filter(str_detect(Name, "Charlotte, NC"))
IncarcerationRate <- IncarcerationRate %>%
  filter(str_detect(Name, "Charlotte, NC"))
MarriedAt35 <- MarriedAt35 %>%
  filter(str_detect(Name, "Charlotte, NC"))
TeenBirthrate <- TeenBirthrate %>%
  filter(str_detect(Name, "Charlotte, NC"))
HouseholdIncome <- HouseholdIncome %>%
  filter(str_detect(Name, "Charlotte, NC"))


# View the filtered data
#print(EmploymentRate)

# Merge the dataframes
merged_df <- merge(EmploymentRate, IncarcerationRate, by = "tract", all = TRUE)
merged_df <- merge(merged_df, MarriedAt35, by = "tract", all = TRUE)
merged_df <- merge(merged_df, TeenBirthrate, by = "tract", all = TRUE)
merged_df <- merge(merged_df, HouseholdIncome, by = "tract", all = TRUE)

# Drop duplicate rows
merged_df <- subset(merged_df, select = -c(Name.x, Name.y))

# View the merged dataframe
view(merged_df)

merged_df <- subset(merged_df, select = -c(Name.x, Name.y))

# determine data type of each field
str(merged_df)

# Convert tract to a string to omit from clustering
merged_df$tract <- as.character(merged_df$tract)

# check for missing values
sum(is.na(merged_df)) 
print(is.na(merged_df))

# Remove rows with NAs
merged_df <- na.omit(merged_df)

# Standardize the data
numeric_cols <- sapply(merged_df, is.numeric)
merged_df_scaled <- scale(merged_df[, numeric_cols])

# Determine best number of clusters############################################

# Elbow Method -------------------------------------
# Define a numeric vector to store WSS
# use the elbow method: look for the "bend" on the plot
fviz_nbclust(merged_df_scaled, kmeans, method = "wss") 

# use the silhouette method
fviz_nbclust(merged_df_scaled, kmeans, method = "silhouette") 

# Do K-means clustering
cluster_upward_mobility = kmeans(merged_df_scaled, centers = 2)
cluster_upward_mobility$size

# plot the clusters
fviz_cluster(cluster_upward_mobility, data = merged_df_scaled) + theme_classic()

# Cluster centroids
cluster_centers <- cluster_upward_mobility$centers
print(cluster_centers)

# Cluster assignments
cluster_assignments <- cluster_upward_mobility$cluster
print(cluster_assignments)

# Within-cluster sum of squared (WCSS)
within_clustier_sum_of_squares <- cluster_upward_mobility$withinss
print(within_clustier_sum_of_squares)

# Combine original dataframe with cluster assignments to get data frame with data and cluster assignment
upward_mobility_with_clusters <- cbind(merged_df, cluster_assignments)
print(upward_mobility_with_clusters)

############################################################################################
############################################################################################

#tracts <- tracts(state = "NC", county = "Mecklenburg")
NC_county_codes = c("007", "025", "071", "097" ,"109", "119", "159", "179")
SC_county_codes = c("023", "057", "091")
NCtracts <- tracts(state = "NC", county=NC_county_codes)
SCtracts <- tracts(state = "SC", county=SC_county_codes)
tracts <- rbind(NCtracts, SCtracts)

merged_data <- merge(x = tracts, y = upward_mobility_with_clusters, all.y = TRUE, by.x = "GEOID", by.y = "tract")

# Plot the map of both clusters using ggplot2
ggplot() +
  geom_sf(data = merged_data, aes(fill = cluster_assignments)) +
  scale_fill_gradient(name = "Cluster", low = "orangered", high = "turquoise") +
  labs(title = "Upward Mobility Clusters - Charlotte Metro Area") +
  theme_minimal()

# Map only clusters struggling with upward mobility
ggplot(merged_data %>% filter(cluster_assignments == 1), 
       aes(fill = cluster_assignments)) +
  geom_sf() +  # No need to explicitly specify data argument since filtered data is used
  scale_fill_gradient(name = "Cluster", low = "lightblue", high = "darkred") +
  labs(title = "Areas Struggling with Upward Mobility - Charlotte Metro Area") +
  theme_minimal()
