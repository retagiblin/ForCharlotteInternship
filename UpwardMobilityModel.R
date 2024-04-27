## Upward Mobility Reality Model
### Run K-means model with newer data from the U.S. Census

# Read in updated data file
combined_df <- read.csv("CombinedNewUpdwardMobilityTractData.csv", header = TRUE, sep = ",", dec = ".", fill = TRUE)

# Determine typesof datafields
str(combined_df)

# check for missing values
sum(is.na(combined_df)) 

# Remove rows with NAs
combined_df <- na.omit(combined_df)

# Convert column number and GEOID to a string to omit from clustering
combined_df$X <- as.character(combined_df$X)
combined_df$GEOID <- as.character(combined_df$GEOID)

numeric_cols <- sapply(combined_df, is.numeric)
combined_df_scaled <- scale(combined_df[, numeric_cols])

# Elbow Method -------------------------------------
# use the elbow method: look for the "bend" on the plot
fviz_nbclust(combined_df_scaled, kmeans, method = "wss") 

# use the silhouette method
fviz_nbclust(combined_df_scaled, kmeans, method = "silhouette") 

# Do K-means clustering
cluster_upward_mobility = kmeans(combined_df_scaled, centers = 2)
cluster_upward_mobility$size

# plot the clusters
fviz_cluster(cluster_upward_mobility, data = combined_df_scaled) + theme_classic()

# Cluster centroids
cluster_centers <- cluster_upward_mobility$centers
#print(cluster_centers)

# Cluster assignments
cluster_assignments <- cluster_upward_mobility$cluster
#print(cluster_assignments)

# Within-cluster sum of squared (WCSS)
within_clustier_sum_of_squares <- cluster_upward_mobility$withinss
#print(within_clustier_sum_of_squares)

# Combine dataframe with cluster assignments to get data frame with data and cluster assignment
upward_mobility_with_clusters <- cbind(combined_df, cluster_assignments)
print(upward_mobility_with_clusters)


################################################################################
## Map clusters

NC_county_codes = c("007", "025", "071", "097" ,"109", "119", "159", "179")
SC_county_codes = c("023", "057", "091")
NCtracts <- tracts(state = "NC", county=NC_county_codes)
SCtracts <- tracts(state = "SC", county=SC_county_codes)
tracts <- rbind(NCtracts, SCtracts)

merged_data <- merge(x = tracts, y = upward_mobility_with_clusters, all.y = TRUE, by.x = "GEOID", by.y = "GEOID")

# Plot the map of both clusters using ggplot2
ggplot() +
  geom_sf(data = merged_data, aes(fill = cluster_assignments)) +
  scale_fill_gradient(name = "Cluster", low = "turquoise", high = "orangered") +
  labs(title = "Upward Mobility Clusters - Charlotte Metro Area") +
  theme_minimal()

# Map only clusters struggling with upward mobility
ggplot(merged_data %>% filter(cluster_assignments == 1), 
       aes(fill = cluster_assignments)) +
  geom_sf() +  # No need to explicitly specify data argument since filtered data is used
  scale_fill_gradient(name = "Cluster", low = "turquoise", high = "orangered") +
  labs(title = "Areas Struggling with Upward Mobility - Charlotte Metro Area") +
  theme_minimal()

subset_merged_data <- merged_data[merged_data$COUNTYFP %in% c("119"), ]

# Plot the map of household_income
ggplot() +
  geom_sf(data = subset_merged_data, aes(fill = household_income)) +
  scale_fill_gradient(name = "Cluster", low = "red", high = "blue") +
  labs(title = "Household Income - Charlotte Metro Area") +
  theme_minimal()

# Plot the map of employment_rate
ggplot() +
  geom_sf(data = subset_merged_data, aes(fill = employment_rate)) +
  scale_fill_gradient(name = "Cluster", low = "white", high = "darkgreen") +
  labs(title = "Employment Rate - Charlotte Metro Area") +
  theme_minimal()

# Plot the map of teen birth_rate
ggplot() +
  geom_sf(data = merged_data, aes(fill = birth_rate)) +
  scale_fill_gradient(name = "Cluster", low = "white", high = "pink") +
  labs(title = "Teen Birth Rate - Charlotte Metro Area") +
  theme_minimal()
