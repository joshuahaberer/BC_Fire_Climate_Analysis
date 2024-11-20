library(sf)
library(ggplot2)
library(dplyr)
library(terra)

# Step 1: Load the precipitation data and convert it into an SF object
precip_data <- read.csv("PrecipData.csv")

# Ensure Latitude and Longitude are numeric
precip_data <- precip_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Convert the precipitation data to an SF object
precip_sf <- st_as_sf(precip_data, coords = c("Longitude", "Latitude"), crs = 4326)  # WGS84 (EPSG:4326)

# Step 2: Transform the precipitation data to BC Albers (EPSG: 3005)
precip_sf <- st_transform(precip_sf, crs = 3005)

# Step 3: Load the BC boundary shapefile and ensure it’s in BC Albers
bc_boundary_raw <- vect("BC_bound.shp")  # Read BC boundary using terra
bc_boundary_albers <- project(bc_boundary_raw, "EPSG:3005")  # Re-project BC boundary to BC Albers

# Convert BC boundary to SF for ggplot
bc_boundary_albers_sf <- st_as_sf(bc_boundary_albers)

# Step 4: Plot the map with ggplot2
ggplot() +
  # Plot BC Boundary
  geom_sf(data = bc_boundary_albers_sf, fill = "lightgrey", color = "lightblue") +
  
  # Plot Precipitation Data Points
  geom_sf(data = precip_sf, aes(color = Precip), size = 2) +  # Adjust size if needed
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for precipitation values
  theme_minimal() +
  
  # Add Titles and Labels
  labs(title = "Map of Precipitation Data Points in British Columbia",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",
       y = "Latitude",
       color = "Precipitation (mm)") +
  theme(legend.position = "bottom")
