library(sf)
library(ggplot2)
library(dplyr)

library(tmap)
library(spdep)
library(raster)
library(sf)
library(lubridate)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)

#Set working directory
dir <- "~/Desktop/GEOG418_GeoStatistics"
setwd(dir)

# Read the CSV file
precip_data <- read.csv("PrecipData.csv")

# Ensure Latitude and Longitude columns are correctly formatted
# Assuming the columns are named "Latitude" and "Longitude"
precip_data <- precip_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Create a simple feature object (sf) using Latitude and Longitude
precip_sf <- st_as_sf(precip_data, coords = c("Longitude", "Latitude"), crs = 3005) #make BC crs

# Optionally, you can select columns that you want to keep in the shapefile
# climate_sf <- climate_sf %>% select(Your_Columns_Here)

# Write the shapefile to disk
st_write(precip_sf, "PrecipData.shp")

# Confirmation message
print("Shapefile has been created: ClimateData.shp")

# Load the shapefiles
precip_sf <- st_read("./precipData.shp") %>%
  st_transform(crs = 3005)

bc_boundary <- st_read("BC_bound.shp") %>%
  st_transform(crs = 3005)

bc_boundary <- st_set_crs(bc_boundary, 3005)


# Create the map
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "lightblue") +
  # Map the TEMP variable to color
  geom_sf(data = precip_sf, aes(color = Precip), size = 2) + 
  scale_color_gradient(low = "blue", high = "red") + # Adjust color gradient as needed
  theme_minimal() +
  labs(title = "Map of Climate Data Points in British Columbia",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",  # Use Longitude for x-axis
       y = "Latitude",   # Use Latitude for y-axis
       color = "Temperature (°C)") + # Label for color legend
  theme(legend.position = "bottom")

library(sf)
library(ggplot2)
library(dplyr)
library(terra)

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




