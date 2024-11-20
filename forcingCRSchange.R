library(sf)

# Step 1: Read the shapefile
bc_boundary_raw <- st_read("BC_bound.shp")

# Step 2: Check the initial CRS of the shapefile
initial_crs <- st_crs(bc_boundary_raw)
print(paste("Initial CRS:", initial_crs))

# Step 3: Force the CRS to WGS84 (EPSG:4326) if it's not already set
# (this is useful if the CRS is missing or incorrectly assigned)
bc_boundary_raw <- st_set_crs(bc_boundary_raw, 4326)

# Step 4: Re-transform to BC Albers (EPSG: 3005)
bc_boundary_albers <- st_transform(bc_boundary_raw, crs = 3005)

# Step 5: Verify the CRS again after transformation
final_crs <- st_crs(bc_boundary_albers)
print(paste("Final CRS:", final_crs))

# Step 6: Check if the CRS was correctly transformed
if (final_crs$epsg == 3005) {
  print("BC boundary is now in BC Albers (EPSG: 3005).")
} else {
  print("Transformation failed!")
}

#####################
install.packages("terra")
library(terra)

# Step 1: Read the shapefile using terra
bc_boundary_raw <- vect("BC_bound.shp")

# Step 2: Check the initial CRS of the shapefile
initial_crs <- crs(bc_boundary_raw)
print(paste("Initial CRS:", initial_crs))

# Step 3: If needed, explicitly set CRS to WGS84 (EPSG:4326)
crs(bc_boundary_raw) <- "EPSG:4326"

# Step 4: Re-project to BC Albers (EPSG:3005)
bc_boundary_albers <- project(bc_boundary_raw, "EPSG:3005")

# Step 5: Check the final CRS after transformation
final_crs <- crs(bc_boundary_albers)
print(paste("Final CRS:", final_crs))

# Step 6: Plot the boundary to confirm it's in the correct projection
plot(bc_boundary_albers, main = "BC Boundary in BC Albers Projection")

# Proceed with your ggplot mapping
library(ggplot2)
bc_boundary_albers_sf <- st_as_sf(bc_boundary_albers)

ggplot() +
  geom_sf(data = bc_boundary_albers_sf, fill = "lightgrey", color = "lightblue") +
  ggtitle("BC Boundary in BC Albers Projection") +
  theme_minimal()


# Step 4: Plot the map using ggplot
ggplot() +
  # Plot BC Boundary
  geom_sf(data = bc_boundary_albers_sf, fill = "lightgrey", color = "lightblue") +
  
  # Plot Precipitation Data
  geom_sf(data = precip_sf, aes(color = Precip), size = 2) +  # Adjust color/size as needed
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for precipitation
  theme_minimal() +
  
  # Add Titles and Labels
  labs(title = "Map of Precipitation Data Points in British Columbia",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",
       y = "Latitude",
       color = "Precipitation (mm)") +
  theme(legend.position = "bottom")

