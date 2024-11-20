# Load libraries
library(sf)      #for handling shapefiles
library(gstat)   #for Kriging
library(ggplot2) #for plotting
library(dplyr)   #for data manipulation
library(raster)
library(tm)
library(tmap)

dir <- "~/Desktop/GEOG418_GeoStatistics"
setwd(dir)

# Read the shapefile
climate_data <- st_read("ClimateData.shp")

f.0 <- as.formula(TEMP ~ 1) 

# Create variogram
var.smpl <- variogram(f.0, climate_data, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = TRUE, fit.sills = TRUE,
                          vgm(model="Sph", nugget = 5, psill = 10, 
                              range = 7000))
plot(var.smpl, dat.fit)

# Define the grid
xmin <- st_bbox(climate_data)$xmin
xmax <- st_bbox(climate_data)$xmax
ymin <- st_bbox(climate_data)$ymin
ymax <- st_bbox(climate_data)$ymax

# Create a regular grid
n <- 50000  # Number of points
grd <- st_as_sf(expand.grid(x = seq(xmin, xmax, length.out = sqrt(n)),
                            y = seq(ymin, ymax, length.out = sqrt(n))),
                coords = c("x", "y"), crs = st_crs(climate_data))

dat.krg <- krige(f.0, climate_data, grd, dat.fit, debug.level=0)

# Convert the kriging output to an sf object
kriging_results_sf <- st_as_sf(dat.krg)

# Create a Raster from Kriging Results
# 1. Convert to a data frame with coordinates for raster creation
coords_df <- as.data.frame(st_coordinates(kriging_results_sf))
coords_df$predicted_temp <- kriging_results_sf$var1.pred  # Replace with your prediction column

# 2. Create the raster from the resulting data frame
predicted_raster <- rasterFromXYZ(coords_df)

# Visualize the raster
#tm_shape(predicted_raster) +
  #tm_raster(palette = "viridis", title = "Predicted Temperature") +
  #tm_layout(title = "Kriging Results for Temperature") +
  #tm_compass(position = c("right", "top")) +
  #tm_scale_bar()

#clip the raster to bc boundary
bc_boundary<- st_read ("./BC_bound.shp")
st_transform(bc_boundary, crs= 3005)

bc_boundary <- st_read("./BC_bound.shp") %>% 
  st_transform(crs = 3005)
clipped_raster <-mask(predicted_raster,bc_boundary)

tm_shape(clipped_raster) +
  tm_raster(palette = viridis(10), title = "Predicted Temperature") +  #add title for legend
  tm_shape(bc_boundary) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(
    title = "Kriging Results for Temperature",  #main plot title
    title.size = 1.2,
    title.position = c("center", "top"),
    legend.position = c("left", "bottom")  #change legend position here
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))




