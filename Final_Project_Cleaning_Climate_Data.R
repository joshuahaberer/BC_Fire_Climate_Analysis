
#You may or may not need all these libraries
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

#######
##Create an csv file with......... 
# Create an empty data frame with specified columns
empty_data <- data.frame(Native.ID = character(), Precip = numeric(), 
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)

csv_file_name <- "BC_AVG_PRECIP1.csv"

# Write the empty data frame to a CSV file
write.csv(empty_data, file = csv_file_name, row.names = FALSE)
########


#Run through all csv files in folder to calculate an aggregate measure of precipitation
# List all CSV files in the directory
#csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
csv_files <- list.files(path = "./WMB", pattern = "\\.csv$", full.names = TRUE)

# Loop through each CSV file
for (file in csv_files) {

#Read each file
hourly_data <- read.csv(file, skip = 1, header = TRUE)
file_name <- file
#Adjust the date/time column so that it is usable in calculations
hourly_data$time <- lubridate::ymd_hms(hourly_data$time)  # Adjust format as needed
class(hourly_data$time)

#Convert rnfl_amt_pst1hr column to numeric and remove NA's
hourly_data$rnfl_amt_pst1hr <- as.numeric(hourly_data$rnfl_amt_pst1hr)
hourly_data <- hourly_data %>%
  filter(!is.na(rnfl_amt_pst1hr))

# Calculate daily average rnfl_amt_pst1hr
daily_avg_precip <- hourly_data %>%
  group_by(date = as.Date(time)) %>%
  summarize(daily_avg_precip = mean(rnfl_amt_pst1hr, na.rm = TRUE))

# Display the daily average precipitation
print(daily_avg_precip)

# Calculate monthly average precipitation
monthly_avg_precip <- hourly_data %>%
  group_by(year = year(time), month = month(time)) %>%
  summarize(monthly_avg_precip = mean(rnfl_amt_pst1hr, na.rm = TRUE)) %>%
  ungroup()  # Ungroup for any future modifications

# Display the monthly average precipitation
print(monthly_avg_precip)


# Filter for the months from May to October
average_precip_may_october <- hourly_data %>%
  filter(month(time) >= 5 & month(time) <= 10) %>%
  summarize(Precip = mean(rnfl_amt_pst1hr, na.rm = TRUE))  # Replace 'rnfl_amt_pst1hr' with your column name

# Display the average precipitation
print(average_precip_may_october)


#Assigning the filename to an object
#Extract the filename (with extension)
file_name <- basename(file_name)

#Remove the file extension
file_name_no_ext <- sub("\\.[^.]*$", "", file_name)

# Display the result
print(file_name_no_ext)

#Read the existing CSV file
file_path <- csv_file_name
data <- read.csv(file_path)

#Print the original data
cat("Original Data:\n")
print(head(data))

#Round the temperature values to two decimals
Roundedprecip <- round(average_precip_may_october,2)

#Convert the weather station ID column to character
data$Native.ID <- as.character(data$Native.ID)

# Step 3: Append new rows
new_values <- data.frame(Native.ID = file_name_no_ext, 
                         TEMP = Roundedprecip, 
                         stringsAsFactors = FALSE)

data <- bind_rows(data, new_values)
print(head(data))

# Print the updated data
cat("Updated Data:\n")
print(head(data))


#Save the updated data frame back to a new CSV file
output_file_path <- csv_file_name
write.csv(data, file = output_file_path, row.names = FALSE)

}

###################
#Merge the climate data for each station with the location data found in the metadata file
metadata <- read.csv("./station_metadata.csv")
precipdata <- read.csv("BC_AVG_PRECIP1.csv")

merged_data <- merge(metadata, precipdata, by = "Native.ID")

#Remove the last two columns which are duplicate Latitude and Longitude
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]

#Change column names for Latitude and Longitude to remove the x
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x","Longitude.x")]<- c("Longitude", "Latitude")

#Omit NA's
merged_data <- na.omit(merged_data)

#There are erroneous temperature values. Filter data to remove these
merged_data <- merged_data[merged_data$Precip <= 100, ]

#Write the dataset so that it is stored
write.csv(merged_data, file = "PrecipData.csv", row.names = FALSE)




