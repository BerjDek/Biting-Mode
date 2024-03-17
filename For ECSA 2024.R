
library(tidyverse)
install.packages("revgeo")
library(revgeo)
library(purrr)
,
Rprt_Lat = current_location_lat,
Rprt_Lon =current_location_lon

install.packages("tmaptools")
library(tmaptools)

#New Script

raw_reports_data <- read.csv(file="full_reports_table.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))

colnames(Raw_reports_data)

reports_data <- raw_reports_data %>%
  dplyr::select(user_id, location_choice, creation_time, type, current_location_lat, current_location_lon, selected_location_lon,selected_location_lat) %>% 
  rename(User_ID = user_id,
         Rprt_Loc_Choice = location_choice,
         Rprt_Date = creation_time,
         Rprt_Type = type)

reports_data <- reports_data %>% 
  mutate(Long = coalesce(current_location_lon, selected_location_lon),
         Lat  = coalesce(current_location_lat, selected_location_lat))%>%
  dplyr::select(-current_location_lon, -selected_location_lon, -current_location_lat, -selected_location_lat)

reports_data_2 <- head(reports_data)




summary(reports_data_2)



# Load necessary libraries
library(tidygeocoder)

# Assuming reports_data_2 is your dataframe
# Make sure the Long and Lat columns are numeric
reports_data_2$Long <- as.numeric(reports_data_2$Long)
reports_data_2$Lat <- as.numeric(reports_data_2$Lat)

# Use the reverse_geocode function to get location information based on Long and Lat
# Here we're specifying that we want the city name, using custom_query to get specific components
# Note: This operation might take some time depending on the size of your data and the API's response time
geocoded_data <- reverse_geocode(
  reports_data_2,
  lat = "Lat",
  long = "Long",
  method = 'osm' # Using OpenStreetMap's Nominatim service
)


geocoded_data_2 <- reverse_geocode(
  reports_data_2,
  lat = "Lat",
  long = "Long",
  method = 'osm', # Using OpenStreetMap's Nominatim service
  return_type = "detailed"
)

?reverse_geocode
# Combine the geocoded data with your original dataset
# Here, we're assuming that the city name is returned in the label column of geocoded_data
# This might vary based on the geocoding service and you might need to adjust the column name accordingly
reports_data_2$City_Name <- geocoded_data$place_name

# View the updated dataset
print(reports_data_2)

summary(geocoded_data)
