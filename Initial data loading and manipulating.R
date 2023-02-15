#Downloading the data
setwd("G:/My Drive/Biting Research/Biting-Mode (R)")
install.packages("rjson")
install.packages("qdapTools")
install.packages("splitstackshape")
install.packages("ggmap")
install.packages('tidygeocoder')

library(rjson)
library(tidyr)
library(dplyr)
library(qdapTools)
library(splitstackshape)
library(ggmap)
library(tidygeocoder)

#Loading Data


Reports_2019 <- fromJSON(file = "all_reports2019.json")

Reports_2019<- stringi::stri_list2matrix(Reports_2019, byrow = TRUE, fill = "")
Reports_2019 <- as.data.frame(Reports_2019)
Reports_2019 <- Reports_2019 %>% rename(report_id = V1, report_date = V3, report_type = V8, Longitude = V9, Latitude = V10)
summary(Reports_2019)



Reports_2021 <- fromJSON(file = "all_reports2021.json")

Reports_2021<- stringi::stri_list2matrix(Reports_2021, byrow = TRUE, fill = "")
Reports_2021 <- as.data.frame(Reports_2021)
Reports_2021 <- Reports_2021 %>% rename(report_id = V1, report_date = V3, report_type = V8, Longitude = V9, Latitude = V10)
summary(Reports_2021)

#loading data set that has both user_Id and Version_id, the data set from Lime survey has the first and data set with the report has the latter
UUID_Conv <- read.csv(file="user_reports_091122.csv", header = TRUE) %>% 
  rename(report_id = version_UUID) 


#adding User Id to the reports data set
Reports_2019 <- inner_join(Reports_2019, UUID_Conv, by = "report_id") 
Reports_2021 <- inner_join(Reports_2021, UUID_Conv, by = "report_id") 

Reports_2019$Latitude <- as.numeric(Reports_2019$Latitude)
Reports_2019$Longitude <- as.numeric(Reports_2019$Longitude)



# use tidycoder() to convert the longitude and latitude to reverse geocode 



reverse <- Reports_2019 %>%
  reverse_geocode(lat = Latitude, long = Longitude, method = 'osm',
                  address = address_found, full_results = TRUE)

reverse_2021 <- Reports_2021 %>%
  reverse_geocode(lat = Latitude, long = Longitude, method = 'osm',
                  address = address_found, full_results = TRUE)

#Things to check.
#average number of reports per user
#distributin over the season, 
#distribution of people filling biting reports (gender, age) use other data
#what are the single reports, 
#amount of time spent on app total per individual (higher better), amount of time spent per report
#number of correct reports from those checked (if needed,)

#things to ask agusti: can we get the amount of time spent on app, number of clicks. also can we get date of download with user uuid.

#steps to do tomorrow: 1- remove  the unnecasery reports


#Selecting the columns important for 
Data_2019 <- reverse %>% 
  select(user_id, report_id, report_date, report_type, V6, state_district, state) %>% 
  rename(month = V6, district = state_district)
write.csv(Data_2019, file = "Data_2019.csv", row.names = FALSE)


Data_2021 <- reverse_2021 %>% 
  select(user_id, report_id, report_date, report_type, V6, state_district, state) %>% 
  rename(month = V6, district = state_district)
write.csv(Data_2021, file = "Data_2021.csv", row.names = FALSE)


#Show distribution by district

Data_2019 %>% 
  group_by(district) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
# 1544 entries from Barcelona 522 NA

Data_2019_top_10 <- Data_2019 %>% 
  group_by(district) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10, count)

ggplot(Data_2019_top_10, aes(x = reorder(district, count), y = count)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("district") + 
  ylab("Count") + 
  ggtitle("Distribution of 2019 Data by Top 10 Districts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Data_2021 %>% 
  group_by(district) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
# 5907 entries from Barcelona, 39638 NA

Data_2021_top_10 <- Data_2021 %>% 
  group_by(district) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10, count)

ggplot(Data_2021_top_10, aes(x = reorder(district, count), y = count)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("district") + 
  ylab("Count") + 
  ggtitle("Distribution of 2021 Data by Top 10 Districts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Show distribution by state

Data_2019 %>% 
  group_by(state) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
# 1843 entries from Catalunya 65 NA, Lombardia  39,

Data_2019_top_10 <- Data_2019 %>% 
  group_by(state) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10, count)

ggplot(Data_2019_top_10, aes(x = reorder(state, count), y = count)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("state") + 
  ylab("Count") + 
  ggtitle("Distribution of 2019 Data by Top 10 states") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(Data_2019_top_10)



Data_2021 %>% 
  group_by(state) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
# 6768 entries from Catalunya 4164 NA, Lombardia      1489

Data_2021_top_10 <- Data_2021 %>% 
  group_by(state) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10, count)

ggplot(Data_2021_top_10, aes(x = reorder(state, count), y = count)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("state") + 
  ylab("Count") + 
  ggtitle("Distribution of 2021 Data by Top 10 states") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(Data_2021_top_10)





#distribution by report type
Data_2019 %>% 
  group_by(report_type) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

Data_2019_report_type <- Data_2019 %>% 
  group_by(report_type) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = (count / sum(count)) * 100)
#around 70/30 split for adult reports

ggplot(Data_2019_report_type, aes(x = report_type, y = percentage)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("Report Type") + 
  ylab("Percentage") + 
  ggtitle("Percentage Distribution by Report Type 2019") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) 

rm(Data_2019_report_type)



Data_2021 %>% 
  group_by(report_type) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

Data_2021_report_type <- Data_2021 %>% 
  group_by(report_type) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = (count / sum(count)) * 100)
#60 percent for adults, only 8 for sites, and 32 percent for bite reports

ggplot(Data_2021_report_type, aes(x = report_type, y = percentage)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("Report Type") + 
  ylab("Percentage") + 
  ggtitle("Percentage Distribution by Report Type 2021") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) 

rm(Data_2021_report_type)



#distribution of report type in Catalunya

Data_2019_catalunya_report_type <- Data_2019 %>% 
  filter(state == "Catalunya") %>% 
  group_by(report_type) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = (count / sum(count)))



ggplot(Data_2019_catalunya_report_type, aes(x = report_type, y = percentage, fill = report_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution by Report Type in Catalunya",
       x = "Report Type",
       y = "Percentage",
       fill = "Report Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

rm(Data_2019_catalunya_report_type)





######CAN REMOVE LATER

#This code first filters the data to only include entries from Catalunya. It then calculates the percentage distribution by report type for the 
#filtered data and for the overall data. The percentage difference is calculated by subtracting the overall percentage from the Catalunya percentage. 
#The data is then combined into a data frame and visualized using ggplot. The bars for the percentage difference are colored blue, while the bars for 
#the overall percentage are colored orange. The y-axis labels are set to show only absolute values using the scale_y_continuous() function. 
#The legend is reversed to show the blue bars on top using the guides() function.


# Filter data for entries from Catalunya
catalunya_data_2019 <- subset(Data_2019, state == "Catalunya")

# Calculate the percentage distribution by report type
catalunya_pct <- round(prop.table(table(catalunya_data_2019$report_type)) * 100, 2)

# Calculate the percentage difference from overall data
overall_pct <- round(prop.table(table(Data_2019$report_type)) * 100, 2)
pct_diff <- catalunya_pct - overall_pct

# Combine the data into a data frame
dist_Data_2019 <- data.frame(
  report_type = names(catalunya_pct),
  catalunya_pct = catalunya_pct,
  overall_pct = overall_pct,
  pct_diff = pct_diff
)

# Create the visualization
ggplot(dist_Data_2019, aes(x = report_type)) +
  geom_bar(aes(y = pct_diff), stat = "identity", fill = "#0072B2") +
  geom_text(aes(y = pct_diff, label = paste0(pct_diff, "%")), vjust = -0.5) +
  geom_bar(aes(y = overall_pct), stat = "identity", fill = "#E69F00") +
  geom_text(aes(y = overall_pct, label = paste0(overall_pct, "%")), vjust = -0.5) +
  labs(x = "Report Type", y = "Percentage Difference") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(labels = function(x) abs(x)) +
  guides(fill = guide_legend(reverse = TRUE))


# Filter data for entries from Catalunya
catalunya_data_2021 <- subset(Data_2021, state == "Catalunya")

# Calculate the percentage distribution by report type
catalunya_pct <- round(prop.table(table(catalunya_data_2021$report_type)) * 100, 2)

# Calculate the percentage difference from overall data
overall_pct <- round(prop.table(table(Data_2021$report_type)) * 100, 2)
pct_diff <- catalunya_pct - overall_pct

# Combine the data into a data frame
dist_Data_2021 <- data.frame(
  report_type = names(catalunya_pct),
  catalunya_pct = catalunya_pct,
  overall_pct = overall_pct,
  pct_diff = pct_diff
)

# Create the visualization
ggplot(dist_Data_2021, aes(x = report_type)) +
  geom_bar(aes(y = pct_diff), stat = "identity", fill = "#0072B2") +
  geom_text(aes(y = pct_diff, label = paste0(pct_diff, "%")), vjust = -0.5) +
  geom_bar(aes(y = overall_pct), stat = "identity", fill = "#E69F00") +
  geom_text(aes(y = overall_pct, label = paste0(overall_pct, "%")), vjust = -0.5) +
  labs(x = "Report Type", y = "Percentage Difference") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(labels = function(x) abs(x)) +
  guides(fill = guide_legend(reverse = TRUE))



#Extract a vector of all user Id's of those of who participated in the survey (data loaded in another Tab)
vec <- data$user_id
#Reduce the reports to only those that have been given by those who have filled the survey
Reports <-  Reports_2019[Reports_2019$user_id %in% vec,]

#percentage of reports coming from those who participated in the survey
count(Reports)/count(Reports_2019)*100

#add report count data and remove report ID
Reports <- Reports %>% 
  group_by(user_id) %>% 
  mutate(report_count = n()) %>% 
  select(-report_id)


#by report type (will be added to messages dataframe)
Report_Number_Type <- Reports %>%
  select(-report_date,-report_count) %>% 
  group_by(report_type)


Report_Number_Type <- as.data.frame(table(Report_Number_Type$user_id, Report_Number_Type$report_type))

Report_Number_Type <- Report_Number_Type %>% pivot_wider(names_from = Var2, values_from = Freq)%>% 
  rename(user_id = Var1, bite_report = bite, site_report = site, adult_report = adult) %>% 
  mutate(total_reports = (bite_report+ adult_report+ site_report))

#by report date

Report_date <- left_join(Reports,Messages, by = "user_id") %>% 
  select(user_id, report_date, first_msg_date, last_msg_date) %>% 
  mutate(month_before = first_msg_date - 26 ) %>% 
  mutate(month_after = last_msg_date + 26) %>% 
  mutate(report_before_msg = case_when(report_date < month_before ~ 0, report_date < first_msg_date & report_date >= month_before ~ 1, report_date >= first_msg_date ~ 0)) %>% 
  mutate(report_during_msg = case_when(report_date >= first_msg_date & report_date <= last_msg_date ~ 1, report_date < first_msg_date | report_date > last_msg_date ~ 0))%>% 
  mutate(report_after_msg = case_when(report_date <= last_msg_date ~ 0, report_date > last_msg_date & report_date <= month_after ~ 1,report_date > month_after ~ 0 )) %>% 
  group_by(user_id) %>%
  mutate(before = sum(report_before_msg))%>%
  mutate(during = sum(report_during_msg))%>%
  mutate(after = sum(report_after_msg))