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
Data_2019$count <- ave(Data_2019$user_id, Data_2019$user_id, FUN = length)
Data_2019$count<- as.numeric(Data_2019$count)
write.csv(Data_2019, file = "Data_2019.csv", row.names = FALSE)


Data_2021 <- reverse_2021 %>% 
  select(user_id, report_id, report_date, report_type, V6, state_district, state) %>% 
  rename(month = V6, district = state_district)
Data_2021$count <- ave(Data_2021$user_id, Data_2021$user_id, FUN = length)
Data_2021$count<- as.numeric(Data_2021$count)
write.csv(Data_2021, file = "Data_2021.csv", row.names = FALSE)

rm(Reports_2019,Reports_2021, UUID_Conv) #keeping reverse since it takes forever to complete

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

rm(Data_2019_top_10, Data_2021_top_10)





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
#53/46 instead of 70/30

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

#adult/site/bite goes from 60/8/32 to 33/41/26

rm(catalunya_pct, overall_pct,pct_diff,dist_Data_2019, dist_Data_2021)

#both in the case of 2019 and 2021, we see a huge increase of site reports most likely  coming from Santi and school interventions, must ask john about it



#To try to make sense of it, i will check what percentage of reports  are contributed by different percentages of users 

# Count the number of unique users and reports
num_users <- length(unique(Data_2019$user_id))
num_reports <- nrow(Data_2019)

# Calculate the average number of reports per user
avg_reports_per_user <- num_reports / num_users

# Calculate the cumulative percentage of reports for each percentage of users
user_counts <- table(Data_2019$user_id)
report_counts <- table(user_counts)
cumulative_report_percents <- cumsum(report_counts / num_reports) * 100

# Print the results
cat(sprintf("Total number of unique users: %d\n", num_users))
cat(sprintf("Total number of reports: %d\n", num_reports))
cat(sprintf("Average number of reports per user: %.2f\n", avg_reports_per_user))
cat(sprintf("Percentage of reports contributed by different percentages of users:\n"))

###Total number of unique users: 1370, Total number of reports: 3177, Average number of reports per user: 2.32


# 2021
num_users <- length(unique(Data_2021$user_id))
num_reports <- nrow(Data_2021)

# Calculate the average number of reports per user
avg_reports_per_user <- num_reports / num_users

# Calculate the cumulative percentage of reports for each percentage of users
user_counts <- table(Data_2021$user_id)
report_counts <- table(user_counts)
cumulative_report_percents <- cumsum(report_counts / num_reports) * 100

# Print the results
cat(sprintf("Total number of unique users: %d\n", num_users))
cat(sprintf("Total number of reports: %d\n", num_reports))
cat(sprintf("Average number of reports per user: %.2f\n", avg_reports_per_user))
cat(sprintf("Percentage of reports contributed by different percentages of users:\n"))

###Total number of unique users: 18205, Total number of reports: 50450, Average number of reports per user: 2.77

### 2019 Catalunya
# Count the number of unique users and reports
num_users <- length(unique(catalunya_data_2019$user_id))
num_reports <- nrow(catalunya_data_2019)

# Calculate the average number of reports per user
avg_reports_per_user <- num_reports / num_users

# Calculate the cumulative percentage of reports for each percentage of users
user_counts <- table(catalunya_data_2019$user_id)
report_counts <- table(user_counts)
cumulative_report_percents <- cumsum(report_counts / num_reports) * 100

# Print the results
cat(sprintf("Total number of unique users: %d\n", num_users))
cat(sprintf("Total number of reports: %d\n", num_reports))
cat(sprintf("Average number of reports per user: %.2f\n", avg_reports_per_user))


###Total number of unique users: 620, Total number of reports: 1843, Average number of reports per user: 2.97

### 2021 Catalunya
# Count the number of unique users and reports
num_users <- length(unique(catalunya_data_2021$user_id))
num_reports <- nrow(catalunya_data_2021)

# Calculate the average number of reports per user
avg_reports_per_user <- num_reports / num_users

# Calculate the cumulative percentage of reports for each percentage of users
user_counts <- table(catalunya_data_2021$user_id)
report_counts <- table(user_counts)
cumulative_report_percents <- cumsum(report_counts / num_reports) * 100

# Print the results
cat(sprintf("Total number of unique users: %d\n", num_users))
cat(sprintf("Total number of reports: %d\n", num_reports))
cat(sprintf("Average number of reports per user: %.2f\n", avg_reports_per_user))

###Total number of unique users: 1148, Total number of reports: 6768, Average number of reports per user: 5.9

rm(num_users, num_reports, avg_reports_per_user, user_counts, report_counts, cumulative_report_percents)




###User report Percent for eac
#2019
# Count number of reports per user
user_report_count <- Data_2019 %>%
  group_by(user_id) %>%
  summarise(report_count = n())

# Calculate percentage of total reports contributed by each user
user_report_percent_2019 <- user_report_count %>%
  mutate(report_percent = (report_count / sum(report_count)) * 100)

#2021
# Count number of reports per user
user_report_count <- Data_2021 %>%
  group_by(user_id) %>%
  summarise(report_count = n())

# Calculate percentage of total reports contributed by each user
user_report_percent_2021 <- user_report_count %>%
  mutate(report_percent = (report_count / sum(report_count)) * 100)

# Catalunya 2019
# Count number of reports per user
user_report_count <- catalunya_data_2019 %>%
  group_by(user_id) %>%
  summarise(report_count = n())

# Calculate percentage of total reports contributed by each user
user_report_percent_Catalunya_2019 <- user_report_count %>%
  mutate(report_percent = (report_count / sum(report_count)) * 100)

# Catalunya 2021
# Count number of reports per user
user_report_count <- catalunya_data_2021 %>%
  group_by(user_id) %>%
  summarise(report_count = n())

# Calculate percentage of total reports contributed by each user
user_report_percent_Catalunya_2021 <- user_report_count %>%
  mutate(report_percent = (report_count / sum(report_count)) * 100)

rm(user_report_count,user_report_percent, user_report_percent_2019, user_report_percent_2021, user_report_percent_Catalunya_2019, user_report_percent_Catalunya_2021)

#top participants influence significantly to the percentage of total reports in Catalunya more so than general and in 2021 more than 2022


#since top 10 percent of participants provide anywhere from 47 to (ebird), to 70 (galaxy zoo) percent of entires come from the top 10 percent of participants
# with an avarage of 50% (foldit) it might be wise to exlude the top 10% of contributers



# cleaning the data from the top 10 percent of reporters

# calculate report counts per user 2019
user_counts <- table(Data_2019$user_id)

# identify the top 10% of users by report count
top_users <- names(head(sort(user_counts, decreasing = TRUE), nrow(user_counts) * 0.1))

# remove reports from top users
Data_2019_filtered <- Data_2019[!(Data_2019$user_id %in% top_users), ]



# assume df is the data frame containing report information with columns "user" and "report_type"

# calculate report counts per user
user_counts <- table(Data_2021$user_id)

# identify the top 10% of users by report count
top_users <- names(head(sort(user_counts, decreasing = TRUE), nrow(user_counts) * 0.1))

# remove reports from top users
Data_2021_filtered <- Data_2021[!(Data_2021$user_id %in% top_users), ]


rm(top_users,user_counts,Data_2019_filtered,Data_2021_filtered)


# Define function to remove top user_ids and plot count distribution
remove_top_user_ids <- function(data, percent_top_user_ids) {
  # Compute cutoff for number of reports by top user_ids to remove
  cutoff <- quantile(data$count, 1 - percent_top_user_ids/100)
  # Identify top user_ids based on cutoff
  top_user_ids <- unique(data$user_id[data$count >= cutoff])
  
  # Remove reports by top user_ids
  data_filtered <- data[!(data$user_id %in% top_user_ids),]
  
  # Plot count distribution before and after removal of top user_ids
  ggplot() + 
    geom_histogram(data = data, aes(x = count), bins = 20, alpha = 0.5, fill = "blue", color = "black") +
    geom_histogram(data = data_filtered, aes(x = count), bins = 20, alpha = 0.5, fill = "green", color = "black") +
    labs(x = "count", y = "Frequency", title = paste0(percent_top_user_ids, "% of top user_ids removed"))
}

remove_top_user_ids(Data_2019,10)
remove_top_user_ids(Data_2021,10)





remove_top_users <- function(data) {
  # Compute total report count for each user
  user_reports <- data %>% group_by(user_id) %>% summarize(total_reports = n())
  
  # Compute the threshold for the top 10% of users
  top_users_threshold <- quantile(user_reports$total_reports, 0.9)
  
  # Get the user IDs for the top users
  top_user_ids <- user_reports %>% filter(total_reports >= top_users_threshold) %>% select(user_id)
  
  # Filter out the reports from the top users
  filtered_data <- data %>% filter(!user_id %in% top_user_ids$user_id)
  
  # Create a plot of report counts before and after removing top users
  report_counts <- data %>% group_by(user_id) %>% summarize(total_reports = n())
  filtered_report_counts <- filtered_data %>% group_by(user_id) %>% summarize(total_reports = n())
  
  ggplot() +
    geom_histogram(aes(x = total_reports), data = report_counts, binwidth = 1, fill = "blue", alpha = 0.5) +
    geom_histogram(aes(x = total_reports), data = filtered_report_counts, binwidth = 1, fill = "red", alpha = 0.5) +
    scale_x_continuous(limits = c(0, max(report_counts$total_reports)), breaks = seq(0, max(report_counts$total_reports), 10)) +
    labs(x = "Report count", y = "Frequency", title = "Report counts before and after removing top users") +
    text(x = 50, y = max(data$report_id)/2,
         label = paste0("Total reports before filtering: ", n)) +
    text(x = 50, y = max(data$report_id)/2 - 10,  
         label = paste0("Total reports after filtering: ", nrow(df_filtered)))
 
}

remove_top_users(Data_2019)



filter_top_users <- function(df, top_percent) {
  # Compute total number of reports
  n <- nrow(df)
  
  # Compute threshold for top users
  top_threshold <- quantile(df$count, 1 - (top_percent / 100))
  
  # Filter top users
  filtered_df <- df %>% filter(count <= top_threshold)
  
  # Compute total number of reports after filtering
  n_filtered <- nrow(filtered_df)
  
  # Create histogram of number of reports
  ggplot(df, aes(count)) +
    geom_histogram(binwidth = 10, fill = "cornflowerblue", color = "black") +
    geom_vline(xintercept = top_threshold, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = top_threshold, y = 0, vjust = 1.2, label = paste0("Top ", top_percent, "% users"), color = "red", hjust = -0.25, vjust = 1) +
    labs(title = "Distribution of Report Counts",
         x = "Number of Reports",
         y = "Frequency") +
    geom_text(aes(x = n_filtered, y = max(hist(df$count, breaks = seq(0, max(df$count), by = 1), plot = FALSE)$counts),
                  label = paste0("Total reports after filtering: ", n_filtered)),
              hjust = 0.25, vjust = 0.5, size = 3, color = "black") +
    geom_text(aes(x = n, y = max(hist(df$count, breaks = seq(0, max(df$count), by = 1), plot = FALSE)$counts),
                  label = paste0("Total reports before filtering: ", n)),
              hjust = 0.5, vjust = 0.25, size = 3, color = "black") +
  scale_x_continuous(limits = c(0, 1000)) +
}

filter_top_users(Data_2019,10)



filter_top_users <- function(df, top_percent) {
  # Compute total number of reports
  n <- nrow(df)
  
  # Compute threshold for top users
  top_threshold <- quantile(df$count, 1 - (top_percent / 100))
  
  # Filter top users
  filtered_df <- df %>% filter(count <= top_threshold)
  
  # Compute total number of reports after filtering
  n_filtered <- nrow(filtered_df)
  
  # Create histogram of number of reports
  ggplot(df, aes(count)) +
    geom_histogram(binwidth = 10, fill = "cornflowerblue", color = "black") +
    scale_x_continuous(limits = c(0, max(df$count))) + # Set x-axis limits
    geom_vline(xintercept = top_threshold, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = top_threshold, y = 0, vjust = 1.2, label = paste0("Top ", top_percent, "% users"), color = "red", hjust = -0.25, vjust = 1) +
    labs(title = "Distribution of Report Counts",
         x = "Number of Reports",
         y = "Frequency") +
    geom_text(aes(x = n_filtered, y = max(hist(df$count, breaks = seq(0, max(df$count), by = 1), plot = FALSE)$counts),
                  label = paste0("Total reports after filtering: ", n_filtered)),
              hjust = 0.25, vjust = 0.5, size = 3, color = "black") +
    geom_text(aes(x = n, y = max(hist(df$count, breaks = seq(0, max(df$count), by = 1), plot = FALSE)$counts),
                  label = paste0("Total reports before filtering: ", n)),
              hjust = 0.5, vjust = 0.25, size = 3, color = "black")
}

filter_top_users(Data_2019,10)


filter_top_users <- function(df, top_percent) {
  # Compute total number of reports
  n <- nrow(df)
  
  # Compute threshold for top users
  top_threshold <- quantile(df$count, 1 - (top_percent / 100))
  
  # Filter top users
  filtered_df <- df %>% filter(count <= top_threshold)
  
  # Compute total number of reports after filtering
  n_filtered <- nrow(filtered_df)
  
  # Create histogram of number of reports
  ggplot(df, aes(count)) +
    geom_histogram(binwidth = 10, fill = "cornflowerblue", color = "black") +
    scale_x_continuous(limits = c(0, max(df$count, na.rm = TRUE))) +
    geom_vline(xintercept = top_threshold, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = top_threshold, y = 0, label = paste0("Top ", top_percent, "% users"), color = "red", hjust = -0.25, vjust = 1) +
    labs(title = "Distribution of Report Counts",
         x = "Number of Reports",
         y = "Frequency") +
    geom_text(aes(x = n_filtered, y = max(hist(df$count, breaks = seq(0, max(df$count), by = 1), plot = FALSE)$counts, na.rm = TRUE),
                  label = paste0("Total reports after filtering: ", n_filtered)),
              hjust = 0.25, vjust = 0.5, size = 3, color = "black") +
    geom_text(aes(x = n, y = max(hist(df$count, breaks = seq(0, max(df$count), by = 1), plot = FALSE)$counts, na.rm = TRUE),
                  label = paste0("Total reports before filtering: ", n)),
              hjust = 0.5, vjust = 0.25, size = 3, color = "black")
}

filter_top_users(Data_2019,10)
