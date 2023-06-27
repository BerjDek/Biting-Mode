#Average Number of reports 2019:2.32,  2021: 2.77,  2022: 3.16



freq_vis1 <- function(data) {
  # Count the number of reports sent by each user
  user_report_counts <- data %>%
    group_by(user_id) %>%
    summarize(report_count = n()) %>%
    ungroup()
  
  # Add a column that indicates the report frequency level for each user
  user_report_counts <- user_report_counts %>%
    mutate(report_frequency = case_when(
      report_count == 1 ~ "1 report",
      report_count > 1 & report_count <= 5 ~ "2-5 reports",
      report_count > 5 & report_count <= 10 ~ "6-10 reports",
      report_count > 10 & report_count <= 20 ~ "11-20 reports",
      report_count > 20 ~ "More than 20 reports"
    ))
  
  # Calculate the percentage of reports sent by users in each report frequency level
  freq_counts <- user_report_counts %>%
    group_by(report_frequency) %>%
    summarize(freq_count = n()) %>%
    ungroup()
  
  freq_percents <- freq_counts %>%
    mutate(freq_percent = freq_count / sum(freq_count) * 100)
  
  # Create a table of report frequency level and percentage of reports sent
  freq_table <- freq_percents %>%
    select(report_frequency, freq_percent) %>%
    rename(`Report Frequency Level` = report_frequency, `Percentage of Reports Sent` = freq_percent)
  
  return(freq_table)
}

report_freq_2019 <- freq_vis2(Data_2019)
report_freq_2019$Year <- "2019"
report_freq_2022 <- freq_vis2(Data_2022)
report_freq_2022$Year <- "2022"
report_freq_2021 <- freq_vis2(Data_2021)
report_freq_2021$Year <- "2021"


report_freq <- bind_rows(report_freq_2019, report_freq_2021, report_freq_2022) %>% 
  rename_with(~ gsub(" ","_", .x), contains(" ")) %>% 
  rename(Percentage = Percentage_of_Reports_Sent, Group = Report_Frequency_Level)  

report_freq$Percentage <- round(report_freq$Percentage, 0)
report_freq$Group <- factor(report_freq$Group, levels = c("More than 20 reports", "11-20 reports", "6-10 reports", "2-5 reports", "1 report"))




ggplot(report_freq, aes(x = Year, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#51A3A3", "#EE6C4D", "#3D5A80", "#F9C22E", "#6B2737")) +
  geom_label(aes(label = paste0(Percentage, "%")), 
             position = position_stack(vjust = 0.5), 
             show.legend = FALSE, 
             size = 3, 
             color = "white")




freq_vis2 <- function(data) {
  # Count the number of reports sent by each user
  user_report_counts <- data %>%
    group_by(user_id) %>%
    summarize(report_count = n()) %>%
    ungroup()
  
  # Add a column that indicates the report frequency level for each user
  user_report_counts <- user_report_counts %>%
    mutate(report_frequency = case_when(
      report_count == 1 ~ "1 report",
      report_count > 1 & report_count <= 3 ~ "2-3 reports",
      report_count > 3 & report_count <= 7 ~ "3-7 reports",
      report_count > 7 ~ "More than 7 reports"
    ))
  
  # Calculate the percentage of reports sent by users in each report frequency level
  freq_counts <- user_report_counts %>%
    group_by(report_frequency) %>%
    summarize(freq_count = n()) %>%
    ungroup()
  
  freq_percents <- freq_counts %>%
    mutate(freq_percent = freq_count / sum(freq_count) * 100)
  
  # Create a table of report frequency level and percentage of reports sent
  freq_table <- freq_percents %>%
    select(report_frequency, freq_percent) %>%
    rename(`Report Frequency Level` = report_frequency, `Percentage of Reports Sent` = freq_percent)
  
  return(freq_table)
}

report_freq_2019 <- freq_vis2(Data_2019)
report_freq_2019$Year <- "2019"
report_freq_2022 <- freq_vis2(Data_2022)
report_freq_2022$Year <- "2022"
report_freq_2021 <- freq_vis2(Data_2021)
report_freq_2021$Year <- "2021"


report_freq <- bind_rows(report_freq_2019, report_freq_2021, report_freq_2022) %>% 
  rename_with(~ gsub(" ","_", .x), contains(" ")) %>% 
  rename(Percentage = Percentage_of_Reports_Sent, Group = Report_Frequency_Level)  

report_freq$Percentage <- round(report_freq$Percentage, 0)
report_freq$Group <- factor(report_freq$Group, levels = c("More than 7 reports", "3-7 reports", "2-3 reports", "1 report"))




ggplot(report_freq, aes(x = Year, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#51A3A3", "#EE6C4D", "#3D5A80",  "#6B2737")) +
  geom_label(aes(label = paste0(Percentage, "%")), 
             position = position_stack(vjust = 0.5), 
             show.legend = FALSE, 
             size = 3, 
             color = "white")