## This is my capstone for Cyclistic bike share 

# Cyclistic Q1 2023 Data Analysis
This repository contains the analysis of Cyclistic bike-share data from Q1 2023. This analysis focuses on understanding the differences in usage between **casual riders** and **annual members** and providing insights to help convert casual riders into annual members.

## Analysis Process

### 1. Data Preparation

load and clean the data for three months (January, February, March 2023):

```r
# Load required libraries
library(ggplot2)
library(dplyr)

# I first loaded the datasets using the path on my cpu 
df_jan <- read.csv("C:/Users/KK/Desktop/Ccyclyst data/2023Q1/202301-divvy-tripdata.csv")
df_feb <- read.csv("C:/Users/KK/Desktop/Ccyclyst data/2023Q1/202302-divvy-tripdata.csv")
df_mar <- read.csv("C:/Users/KK/Desktop/Ccyclyst data/2023Q1/202303-divvy-tripdata.csv")

#  Next I Combined the datasets into one data frame
df_combined <- rbind(df_jan, df_feb, df_mar)

# I Convert 'started_at' and 'ended_at' columns to datetime format
df_combined$started_at <- as.POSIXct(df_combined$started_at, format="%Y-%m-%d %H:%M:%S")
df_combined$ended_at <- as.POSIXct(df_combined$ended_at, format="%Y-%m-%d %H:%M:%S")

## Next I transformed the data by adding new columns to best represent different types of data

# I Created new columns for ride start time, day of the week, and month
df_combined$ride_start <- format(df_combined$started_at, "%H:%M:%S")
df_combined$ride_end <- format(df_combined$ended_at, "%H:%M:%S")
df_combined$Month_start <- format(df_combined$started_at, "%b")
df_combined$Dow_start <- format(df_combined$started_at, "%a")

# Calculate trip duration in minutes
df_combined$trip_duration <- as.numeric(difftime(df_combined$ended_at, df_combined$started_at, units = "mins"))

# Rename 'member_casual' values to 'members' and 'casual'
df_combined$member_casual <- ifelse(df_combined$member_casual == "member", "members", "casual")


## Following that I then proceeded to create visuals to enhance my findings and make them more understandable
## All of these visuals will demonstrate how casual and annual members use the bikes differently


## Here is a Pie chart that shows Q1 riders the percentage of which are annual members and casual
### 77.3%: Annual Members and 22.7%: Causal Riders in Q1 of 2023

## Here is how I went about creating the pie chart

rider_counts <- table(df_combined$member_casual)
pie_colors <- c('#1f77b4', '#ff7f0e')
pie(rider_counts, labels = paste0(names(rider_counts), ": ", round(100 * rider_counts / sum(rider_counts), 1), "%"),
    main = "Proportion of Casual Riders and Annual Members in Q1", col=pie_colors)




## Ride count by day of the week

ggplot(df_combined, aes(x=Dow_start, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Ride Counts by Day of the Week (Casual vs Members)", x="Day of the Week", y="Number of Rides") +
  scale_fill_manual(values=c('#1f77b4', '#ff7f0e')) +
  theme_minimal()



## Time of day usage

df_combined$hour <- format(df_combined$started_at, "%H")
usage_by_hour <- df_combined %>%
  group_by(member_casual, hour) %>%
  summarise(ride_count = n())

ggplot(usage_by_hour, aes(x=as.numeric(hour), y=ride_count, color=member_casual)) +
  geom_line(size=1.2) +
  labs(title="Bike Usage by Time of Day (Casual vs Members)", x="Hour of Day", y="Number of Rides") +
  scale_color_manual(values=c('#1f77b4', '#ff7f0e')) +
  theme_minimal()


## Monthly Rides taken by casual and members

ggplot(df_combined, aes(x=Month_start, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Monthly Ride Counts (Casual vs Members)", 
       x="Month", 
       y="Number of Rides") +
  scale_fill_manual(values=color_palette) +
  theme_minimal()


  Conclusion + Reccomondations

In conclusion: Casual riders had an increase in March and also an increase during the weekend, Members primarily use bikes during commute hours on weekdays, and Casual riders tend to take longer trips

Focus on seasonal and weekend Usage: 
  # Offering membership deals tied to seasonal and weekends could encourage casual riders to become annual riders 

Focus on Commuter-Friendly Offers: 
# Emphasizing commuter benefits (Ex: special deals for work-related commutes could boost membership) 

Tailored Marketing for Ride Duration: 
  # Offering ride packages or membership plans that cater to leisure or longer rides, while appealing to members’ shorter, more practical ride needs





