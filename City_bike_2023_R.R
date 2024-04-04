# Install and load packages for cleaning

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("dplyr")
install.packages("ggplot2")


library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(dplyr)
library(ggplot2)

# Set directory

getwd()
setwd("C:/Users/Joshua Kitt/Documents/Total_2023_CSV")

# Import csv files into R

t1 <- read.csv("202301-divvy-tripdata.csv")
t2 <- read.csv("202302-divvy-tripdata.csv")
t3 <- read.csv("202303-divvy-tripdata.csv")
t4 <- read.csv("202304-divvy-tripdata.csv")
t5 <- read.csv("202305-divvy-tripdata.csv")
t6 <- read.csv("202306-divvy-tripdata.csv")
t7 <- read.csv("202307-divvy-tripdata.csv")
t8 <- read.csv("202308-divvy-tripdata.csv")
t9 <- read.csv("202309-divvy-tripdata.csv")
t10 <- read.csv("202310-divvy-tripdata.csv")
t11 <- read.csv("202311-divvy-tripdata.csv")
t12 <- read.csv("202312-divvy-tripdata.csv")

# Compare all column have same no. of columns and data type
compare_df_cols(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)

# Combine all files 
master <- rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)

# view dataset
head(master)
glimpse(master)
dim(master)

# Convert character datatype to datetime format

clean_data <- master
clean_data$started_at <- as.POSIXct(clean_data$started_at, format = "%Y-%m-%d %H:%S:%S")

clean_data$ended_at <- as.POSIXct(clean_data$ended_at, format = "%Y-%m-%d %H:%S:%S")

# View the datetype is converted 

str(clean_data)

# Create columns for trip duration, hour, month and day 

clean_data <- clean_data %>% 
  mutate(trip_duration = difftime(clean_data$ended_at,clean_data$started_at, "min")) %>% 
  mutate(trip_duration_min = as.numeric(trip_duration)/60) %>% 
  mutate(hour = format(started_at, format = "%H")) %>% 
  mutate(month = month(started_at, label = TRUE)) %>%  
  mutate(day = weekdays(started_at)) %>% 
  mutate(date = as_date(started_at))
  
# Filter out rows less than 0

clean_data <- clean_data %>% 
  filter(trip_duration > 0) %>%
# Delete rows with empty cells 
   drop_na()

# Check for Unique values for rideable type and membership
unique(clean_data$rideable_type)
unique(clean_data$member_casual)

# Check for duplicate entries in ride_id
sum(duplicated(clean_data$ride_id))

view(clean_data)
# write.csv(clean_data, "clean_data.csv") to export to csv file
# Analyses & Visualization

citybikes <- clean_data


# Analyses & Visualization

# Total Usage - Casual vs Members

total_usage_df <- citybikes %>% 
  group_by(member_casual) %>% 
  summarize(n=n()) %>% 
  mutate(percentage = n*100/sum(n)) %>% 
  print()


ggplot(citybikes, aes (x= member_casual, fill=member_casual)) + 
  geom_bar() + 
  labs( title = "Number of Casual Riders Vs Members", x = "User Type", y = "Number of Members") + scale_fill_discrete( name = "Member Type", labels = c("Casual", "Member")) 

# Average trip duration - Casual vs Members

avg_trip_df <- citybikes %>% 
  group_by(member_casual) %>% 
  summarise(avg_trip = mean(trip_duration_min)) %>% 
  print()

ggplot(avg_trip_df, aes (x= member_casual, y= avg_trip, fill = member_casual)) + 
  geom_col() +
  labs( title = "Average Trip Duration", x = "Member Type", y = "Average ride duration") + scale_fill_discrete( name = "Member Type", labels = c("Casual", "Member")) 

# Monthly Usage - Casual vs Members

monthly_usage_df <- citybikes %>% 
  group_by(month, member_casual) %>% 
  summarise(n=n()) %>% 
  arrange(month) %>% 
  print

ggplot(citybikes, aes (x= month, fill=member_casual)) + 
  geom_bar(position = "dodge") + 
  labs( title = "Usuage by month", x = "Months", y = "Number of rides",) + 
  scale_fill_discrete( name = "Member Type", labels = c("Casual", "Member")) 

#Usage by bike type 

bike_usage_df <- citybikes %>% 
  group_by (rideable_type, member_casual) %>% 
  summarise(n=n()) %>% 
  mutate(percentage = n*100/sum(n)) %>% 
  print()

ggplot(citybikes, aes(x=rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Usage by bike type", x = "Bike type", y = "Number of Users")+
  scale_fill_discrete( name = "Member Type", labels = c("Casual", "Member")) 


# Average trip duration by month & members
citybikes %>%
  group_by(month, member_casual) %>%
  summarise(mean_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = mean_trip_duration, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Average Duration", x = "Months", y = "Average Trip Duration (s)") + 
  scale_fill_discrete( name = "Member Type", labels = c("Casual", "Member"))

# Usage by hour

usage_hour_df <- citybikes %>% 
  group_by(hour, member_casual) %>% 
  summarise(n=n()) %>% 
  print()

ggplot(citybikes, aes( x=hour , fill = member_casual)) + 
  geom_bar(position = "dodge") +
  labs( title = "Usage by hour", x= "Start hour", y = "Number of rides") +
  scale_fill_discrete(name = "Member type", labels = c("Casual", "Member"))


# Usage by hour (line graph)
ggplot(citybikes, aes(x = hour, color = member_casual, group = member_casual)) + 
  geom_line(stat = "count") +
  labs(title = "Usage by hour", x = "Start hour", y = "Number of rides") +
  scale_color_discrete(name = "Member type", labels = c("Casual", "Member"))


# Usage by Day

usage_hour_df <- citybikes %>% 
  group_by(day, member_casual) %>% 
  summarise(n=n()) %>% 
  print()

ggplot(citybikes, aes( x=day , fill = member_casual)) + 
  geom_bar(position = "dodge") +
  labs( title = "Usage by hour", x= "Start hour", y = "Number of rides") +
  scale_fill_discrete(name = "Member type", labels = c("Casual", "Member"))


# Most frequent start stations for each member type
top_start_station <- citybikes %>% 
  count(start_station_name, member_casual) %>%
  group_by(member_casual) %>% 
  top_n(5,n)

print(top_start_station)

# Most frequent end stations for each member type
top_end_station <- clean_data %>% 
  count(end_station_name, member_casual) %>%
  group_by(member_casual) %>%
  top_n(5, n)

print(top_end_station)
