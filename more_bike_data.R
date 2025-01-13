
# import packages
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# load more recent data and view the dataframe
nov_2024_data <- read_csv("202411-divvy-tripdata.csv")
head(nov_2024_data)
colnames(nov_2024_data)
summary(nov_2024_data)

# remove some columns
nov_2024_data <- nov_2024_data %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng))
colnames(nov_2024_data)

# create ride_time and day of week
nov_2024_data$day_of_week <- format(as.Date(nov_2024_data$started_at), "%A")

# look at rides by day of the week
nov_2024_data$day_of_week <- ordered(nov_2024_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(data = nov_2024_data) + geom_bar(mapping = aes(x = day_of_week, fill = rideable_type))
ggplot(data = nov_2024_data) + geom_bar(mapping = aes(x = day_of_week, fill = member_casual))
