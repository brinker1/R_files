
# load the tidyverse and conflicted packages, then prioritize dplyr if conflicts
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# load the data for q1 of 2019 and 2020 into dataframes
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# view the data
head(q1_2019)
colnames(q1_2019)
colnames(q1_2020)
head(select(q1_2019, usertype))
head(select(q1_2020, member_casual))

# rename q1_2019 columns to be consistent with q1_2020 columns
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))
# double check that they match
colnames(q1_2019)
colnames(q1_2020)

# check data types for inconsistencies between the tables
str(q1_2019)
str(q1_2020)

# Convert ride_id and rideable_type to character so that they can stack correctly
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type)) 
# check
str(q1_2019)

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))

# consolidate labels for "member_casual" to be consistent
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# separate out date/time stuff so I can get ride time for everyone
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# create ride length for all rows from difference in times
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# drop bad rows
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# begin analysis: ride type vs. ride length
ggplot(data = all_trips_v2) + geom_point(mapping = aes(x = member_casual, y = ride_length))

# ride length for each member type
all_trips_casual <- all_trips_v2 %>% filter(member_casual == "casual")
all_trips_member <- all_trips_v2 %>% filter(member_casual == "member")
mean(all_trips_member$ride_length)
mean(all_trips_casual$ride_length)
median(all_trips_member$ride_length)
median(all_trips_casual$ride_length)

# ride length vs. day of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(data = all_trips_v2) + geom_bar(mapping = aes(x = day_of_week, fill = member_casual))

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
