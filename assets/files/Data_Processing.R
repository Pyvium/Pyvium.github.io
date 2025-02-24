# ---- Initial Set up ----

# Load libraries
library(tidyverse)
library(scales)

# Create independent copies of the data sets using tibbles
rides_2019 <- as_tibble(Divvy_Trips_2019_Q1)
rides_2020 <- as_tibble(Divvy_Trips_2020_Q1)


# ---- Standardisation ----
# Standardise column names
rides_2019 <- rides_2019 %>% 
  rename(
    bike_id = bikeid,
    ride_length = tripduration,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    user_type = usertype,
    birth_year = birthyear
  )
rides_2020 <- rides_2020 %>% 
  rename(
    start_time = started_at,
    end_time = ended_at,
    user_type = member_casual
  )

# Convert start_time and end_time from characters to datetimes
rides_2019 <- rides_2019 %>%
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))
rides_2020 <- rides_2020 %>%
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))

# Add missing ride_length column to rides_2020 and format it for rides_2019
rides_2020 <- rides_2020 %>%
  mutate(ride_length = seconds_to_period(end_time - start_time))
rides_2019 <- rides_2019 %>%
  mutate(ride_length = seconds_to_period(ride_length))

# Remove rideable_type column from rides_2020 as there is only a single type,
# namely docked_bike
rides_2020["rideable_type"] <- NULL

# Create a week day column
rides_2019 <- rides_2019 %>% mutate(week_day = weekdays(start_time))
rides_2020 <- rides_2020 %>% mutate(week_day = weekdays(start_time))

# Reorder columns for more uniformity
rides_2019 <- rides_2019[, c("trip_id", "start_time", "end_time", "ride_length",
                             "week_day", "start_station_id",
                             "start_station_name", "end_station_id",
                             "end_station_name", "user_type", "bike_id",
                             "gender", "birth_year")]
rides_2020 <- rides_2020[, c("ride_id", "start_time", "end_time", "ride_length",
                             "week_day", "start_station_id",
                             "start_station_name", "end_station_id",
                             "end_station_name", "user_type", "start_lat",
                             "start_lng", "end_lat", "end_lng")]


# ---- Data Validation and Cleaning ----
## Verify the data type for each field
sapply(rides_2019, function(x) class(unique(x)))
sapply(rides_2020, function(x) class(unique(x)))

## Verify the data range and ensure data consistency
# Confirm that all trip and ride IDs are distinct
if (n_distinct(rides_2019$trip_id) != nrow(rides_2019))
  stop("Error: trip IDs are not unique")
if (n_distinct(rides_2020$ride_id) != nrow(rides_2020))
  stop("Error: ride IDs are not unique")

# Confirm that start time < end time and, if not, switch them, updating ride
# length and week day as well
if(any(rides_2019$start_time > rides_2019$end_time))
  stop("Error: start time is after end time")
rides_2020 <- rides_2020 %>%
  mutate(
    # We need temporary values in order to avoid overwriting the original value
    tmp_start_time = ifelse(start_time > end_time, end_time, start_time),
    tmp_end_time = ifelse(start_time > end_time, start_time, end_time),
    start_time = ymd_hms(as_datetime(tmp_start_time)),
    end_time = ymd_hms(as_datetime(tmp_end_time)),
    ride_length = seconds_to_period(end_time - start_time),
    week_day = weekdays(start_time)) %>%
  select(-tmp_start_time, - tmp_end_time)

# Confirm that only non-negative ride lengths exist
if(any(as.numeric(rides_2019$ride_length) < 0))
  stop("Error: negative ride lengths detected")
if(any(as.numeric(rides_2020$ride_length) < 0))
  stop("Error: negative ride lengths detected")

# Confirm that only actual week days exist in the data sets
actual_week_days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                     "Saturday", "Sunday")
if (length(setdiff(unique(rides_2019$week_day), actual_week_days)) > 0)
  stop("Error: invalid week days detected")
if (length(setdiff(unique(rides_2020$week_day), actual_week_days)) > 0)
  stop("Error: invalid week days detected")

# Standardise user types to members and casuals (i.e. casual riders)
rides_2019 <- rides_2019 %>%
  mutate(user_type = ifelse(user_type == "Subscriber", "member", "casual"))

## Verify data constraints
# Confirm that only real genders exist in rides_2019
actual_genders = c("Male", "Female", NA)
if(length(setdiff(unique(rides_2019$gender), actual_genders)) > 0)
  stop("Error: invalid genders detected")

# Confirm that only real years exist for birth years
if (!all(with(na.omit(rides_2019), 1900 <= birth_year & birth_year <= 2020)))
  stop("Error: invalid birth years detected")

# Confirm that latitude and longitude values are real values
if(!all(with(rides_2020, -90 <= start_lat & start_lat <= 90)))
  stop("Error: invalid latitude detected")
if(!all(with(na.omit(rides_2020), -90 <= end_lat & end_lat <= 90)))
  stop("Error: invalid latitude detected")
if(!all(with(rides_2020, -180 <= start_lng & start_lng <= 180)))
  stop("Error: invalid longitude detected")
if(!all(with(na.omit(rides_2020), -180 <= end_lng & end_lng <= 180)))
  stop("Error: invalid longitude detected")


# ---- Further Data Cleaning ----
# Determine the number of missing values per column
colSums(is.na(rides_2019))
colSums(is.na(rides_2020))

# Fill in missing values for one record (Note: for this record, the ride length
# is merely 12s, allowing us to infer that the bike was probably returned to
# the docking station it was taken from)
rides_2020[is.na(rides_2020$end_station_id),
           c("end_station_id", "end_station_name", "end_lat", "end_lng")] <-
  rides_2020[is.na(rides_2020$end_station_id),
             c("start_station_id", "start_station_name",
               "start_lat", "start_lng")]

# Remove extremely unlikely ride_length outliers
rides_2019 <- rides_2019 %>% filter(ride_length <= "24H")
rides_2020 <- rides_2020 %>% filter(ride_length <= "24H")

# Remove any records containing bikes that were removed for quality control
rides_2019 <- rides_2019 %>% filter(start_station_name != "HQ QR")
rides_2020 <- rides_2020 %>% filter(start_station_name != "HQ QR")

# Merge the two data sets
all_rides <- bind_rows(rides_2019, rides_2020)


# ---- Data Analysis and Visualisation ----
# Descriptive analysis on ride_length
rides_mins <- all_rides %>%
  mutate(ride_length = as.numeric(ride_length) / 60)
summary(rides_mins$ride_length)

# Compare members and casual users
aggregate(rides_mins$ride_length ~ rides_mins$user_type, FUN = mean)
aggregate(rides_mins$ride_length ~ rides_mins$user_type, FUN = median)
aggregate(rides_mins$ride_length ~ rides_mins$user_type, FUN = max)
aggregate(rides_mins$ride_length ~ rides_mins$user_type, FUN = min)

# Compare average ride length by week day for members vs casual users
all_rides$week_day <- ordered(all_rides$week_day, levels = c(
  "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ride_length_by_week_type <- aggregate(as.numeric(all_rides$ride_length) / 60
                                      ~ all_rides$user_type
          + all_rides$week_day, FUN = mean)

colnames(ride_length_by_week_type) <- c("user_type", "week_day", "ride_length")
ggplot(data = ride_length_by_week_type) +
  geom_bar(aes(x = week_day, y = ride_length, fill = user_type),
           stat = "identity", position = "dodge") +
  labs(x = "Day of the Week", y = "Ride length [min]", title = "Average Ride Length",
       fill = "User Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ridership data showing user type, week day, number of rides and average length
all_rides %>%
  group_by(user_type, week_day) %>%
  summarise(number_of_rides = n(), average_duration =
              mean(as.numeric(ride_length) / 60)) %>%
arrange(user_type, week_day)

# Visualise  number of rides by rider type
all_rides %>%
  group_by(user_type, week_day) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(user_type, week_day) %>%
  ggplot(aes(x = week_day, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides Per User Type",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "User Type") + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gender distribution
gender_dist <- all_rides %>%
  filter(!is.na(gender)) %>%
  group_by(user_type, gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(data = gender_dist, aes(x = gender, y = percentage, fill = user_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Percentage of Male/Female Users by User Type in 2019",
       x = "Gender",
       y = "Percentage",
       fill = "User Type")