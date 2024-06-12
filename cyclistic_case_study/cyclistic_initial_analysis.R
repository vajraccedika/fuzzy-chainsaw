install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("tidygeocoder")
install.packages("leaflet")
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(tidygeocoder)
library(leaflet)
March_2024 <- read_csv("ride_data/202403-divvy-tripdata.csv")
Feb_2024 <- read_csv("ride_data/2020402-divvy-tripdata.csv")
X202403_divvy_tripdata <- read_csv("ride_data/202403-divvy-tripdata.csv")
df <- X202403_divvy_tripdata
df <- mutate(df, ride_duration = difftime(ended_at, started_at, units="mins"))
# create new df for time analysis
time_df <- df
#create a df for duration analysis, where duration is at least 1 minute
# all following analysis takes place where ride duration is at least ONE MINUTE
duration_df <- time_df %>% filter(ride_duration >= 1)

#arrange descending by ride_duration:
duration_df <- arrange(duration_df, -ride_duration)
#now let's see what the difference in avg ride duration is
duration_df %>% group_by(member_casual) %>% drop_na() %>% summarize(avg_ride_duration = mean(ride_duration)) %>% mutate(avg_ride_cost)
# then break it down by bike type:
duration_df %>% group_by(member_casual, rideable_type) %>% drop_na() %>% summarize(avg_ride_duration = mean(ride_duration))
# total number of rides, by casual and member, where 
table(duration_df$member_casual)
# number of total rides in the duration table
duration_tot_rides = 80617 + 214482
#pct of rides that are casual
duration_pct_casual = 80617/duration_tot_rides * 100
# pct of rides that are membership rides
duration_pct_member = 214482/duration_tot_rides * 100
# add a column for hour started based on start_time
duration_df <- duration_df %>%
  mutate(hour = lubridate::hour(started_at))
# now get a summary of rides started each hour
rides_per_hour_df <- duration_df %>%
  group_by(hour) %>%
  summarise(rides_per_hour = n())
# join rides per hour df back into duration_df on the hour column
duration_df <- duration_df %>%
  left_join(rides_per_hour_df, by = "hour")
# let's look at rides per hour specifically:
View(rides_per_hour)
# now let's see if there's any difference between the rides per hour between members and casual riders:
# Calculate the total number of rides per hour grouped by 'hour' and 'member_casual'
rides_per_hour_df <- duration_df %>%
  group_by(hour, member_casual) %>%
  summarise(rides_per_hour = n(), .groups = 'drop')
# now let's visualize it:
# Create a side-by-side bar graph
ggplot(rides_per_hour_df, aes(x = factor(hour), y = rides_per_hour, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Hour of the Day", y = "Number of Rides", fill = "User Type",
       title = "Number of Rides per Hour by User Type") +
  scale_x_discrete(breaks = 0:23) +  # Ensure all hours are shown on the x-axis
  theme_minimal()
# ok the side by side shows the difference in total tides well, and we get the 
# beginning of a trend shape, but let's use line graphs to see the overall trend
# comparing members and casual riders:
ggplot(rides_per_hour_df, aes(x = hour, y = rides_per_hour, color = member_casual)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  # Optional: to highlight the data points
  labs(x = "Hour of the Day", y = "Number of Rides", color = "User Type",
       title = "Number of Rides per Hour by User Type") +
  scale_x_continuous(breaks = 0:23) +  # Ensure all hours are shown on the x-axis
  theme_minimal()
#next, we want to compare percentage of total rides by member/casual across bike types:
# Calculate the total number of rides for each combination of 'rideable_type' and 'member_casual'
rides_summary_df <- duration_df %>%
  group_by(rideable_type, member_casual) %>%
  summarise(total_rides = n(), .groups = 'drop')

# Calculate the total number of rides
total_rides_all <- sum(rides_summary_df$total_rides)

# Calculate the percentage of total rides for each combination
rides_summary_df <- rides_summary_df %>%
  mutate(percentage_rides = (total_rides / total_rides_all) * 100)

# Create a side-by-side bar graph
ggplot(rides_summary_df, aes(x = rideable_type, y = percentage_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", percentage_rides)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Type of Bike", y = "Percentage of Total Rides", fill = "User Type",
       title = "Percentage of Total Rides by Bike and User") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
# now we want to visualize the difference in average duration of rides by bike type and user type:
# Calculate the average ride duration for each combination of 'rideable_type' and 'member_casual'
average_duration_df <- duration_df %>%
  group_by(rideable_type, member_casual) %>%
  summarise(average_duration = mean(ride_duration, na.rm = TRUE), .groups = 'drop')

# Create a side-by-side bar graph
ggplot(average_duration_df, aes(x = rideable_type, y = average_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f", average_duration)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(x = "Type of Bike", y = "Average Ride Duration (minutes)", fill = "User Type",
       title = "Average Ride Duration by Bike and User") +
  theme_minimal()
# Given the following price data:
#   classic bike:       45 min free, then $0.18/min
# scooter:            Free unlock, $0.29/min
# ebike:              Free unlock, $0.18/min
# DAY PASS: $18.10 + 
#   classic:            3 hour free, then $0.18/min
# scooter:            Free unlock, $0.44/min
# ebike:              Free unlock, $0.44/min
# SINGLE RIDE:
#   classic:            $1 unlock, then $0.18/min
# scooter:            $1 unlock, then $0.44/min
# ebike:              $1 unlock, then $0.44/min
# we will estimate the costs to casual riders for average duration rides:
# Filter for casual riders and calculate the average ride duration for each bike type
average_duration_casual <- duration_df %>%
  filter(member_casual == "casual") %>%
  group_by(rideable_type) %>%
  summarise(average_duration_minutes = mean(as.numeric(ride_duration, units = "mins")), .groups = 'drop')

# Define the cost calculation functions
calculate_day_pass_cost <- function(rideable_type, duration) {
  if (rideable_type == "classic_bike") {
    if (duration <= 180) {
      return(18.10)
    } else {
      return(18.10 + 0.18 * (duration - 180))
    }
  } else if (rideable_type == "electric_bike") {
    return(18.10 + 0.44 * duration)
  }
}

calculate_single_ride_cost <- function(rideable_type, duration) {
  if (rideable_type == "classic_bike") {
    return(1 + 0.18 * duration)
  } else if (rideable_type == "electric_bike") {
    return(1 + 0.44 * duration)
  }
}

# Calculate the cost for day_pass and single_ride
average_duration_casual <- average_duration_casual %>%
  mutate(day_pass_cost = mapply(calculate_day_pass_cost, rideable_type, average_duration_minutes),
         single_ride_cost = mapply(calculate_single_ride_cost, rideable_type, average_duration_minutes))

# Reshape the data to a long format for plotting
average_duration_long <- average_duration_casual %>%
  pivot_longer(cols = c(day_pass_cost, single_ride_cost),
               names_to = "ride_type",
               values_to = "cost")

# Create a side-by-side bar graph with cost labels
ggplot(average_duration_long, aes(x = rideable_type, y = cost, fill = ride_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("$%.2f", cost)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Rideable Type", y = "Cost ($)", fill = "Ride Type",
       title = "Costs for Single Ride and Day Pass by Bike Type",
       subtitle = "Classic: 38.64 mins, Electric: 13.21 mins") +
  theme_minimal() +
  scale_fill_manual(values = c("day_pass_cost" = "blue", "single_ride_cost" = "red"),
                    labels = c("Day Pass", "Single Ride"))