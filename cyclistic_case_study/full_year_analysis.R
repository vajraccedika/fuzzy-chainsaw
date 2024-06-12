# We did an initial analysis on a single month of data to get an idea what the 
# data might contain and what kind of analysis we'd do on it. Now we'll do an
# entire year of data to create our final analysis.

# Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("janitor")
install.packages("scales")
install.packages("tidygeocoder")
install.packages("leaflet")
install.packages("stringr")
install.packages("htmlwidgets")
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(janitor)
library(scales)
library(tidygeocoder)
library(leaflet)
library(stringr)
library(htmlwidgets)

# import a year's worth of ride data
Apr_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202304-divvy-tripdata.csv")
May_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202305-divvy-tripdata.csv")
Jun_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202306-divvy-tripdata.csv")
Jul_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202307-divvy-tripdata.csv")
Aug_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202308-divvy-tripdata.csv")
Sep_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202309-divvy-tripdata.csv")
Oct_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202310-divvy-tripdata.csv")
Nov_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202311-divvy-tripdata.csv")
Dec_2023 <- read_csv("~/ryan/Google DA/case_study/ride_data/202312-divvy-tripdata.csv")
Jan_2024 <- read_csv("~/ryan/Google DA/case_study/ride_data/202401-divvy-tripdata.csv")
Feb_2024 <- read_csv("~/ryan/Google DA/case_study/ride_data/202402-divvy-tripdata.csv")
Mar_2024 <- read_csv("~/ryan/Google DA/case_study/ride_data/202403-divvy-tripdata.csv")

# Make sure all our colnames and data types match for all of our datasets:
# str() and attr(df, "spec") are the two options - str is a bit more verbose
attr(Apr_2023, "spec")
attr(May_2023, "spec")
attr(Jun_2023, "spec")
attr(Jul_2023, "spec")
attr(Aug_2023, "spec")
attr(Sep_2023, "spec")
attr(Oct_2023, "spec")
attr(Nov_2023, "spec")
attr(Dec_2023, "spec")
attr(Jan_2024, "spec")
attr(Feb_2024, "spec")
attr(Mar_2024, "spec")

# All our datasets have the same column names and data types for each column
# All of the column names are snake_case, which we like, so we don't need to
# rename anything, so we can merge everything directly, using bind_rows()

merged_df <- bind_rows(Apr_2023, May_2023, Jun_2023, Jul_2023, Aug_2023, Sep_2023, Oct_2023, Nov_2023, Dec_2023, Jan_2024, Feb_2024, Mar_2024)

# remove any empty entries
merged_df <- remove_empty(merged_df, which = c())

# create a col for duration:
merged_df <- mutate(merged_df, duration = difftime(ended_at, started_at, units="mins"))

# create a col for hour departed:
merged_df <- merged_df %>%
  mutate(hour = lubridate::hour(started_at))

# create a col for day of the week:
merged_df <- merged_df %>%
  mutate(day = wday(started_at, week_start = 1))

#create a col for month:
merged_df <- merged_df %>%
  mutate(month = month(started_at, label=TRUE))

# Create estimated costs, really only useful for single_rides, since we don't
# know how many rides each user of a daily pass took, but still interesting
# Define the function to calculate costs
calculate_costs <- function(df) {
  df %>%
    mutate(
      single_ride_cost = ifelse(member_casual == "casual",
                                ifelse(rideable_type == "classic_bike",
                                       1 + 0.18 * duration,
                                       1 + 0.44 * duration),
                                NA),
      day_pass_cost = ifelse(member_casual == "casual",
                             ifelse(rideable_type == "classic_bike",
                                    ifelse(duration > 180, 18 + 0.18 * (duration - 180), 18),
                                    18 + 0.44 * duration),
                             NA),
      member_cost = ifelse(member_casual == "member",
                           ifelse(rideable_type == "classic_bike",
                                  ifelse(duration > 45, 0.18 * (duration - 45), 0),
                                  0.18 * duration),
                           NA)
    )
}

# Apply the function to the data frame
merged_df <- calculate_costs(merged_df)

# Now remove any rows where duration is <= 0:
clean_df <- merged_df %>%
  filter(duration>=0)

# After cleaning, we looked at the df, clean_df:
View(clean_df)
# We discovered there's a third kind of entry int he ride data: docked_bike.
# After some searching online, we discovered docked_bike is an older
# designation for classic_bike. We need to convert these to classic_bike
clean_df$rideable_type[clean_df$rideable_type == 'docked_bike'] <- 'classic_bike'

# Now we need to see how many rides don't have an end_station or end_lat/end_lng:
sum(is.na(clean_df$end_station_id))

# 928497 rides have no end_station_id!
sum(is.na(clean_df$end_lat))
sum(is.na(clean_df$end_lng))
# There are only 7566 records with no end_lat/end_lng

# decide whether to remove only records with no ending lat/lng or with no 
# end_station:
pct_no_end_station = 928497/5748713
print(pct_no_end_station)
# roughly 16% of rides have no end station
pct_no_end_latlong = 7566/5748713
print(pct_no_end_latlong)
# only 0.13% of rides have no latlong, so first we'll remove those
clean_df <- clean_df[!is.na(clean_df$end_lat),]
# In the initial analysis we removed any rides less than one minute, we should 
# probably do that here as well - rides less than 1 minute are likely to be
# a problem with the bike or some other mistake
clean_df <- clean_df %>%
  filter(duration >= 1)

# Ok let's start to get some visualization
# Starting with rides by member/casual user per month, side by side bar

# create a df to plot from
rides_by_month <- clean_df %>%
  group_by(month, member_casual) %>%
  summarise(total_rides = n()) %>%
  ungroup()

# Create a side-by-side bar graph
ggplot(rides_by_month, aes(x = month, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Total Number of Rides by User by Month",
       x = "Month",
       y = "Total Rides",
       fill = "User Type") +
  theme_minimal() +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))

# Total rides every month by members were always higher than casual rides,
# especially pronounces during the winter months, which suggests members
# are using the bikes for regular transportation

# Let's see a percentage of rides by bike type for the year by user type:
rides_by_member_casual <- clean_df %>%
  group_by(member_casual) %>%
  summarise(total_rides = n()) %>%
  ungroup()

ggplot(rides_by_member_casual, aes(x = member_casual, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::comma(total_rides)), vjust = -0.3) +
  labs(title = "Total Number of Rides by User Type",
       
       x = "User Type",
       y = "Total Rides") +
  theme_minimal() +
  scale_fill_discrete(name = "User Type") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))

# Let's get the number of rides per hour by day of the week, by member/casual:
# Summarize the total number of rides by hour, member_casual, and day of the 
# week
rides_by_hour_day <- clean_df %>%
  group_by(hour, member_casual, day) %>%
  summarise(total_rides = n()) %>%
  ungroup()

# NOTE: we tried to label AM/PM but it was too busy for the facet_wrap

ggplot(rides_by_hour_day, aes(x = hour, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Total Number of Rides by Hour and User Type",
       subtitle = "Day of the Week",
       x = "Hour of the Day",
       y = "Total Rides",
       fill = "User Type") +
  theme_minimal() +
  facet_wrap(~ day, scales = "free_x", labeller = labeller(day = c(`1` = "Monday", `2` = "Tuesday", `3` = "Wednesday", `4` = "Thursday", `5` = "Friday", `6` = "Saturday", `7` = "Sunday"))) +
  scale_x_continuous(breaks = seq(0, 23, by = 3), 
                     labels = c("0", "3", "6", "9", "12", "15", "18", "21")) +
theme(axis.text.x = element_text(size = 6)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))

# For our presentation, let's get a comparison between weekday and weekend
# between casual and members:

# Calculate average rides per hour for weekdays (days 1-5) by user type
weekday_avg_rides <- rides_by_hour_day %>%
  filter(day %in% 1:5) %>%
  group_by(hour, member_casual) %>%
  summarise(average_rides = mean(total_rides, na.rm = TRUE)) %>%
  ungroup()

# Calculate average rides per hour for weekends (days 6-7) by user type
weekend_avg_rides <- rides_by_hour_day %>%
  filter(day %in% 6:7) %>%
  group_by(hour, member_casual) %>%
  summarise(average_rides = mean(total_rides, na.rm = TRUE)) %>%
  ungroup()

# Create plot for weekdays
ggplot(weekday_avg_rides, aes(x = hour, y = average_rides, color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Avg Weekday Rides/Hr by User",
       x = "Hour of the Day",
       y = "Average Rides",
       color = "User Type") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, by = 3), 
                     labels = c("12 AM", "3 AM", "6 AM", "9 AM", "12 PM", "3 PM", "6 PM", "9 PM")) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))

# Create plot for weekends
ggplot(weekend_avg_rides, aes(x = hour, y = average_rides, color = member_casual, group = member_casual)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Avg Weekend Rides/Hr by User",
         x = "Hour of the Day",
         y = "Average Rides",
         color = "User Type") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 23, by = 3), 
                       labels = c("12 AM", "3 AM", "6 AM", "9 AM", "12 PM", "3 PM", "6 PM", "9 PM")) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))

# Now that we see the total number of rides by year (member is more) let's get 
# the average duration of rides for the whole year by member/casual:

rides_by_hour <- clean_df %>%
  group_by(hour, member_casual) %>%
  summarise(total_rides = n()) %>%
  ungroup()

ggplot(rides_by_hour, aes(x = hour, y = total_rides, color = member_casual)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Total Rides by Hour by User Type",
       x = "Hour",
       y = "Rides",
       color = "User Type") +
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = 0:23)

# We can see from this that total rides for members peaks in the morning and
# afternoon, probably using the bikes for regular commute

# Now let's look at ride duration by member_group:

# Summarize the average duration of rides by member_casual
average_duration_by_member_casual <- clean_df %>%
  group_by(member_casual) %>%
  summarise(average_duration = mean(duration, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_duration_by_member_casual, aes(x = member_casual, y = average_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f", average_duration)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = "Average Duration of Rides by User Type",
       x = "User Type",
       y = "Average Duration (minutes)",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10))

# Now let's get avg duration of rides by bike and user type

average_duration_by_member_casual_bike <- clean_df %>%
  group_by(member_casual, rideable_type) %>%
  summarise(average_duration = mean(duration, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_duration_by_member_casual_bike, aes(x = rideable_type, y = average_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f", average_duration)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(x = "Type of Bike", 
       y = "Average Ride Duration (minutes)", 
       fill = "User Type",
       title = "Average Ride Duration by Bike and User") +
  theme_minimal()

# Here we can see that members use both bike types for approximately the same
# duration, while casual riders use classic bikes for much longer

# Ok, let's compare member cost for average duration rides to single ride cost:
# Melt the dataframe to long format for ggplot2
cost_comparison <- clean_df %>%
  select(rideable_type, member_cost, single_ride_cost, day_pass_cost) %>%
  gather(key = "cost_type", value = "cost", -rideable_type)

# Summarize the average costs by rideable_type and cost_type
average_costs <- cost_comparison %>%
  group_by(rideable_type, cost_type) %>%
  summarise(avg_cost = mean(cost, na.rm = TRUE)) %>%
  filter(cost_type %in% c("member_cost", "single_ride_cost")) %>%
  ungroup()

# Create a side-by-side bar graph
ggplot(average_costs, aes(x = rideable_type, y = avg_cost, fill = cost_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("$%.2f", avg_cost)), 
            position = position_dodge(0.9), vjust = -0.5, size = 3) +
  labs(title = "Average Costs by Bike Type and Payment Plan",
       x = "Bike",
       y = "Average Cost",
       fill = "Payment Plan") +
  theme_minimal() +
  scale_fill_manual(values = c( "#00BFC4", "#F8766D"))
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10))

# We can see that, as predicted, the member cost for rides is much cheaper per
# ride than the single ride cost, though since we don't have rider id data,
# we can't say how many single rides a particular user has taken.

# Now let's see which stations are the most popular to start rides from
# Summarize the total number of rides by start station and member_casual
top_start_stations <- clean_df %>%
  filter(!is.na(start_station_name)) %>%
  group_by(member_casual, start_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(member_casual, desc(total_rides)) %>%
  top_n(10, total_rides)

clean_top_stations <- top_start_stations

# Tidygeocoder isn't picking up lat/lon of some of these stations - if we had
# a station_info table we wouldn't need to do this: replace two station names
# with locations that are very close to the actual bike stations:
clean_top_stations$start_station_name <- str_replace(clean_top_stations$start_station_name, "DuSable Lake Shore Dr & Monroe St", "Monroe Harbor Tender Service")

# Lake Shore and North Blvd don't actually intersect so although the station
# name is that, we changed to a reasonably close place
clean_top_stations$start_station_name <- str_replace(clean_top_stations$start_station_name, "DuSable Lake Shore Dr & North Blvd", "1550 North Lake Shore Drive")

# Let's try to map the popular start stations by member and casual user:
station_coordinates <- clean_top_stations %>%
  geocode(start_station_name, method = 'osm', lat = latitude, long = longitude)

station_coordinates <- station_coordinates %>%
  mutate(color = case_when(str_detect(member_casual, "member") ~  "#00BFC4",
                           str_detect(member_casual, "casual") ~  "#F8766D",
                           TRUE ~ "#00BFC4"))

station_coordinates <- station_coordinates %>%
  group_by(member_casual) %>%
  arrange(member_casual, desc(total_rides)) %>%
  mutate(rank = dense_rank(desc(total_rides))) %>%
  ungroup()

label_colors = c( "#00BFC4", "#F8766D")
label_text = c("Member", "Casual")

# Make a map
map <- leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap tiles as the base layer
  addCircleMarkers(data = station_coordinates, # Add circle markers for stations
                   lat = ~latitude, lng = ~longitude, # Latitude and longitude coordinates
                   radius = ~sqrt(total_rides/1000) * 2, # Adjust radius based on the number of rides
                   color = "black",
                   fillColor = ~station_coordinates$color,
                   fillOpacity = 0.6,
                   label = ~rank,
                   stroke = TRUE,
                   popup = ~start_station_name) %>%
  addLegend(
            position = "bottomright", # Add legend for marker sizes
            colors = label_colors,
            labels = label_text,
            title = "Popular Start Stations by User") %>%
  addProviderTiles("Stamen.TonerLabels") # Add additional tile layer for labels

# Now that we have a map: let's save it
saveWidget(map, file = "/Users/annli/ryan/Google DA/case_study/cyclistic_case_study/visualizations/map1.html")

# Finally, we think that rides that start/stop at the same station may be 
# sightseeing or pleasure rides - user picks up a bike, rides around, and 
# returns to the start location, maybe near a hotel

# Filter data to drop rides where start or end station is NA
filtered_df <- clean_df %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name))

# Filter data for rides that start and end at the same station
same_station_rides <- filtered_df %>%
  filter(start_station_name == end_station_name)

# Summarize the number of rides by user type
ride_summary <- same_station_rides %>%
  group_by(member_casual) %>%
  summarise(total_rides = n())

# Create a side-by-side bar graph
ggplot(ride_summary, aes(x = member_casual, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Same Station Start and End Rides",
       x = "User Type",
       y = "Total Rides",
       fill = "User Type") +
  theme_minimal() +
  geom_text(aes(label = scales::comma(total_rides)), vjust = -0.3) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))