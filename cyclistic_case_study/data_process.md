BUSINESS QUESTION: How do annual members and casual riders use cyclistic differently and how can those insights drive an awareness campaign to convert casual riders to annual members?
DATA SOURCES: Origin of data: Coursera/divvy open license dataset
Time frame of data: 03/2023-03/2024 (1 year of data) - we chose this data because it's still relevant and long enough to give us a sense of trends over an entire year

Information about Divvy:


20204.05.01 - downloaded data locally
    - unzipped raw files
    - uploaded to google CS
    - uploaded 2024.03 dataset to bigquery and performed exploratory analysis
    Notes:
    SCHEMA
        ride_id
        rideable_type
        started_at (time)
        ended_at (time)
        start_station_name
        start_station_id
        end_station_name
        end_station_id
        start_lat
        start_lon
        end_lat
        end_lon
        member_casual       string, member/casual
Analysis questions:
    1. Where do casuals start/end their rides?
    2. where do members start/end rides?
    3. what's avg durtation of ride for casuals v members
    4. what's overall numbers/pct of members/casuals, plotted over time?
    5. Does share of member/casual rides change over time of year?
Initial analysis for 1 month:
table with cols:
    avg_ride_duration   duration_max    duration_min    sum_start_station   sum_end_station, group by member_casual
table 2 with cols:
    start_time_hour     end_time_hour   member_casual (group by hour)

Uploaded data for 2024.03 to R Studio
Notes:
    - some rides have a negative duration, need to filter these out of any duration based results
    - some rides don't have station_start_name or station_end_name, need to pull them with a join from the stations table
    - some rides don't have station_id either - remove these for location analysis

Next up:
TIME analyses:
    - avg/min/max duration, by casual/member
    - n.rides, hourly, by casual/member
LOCATION analyses:
    - count, station, by casual/member
    - lat lon cluster plot, start, end by casual/member
COST analysis:
    - convert expenditure total to dollars for casual/members

2024.05.06:
    - continued analysis in R
    - created a duration df to study ride duration
    - checked average duration by membership
    - new data limitation realized: we don't know if casual is day or onetime
    - new data limitation realized: we don't know if member is divvy or lyft pink

2024.05.07
    - got monthly data grouped by member type and bike type
    - working with DURATION_DF (duration >= 1 minute), we found:
    total rides = 295099
    member rides = 214482
    casual rides = 80617
    casual pct = 27.32
    member pct = 72.68
    NEXT: break down by hour of start/end

2024.05.21:
    Note that we don't have data for individual RIDERS, just RIDES. This means we can't extablish behavior about specific riders, just make inferences about the behavior of rider types.
    This is a data limitation.
    - We need to further break down the ride percentages by bike type - what type of bikes are casual/members using?
    casual        classic_bike  27.551219 mins   
    casual        electric_bike 13.674263 mins   
    member        classic_bike  12.539366 mins   
    member        electric_bike  9.806928 mins 
    This suggests that casual riders take longer rides on both types of bike, which could mean they end up being more expensive
    - We have a breakdown of rides per hour:
    rides_per_hour_df
    There is a significant difference in rides started by hour between members and casual users:
    We can see that member rides spike between 0600 and 0900, and then again between 1500 and 1900,
    compared to casual riders which shows a gradual increase from 0500 to 1700 and then a decrease to 0000
    Possible explanation: members use the service to commute to and from work
2024.05.22:
    Today's questions:
    - create a graph of pct of total rides by member/casual classic/ebike
    ANSWER: behavior is almost the same: 50/50 for member and very slightly more ebike than classic for casual
    - create a graph to show avg duration of rides by member/casual classic/ebike
    ANSWER: casual riders ride much longer on classic bikes and slightly longer on ebikes than members
    - create a cosst graph based on:
    MEMBER: $143.90 per year
    classic bike:       45 min free, then $0.18/min
    scooter:            Free unlock, $0.29/min
    ebike:              Free unlock, $0.18/min
    DAY PASS: $18.10 + 
    classic:            3 hour free, then $0.18/min
    scooter:            Free unlock, $0.44/min
    ebike:              Free unlock, $0.44/min
    SINGLE RIDE:
    classic:            $1 unlock, then $0.18/min
    scooter:            $1 unlock, then $0.44/min
    ebike:              $1 unlock, then $0.44/min

    ANSWER:
    For mean duration of ride, the day pass cost for classic_bike rides is 18.10 and electric_bike cost is 23.91.
    This does NOT take into account DAY PASS users who may have taken multiple rides in the same day.
    SINGLE USE mean duration cost was 7.96 for classic_bike and 6.81 for electric_bike.

    - How many repeat trips are made by casual/members? Define a repeat trip as a trip that starts in the same hour, ends in the same hour, starts at the same station, and ends at the same station
    - Create a viz for repeat trips (if possible) - can do geocoding but not sure if can plot a "journey"
2024.05.23:
    - Figure out how to define and count "repeat trips"
    - Change our bikes/users/pct of total rides from a bar to a pie graph
    - Get ride counts grouped by casual/member for days of the week
    - Create a viz for days of the week vs weekend, group/member
    - Create a viz for hourly use by day
    - Figure out where most rides start/end
    
**** INITIAL ANALYSIS ENDED, YEARLONG ANALYSIS BEGINS ****
    
    - Get all the data for the past year and clean it/merge it
    Uploaded data for 2024/02
    Zipped data for 2023/04 through 2024/01
    Now we have a year of data
    Now we'll discard our objects and start over, cleaning and creating a single dataset that contains 1 year worth of data
UPDATE: decided to DL R studio desktop as Rstudio cloud kept crashing when trying to load a whole year's data
Organized data into local folder. Only keeping one year of data from Mar2024 through Apr2023. Now the only data that exists locally is a zipped copy of the entiere dataset, consisting of zipped files, and the 12 months we will use for analysis.
UPDATE: we have created a single df with a year's worth of data. 
NEXT TASKS:
X   - add start_hour column 
X   - add day column (day of the week)
X  - add ride duration column
X   - add month column
X   - add estimated cost for day_pass and single_ride, where member = casual, and annual_cost for members
2024.05.24:
    UPDATE: we discovered some instances where rideable_type = docked_bike, we need to convert these to classic_bike
X   - convert all instances of docked_bike to classic_bike
X   - remove instances where there's no end data - lost or stolen bikes
X   - remove rides with duration < 1 minute
    - geomap 10 most popular start and end locations, grouped by member/casual
X   - visualize n rides by hour, by day of the week, by member casual (line)
X   - visualize n rides by day, side by side bar
    NOTE: if we could get station data, we could try to apply station_id and station_name to rides that only have lat/long info
    - visualize trips that start and end at same station where ride time > 5 minutes
    - check pct of same start/end trips vs different end/start trips, by member/casual
2024.05.27:
    NOTE: we're going to get the popular locations/rides but only going to go with station_id's for sake of consistency, since we don't have the station info we need to approximate the lat/longs of the unidentified rides
X   - Get average duration by user for whole year and all bike types
X   - Get average duration of ride by user and bike type


TO DO BETTER:
- Include station_data: a table with the lat/long of each station, so that analysis can attempt to map rides to stations
- Include user_id data as long as it's properly anonymized: this way we can target specific users who might save money as a member or who otherwise use the bikes as members do

2024.05.30:
WHERE WE ARE:
TODO: Day of the week line graph vs weekend line graph
- Analysis is complete, findings as follows:

- Members take 1.7x more rides than casual users
- Member rides peak during the morning and afternoon, suggesting members use bikes for regular commute
- Members take more rides than casual users M-F, and about the same number of rides on weekends
- Members use ebikes and classic bikes for roughly the same amount of time, while casuals use classic bikes much longer than    ebikes
- Casual users ride longer than members, suggesting riding for pleasure/sightseeing/farther destinations
- Rides for both members and casual users dip during the winter months, but much more for casual riders than members, reinforcing regular commute as a motivation for members
- Casual users take almost twice as many rides that start/stop from the same station as members

RECOMMENDATIONS:
The course assignment was simply to analyze the difference in behavior between casual and members and then provide recommendations to Lily Moreno, director of marketing.
WHAT WE DID:
Rather than using the exact analysis steps provided by Google DA course, we performed a lot of our own analysis.
