# import library
library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)

# import data set
events_post_2008 <- read_excel("2008_Present_Data/events.xlsx")

eventspre2008 <- read_excel("Before_2008_Data/event.xlsx")

# sub-setting data
long_lat_events_post_2008 <- events_post_2008 %>% select(ev_id, ev_date,
longitude, latitude, ev_state)

long_lat_events_pre2008 <- eventspre2008 %>% select(ev_id, ev_date,
  longitude, latitude, ev_state)

# Removing NA values
long_lat_events_post_2008 <- na.omit(long_lat_events_post_2008)
long_lat_events_pre2008 <- na.omit(long_lat_events_pre2008)

# Merging data sets
long_lat_events_all <- rbind(long_lat_events_pre2008,
  long_lat_events_post_2008)

# Creating a program that converts co-ordinates from
# degrees minutes seconds to decimal co-ordinates

for (i in 1:nrow(long_lat_events_all)) { # nolint
  # Longitude
  # Finding the degree co-ordinate
  degrees_long <- as.numeric(substring(long_lat_events_all$longitude[i],
    nchar(long_lat_events_all$longitude[i]) - 7,
    nchar(long_lat_events_all$longitude[i]) - 5))
  # Finding the minute co-ordinate
  minutes_long <- as.numeric(substring(long_lat_events_all$longitude[i],
    nchar(long_lat_events_all$longitude[i]) - 4,
    nchar(long_lat_events_all$longitude[i]) - 3))
  # Finding the second co-ordinate
  seconds_long <- as.numeric(substring(long_lat_events_all$longitude[i],
    nchar(long_lat_events_all$longitude[i]) - 2,
    nchar(long_lat_events_all$longitude[i]) - 1))
  # Finding whether it is W or E
  east_or_west_long <- substring(long_lat_events_all$longitude[i],
    nchar(long_lat_events_all$longitude[i]) - 0,
    nchar(long_lat_events_all$longitude[i]) - 0)
  # If the co-ordinate is on the W then it is negative otherwise it is positive
  if (east_or_west_long == "W") {
    long_lat_events_all$longitude[i] <- round(-1 * (degrees_long +
    (minutes_long / 60) + (seconds_long / 3600)), digits = 3)
  } else {
    long_lat_events_all$longitude[i] <- round(degrees_long +
    (minutes_long / 60) + (seconds_long / 3600), digits = 3)
  }
  # Latitude
  # Finding the degree co-ordinate
  degrees_lat <- as.numeric(substring(long_lat_events_all$latitude[i],
    nchar(long_lat_events_all$latitude[i]) - 6,
    nchar(long_lat_events_all$latitude[i]) - 5))
  # Finding the minute co-ordinate
  minutes_lat <- as.numeric(substring(long_lat_events_all$latitude[i],
    nchar(long_lat_events_all$latitude[i]) - 4,
    nchar(long_lat_events_all$latitude[i]) - 3))
  # Finding the second co-ordinate
  seconds_lat <- as.numeric(substring(long_lat_events_all$latitude[i],
    nchar(long_lat_events_all$latitude[i]) - 2,
    nchar(long_lat_events_all$latitude[i]) - 1))
  # Finding whether it is N or S
  east_or_west_lat <- substring(long_lat_events_all$latitude[i],
    nchar(long_lat_events_all$latitude[i]) - 0,
    nchar(long_lat_events_all$latitude[i]) - 0)
  # If the co-ordinate is on the S then it is negative otherwise it is positive
  if (east_or_west_long == "S") {
    #
    long_lat_events_all$latitude[i] <- round(degrees_lat +
    (minutes_lat / 60) + (seconds_lat / 3600), digits = 3)
    } else {
    #
    long_lat_events_all$latitude[i] <- round(degrees_lat +
    (minutes_lat / 60) + (seconds_lat / 3600), digits = 3)
    }
}

# Clearing useless variables from environment
rm(list = c("degrees_lat", "degrees_long",
"east_or_west_lat", "east_or_west_long", "i",
"minutes_lat", "minutes_long", "seconds_lat", "seconds_long"))

# Removing NA values (data that cannot be propperly converted)
long_lat_events_all <- na.omit(long_lat_events_all)

# Duplicating data set
accident_long_lat <- long_lat_events_all
accident_long_lat[] <- NA

# Filtering long_lat_events_all to only be mainland USA

for (i in 1:nrow(long_lat_events_all)) { # nolint
  #
  longitude <- as.numeric(long_lat_events_all$longitude[i])
  latitude <- as.numeric(long_lat_events_all$latitude[i])
  #
  if (-65 > longitude && longitude > -130) {
    #
    if (50 > latitude && latitude > 25) {
      #
      accident_long_lat[i, ]  <- long_lat_events_all[i, ]
    }
  }
}

accident_long_lat <- na.omit(accident_long_lat)

# Making long and lat numeric
accident_long_lat$longitude <- as.numeric(accident_long_lat$longitude)
accident_long_lat$latitude <- as.numeric(accident_long_lat$latitude)

# Removing rows before the year 2000
accident_long_lat <- accident_long_lat[-c(1:19), ]

rm(list = c("i", "longitude", "latitude"))


# Getting map data
main_states <- map_data("state")

# State population data
state_population <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE) # nolint

# Create a new coloumn in the state_population data frame called "percentage"
state_population$percentage <- (state_population$population /
sum(state_population$population)) * 100

# Create a new coloumn in the state_population data frame called "abbreviation"
state_population$abbreviation <- tolower(state_population$region)

# Create a list of all abbreviations of the states capitalised
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE",
"FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
"MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
"NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
"VT", "VA", "WA", "WV", "WI", "WY")

# Sort the state_population data frame by the region column
state_population <- state_population[order(state_population$region), ]

# Add the state_abbreviations list to the state_population data
#frame in the abbreviation column
state_population$abbreviation <- state_abbreviations

# Creating new data
merged_states <- inner_join(main_states, state_population, by = "region")

# Table the long_lat_events_all data frame
crash_table <- table(long_lat_events_all$ev_state)

# Turn the table into a data frame
crash_df <- as.data.frame(crash_table)

# Reorder the crash_df descending
crash_df <- crash_df[order(crash_df$Freq, decreasing = TRUE), ]

# Create a new column in crash_df called "Percentage" and input the percentage
crash_df$Percentage <- round(crash_df$Freq / sum(crash_df$Freq) * 100, 2)

# Merge the crash_df with the state_population data frame
crash_df_merged <- merge(state_population, crash_df)

# Only keep rows in which Var1 and region are the same
crash_df_merged <- crash_df_merged[crash_df_merged$Var1 == crash_df_merged$abbreviation, ]

# Rename the Freq column to "Accidents"
colnames(crash_df_merged)[2] <- "Accidents"

# Rename the percentage column to "Percentage_Population"
colnames(crash_df_merged)[4] <- "Percentage_Popluation"

# Rename the percentage column to "Percentage_Population"
colnames(crash_df_merged)[8] <- "Percentage_Accidents"

# Manipulating data to make a bar plot

# Create a new data frame called "crash_df_merged2"
# from the crash_df_merged data frame
crash_df_merged2 <- crash_df_merged

# Remove the region column
crash_df_merged2 <- crash_df_merged2[, -1]

# Remove the elect_votes column
crash_df_merged2 <- crash_df_merged2[, -2]

# Make abbreviations the first column in the crash_df_merged2 data frame
crash_df_merged2 <- crash_df_merged2[, c("abbreviation", "Accidents",
 "Percentage_Popluation", "Var1", "Freq", "Percentage_Accidents")]

# Add a column to the end named "Percentage_Type" and have it be filled with "Y"
crash_df_merged2$Percentage_Type1 <- "Accidents"

# Add a column to the end named "Percentage_Type" and have it be filled with "Y"
crash_df_merged2$Percentage_Type2 <- "Population"

# Create a new data frame called "crash_df_merged3" in which columns
# 4, 5, 6, and 7 are taken from the crash_df_merged2 data frame
crash_df_merged3 <- crash_df_merged2[, c(4, 5, 6, 7)]

# Create a new data frame called "crash_df_merged4" in which columns
# 1, 2, 3, and 8 are taken from the crash_df_merged2 data frame
crash_df_merged4 <- crash_df_merged2[, c(1, 2, 3, 8)]

# Rename the columns in crash_df_merged3 to "State", "Number", "Percentage", and "Percentage_Type"
colnames(crash_df_merged3) <- c("State", "Number", "Percentage", "Percentage_Type")

# Only include the top 10 states in the crash_df_merged3 data frame
crash_df_merged3 <- crash_df_merged3[1:10, ]

# Rename the columns in crash_df_merged4 to "State", "Number", "Percentage", and "Percentage_Type"
colnames(crash_df_merged4) <- c("State", "Number", "Percentage", "Percentage_Type")

# Only include the top 10 states in the crash_df_merged4 data frame
crash_df_merged4 <- crash_df_merged4[1:10, ]

# Combine the crash_df_merged3 and crash_df_merged4 data frames
crash_df_merged5 <- rbind(crash_df_merged3, crash_df_merged4)

# create a bar plot using the crash_df_merged5 data frame
bar_plot_two <- ggplot(crash_df_merged5, 
  aes(x = reorder(State, -Percentage), 
  y = Percentage, fill = Percentage_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Accidents and Population by State",
  x = "State", y = "Percentage", fill = "Percentage Type")

# Only include the top 10 rows based on Freq
crash_df_merged <- crash_df_merged[1:10, ]

# Only include the top 10 rows based on Freq
crash_df <- crash_df[1:50, ]