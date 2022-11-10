# Cleaning for question 1

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


# Need to import it the way because of an error
john <- read.csv("Data/table_Full_Data_data.csv", fileEncoding = "UTF-16LE")