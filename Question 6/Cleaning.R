library(tidyverse)
library(readxl)

aircraft <- read_xlsx("2008_Present_Data/aircraft.xlsx")
events <- read_excel("2008_Present_Data/events.xlsx")

# Remove all row that are NA in acft_year
aircraft <- aircraft[!is.na(aircraft$acft_year), ]

# Only include ev_id, acft_make, acft_model, and acft_year in aircraft
aircraft <- aircraft[, c("ev_id", "acft_make", "acft_model",
    "acft_year", "damage")]

# Only ev_id, ev_date, ev_year, ev_month, ev_time, ev_city,
# in events
events <- events[, c("ev_id", "ev_date", "ev_year", "ev_month",
                     "ev_time", "ev_city")]

# Combine the aircraft and events data frames if they have the same ev_id
aircraft_events_post_2008 <- merge(aircraft, events, by = "ev_id")

# Create a new column that is the accident_age which is the difference
# between the year of the accident and the year the aircraft was made
aircraft_events_post_2008$accident_age <- aircraft_events_post_2008$ev_year -
    aircraft_events_post_2008$acft_year

# Remove aircraft_age that are more than 200 years old
aircraft_events_post_2008 <- aircraft_events_post_2008[
    aircraft_events_post_2008$accident_age < 200, ]

# Capitalize all words in acft_make
aircraft_events_post_2008$acft_make <-
toupper(aircraft_events_post_2008$acft_make)

# Find the most common aircraft makes in the data frame
common_aircraft_make <- table(aircraft_events_post_2008$acft_make)

# Sort the table from most common to least common
common_aircraft_make <- sort(common_aircraft_make, decreasing = TRUE)

# Create a data frame with the most common aircraft makes
common_aircraft_make <- data.frame(common_aircraft_make)

# Only include the top 10 aircraft makes
common_aircraft_make <- common_aircraft_make[1:10, ]

# Only include "CESSNA", "PIPER", "BEECH", "BELL", "BOEING"
# in the acrf_make column in the aircraft_events_post_2008 data frame
aircraft_events_post_2008 <- aircraft_events_post_2008[
    aircraft_events_post_2008$acft_make %in% c("CESSNA", "PIPER",
    "BEECH", "BOEING"), ]

# Remove ev_year smaller than 2013
aircraft_events_post_2008 <- aircraft_events_post_2008[
    aircraft_events_post_2008$ev_year >= 2013, ]

# Create a new data frame but only include CESSNA aircraft
cessna_accidents <- aircraft_events_post_2008[
    aircraft_events_post_2008$acft_make == "CESSNA", ]

# Remove NA values from cessna_accidents
cessna_accidents <- na.omit(cessna_accidents)

# Remove UNK from damage column
cessna_accidents <- cessna_accidents[
    cessna_accidents$damage != "UNK", ]

# Create a new data frame called cessna_accidents_substantial
# and only include SUBST in the damage column
cessna_accidents_subs <- cessna_accidents[
    cessna_accidents$damage == "SUBS", ]