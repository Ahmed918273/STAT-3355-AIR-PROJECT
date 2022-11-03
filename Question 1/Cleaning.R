# Cleaning for question 1

# import library
library(tidyverse)
library(readxl)
library(stringr)

# import data set
EventsPost2008 <- read_excel("2008_Present_Data/events.xlsx")

# sub-setting data
LongLatEventsPost2008 <-EventsPost2008 %>% select(ev_id, ev_date, longitude, latitude)

# Removing NA values
LongLatEventsPost2008 <- na.omit(LongLatEventsPost2008)

# Creating a program that converts co-ordinates from degrees minutes seconds to
# decimal co-ordinates
for (i in 1:nrow(LongLatEventsPost2008)) {
  
  # Longitude
  
  # Finding the degree co-ordinate
  degreesLong <- as.numeric(substring(LongLatEventsPost2008$longitude[i], nchar(LongLatEventsPost2008$longitude[i]) - 7, nchar(LongLatEventsPost2008$longitude[i]) - 5))
  # Finding the minute co-ordinate
  minutesLong <- as.numeric(substring(LongLatEventsPost2008$longitude[i], nchar(LongLatEventsPost2008$longitude[i]) - 4, nchar(LongLatEventsPost2008$longitude[i]) - 3))
  # Finding the second co-ordinate
  secondsLong <- as.numeric(substring(LongLatEventsPost2008$longitude[i], nchar(LongLatEventsPost2008$longitude[i]) - 2, nchar(LongLatEventsPost2008$longitude[i]) - 1))
  
  # Finding whether it is W or E
  eastOrwestLong <- substring(LongLatEventsPost2008$longitude[i], nchar(LongLatEventsPost2008$longitude[i]) - 0, nchar(LongLatEventsPost2008$longitude[i]) - 0)
  
  # If the co-ordinate is on the W then it is negative otherwise it is positive
  if (eastOrwestLong == "W"){
    LongLatEventsPost2008$longitude[i] <- round(-1 * (degreesLong + (minutesLong/60) + (secondsLong/3600)), digits = 3)
  }
  else {
    LongLatEventsPost2008$longitude[i] <- round(degreesLong + (minutesLong/60) + (secondsLong/3600), digits = 3)
  }
  
  # Latitude
  
  # Finding the degree co-ordinate
  degreesLat <- as.numeric(substring(LongLatEventsPost2008$latitude[i], nchar(LongLatEventsPost2008$latitude[i]) - 6, nchar(LongLatEventsPost2008$latitude[i]) - 5))
  # Finding the minute co-ordinate
  minutesLat <- as.numeric(substring(LongLatEventsPost2008$latitude[i], nchar(LongLatEventsPost2008$latitude[i]) - 4, nchar(LongLatEventsPost2008$latitude[i]) - 3))
  # Finding the second co-ordinate
  secondsLat <- as.numeric(substring(LongLatEventsPost2008$latitude[i], nchar(LongLatEventsPost2008$latitude[i]) - 2, nchar(LongLatEventsPost2008$latitude[i]) - 1))
  
  # Finding whether it is N or S
  eastOrwestLat <- substring(LongLatEventsPost2008$latitude[i], nchar(LongLatEventsPost2008$latitude[i]) - 0, nchar(LongLatEventsPost2008$latitude[i]) - 0)
  
  # If the co-ordinate is on the S then it is negative otherwise it is positive
  if (eastOrwestLong == "S") {
    
    LongLatEventsPost2008$latitude[i] <- round(degreesLat + (minutesLat/60) + (secondsLat/3600), digits = 3)  
    }
  else {
    
    LongLatEventsPost2008$latitude[i] <- round(degreesLat + (minutesLat/60) + (secondsLat/3600), digits = 3)  
    }
}


rm(list = c('degreesLat', 'degreesLong', 'eastOrwestLat', 'eastOrwestLong', 'i',
            'minutesLat', 'minutesLong', 'secondsLat', 'secondsLong'))


# Duplicating data set
AccidentLongLat <- LongLatEventsPost2008
AccidentLongLat[] <- NA

# Filtering LongLatEventsPost2008 to only be mainland USA

for (i in 1:nrow(LongLatEventsPost2008)) {
  
  longitude <- as.numeric(LongLatEventsPost2008$longitude[i])
  latitude <- as.numeric(LongLatEventsPost2008$latitude[i])
  
  if (-65 > longitude && longitude > -130) {
    
    if (50 > latitude && latitude > 25) {
      
      AccidentLongLat[i,]  <- LongLatEventsPost2008[i,]
    }
  }
}

AccidentLongLat <- na.omit(AccidentLongLat)

AccidentLongLat$longitude <- as.numeric(AccidentLongLat$longitude)
AccidentLongLat$latitude <- as.numeric(AccidentLongLat$latitude)

rm(list = c('i', 'longitude', 'latitude'))


