# Importing library
library(tidyverse)
library(maps)

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

us_cities2 <- arrange(us.cities, pop)
tail(us_cities2)


# Plot all states with ggplot
main_cities <- filter(us.cities, long > -130)
main_cities <- filter(main_cities, pop > 250000)


# Creating base plot
g <- ggplot()

# Creating a map with state population
g <- g + geom_polygon(data = merged_states,
  aes(x = long, y = lat,
  group = group, fill = population / 1000000),
  color = "black", size = 0.2) +
  scale_fill_continuous(name = "State Population",
  low = "lightgreen",
  high = "darkgreen",
  limits = c(0, 40),
  breaks = c(5, 10, 15, 20, 25, 30, 35),
  na.value = "grey50") +
  labs(title = "Aircraft Accidents in the Mainland United States from 2000 to 2022 (19060 Accidents)",
       x = "Longitude", y = "Latitude")

# Center align the title and change its size to 20
g <- g + theme(plot.title = element_text(hjust = 0.5, size = 15))

# Creating dots on map
g <- g + geom_point(data = accident_long_lat,
  aes(x = longitude, y = latitude),
  color = "red", size = 1,
  alpha = 0.1)

map_with_cities <- g + geom_point(data = main_cities,
                                  aes(x = long, y = lat, size = pop / 1000000), size = 10,
                                  color = "blue",  alpha = 0.5) +
  scale_size(name = "City Population (Millions)")

  # Zoom into Texas
map_with_cities_texas <- map_with_cities + coord_fixed(xlim = c(-107, -93),
    ylim = c(25, 37))