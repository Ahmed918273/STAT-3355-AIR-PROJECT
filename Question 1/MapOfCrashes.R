# Importing library
library(tidyverse)
library(maps)

# Getting map data
main_states <- map_data("state")

# State population data
state_population <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE) # nolint

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
  labs(title = "Aircraft Accidents in the Mainland United 
  States from 1974 to 2022 (19079 Accidents)")

# Creating dots on map
g <- g + geom_point(data = accident_long_lat,
  aes(x = longitude, y = latitude),
  color = "red", size = 0.5,
  alpha = 0.5)

g <- g + geom_point(data = main_cities,
  aes(x = long, y = lat, size = pop / 1000000),
  color = "blue",  alpha = 1) +
  scale_size(name = "City Population (Millions)")