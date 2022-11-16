# Importing library
library(tidyverse)
library(maps)

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
