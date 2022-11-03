# Importing library
library(tidyverse)
library(maps)

# Getting map data
MainStates <- map_data("state")

# State population data
StatePopulation <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE)

# Creating new data 
MergedStates <- inner_join(MainStates, StatePopulation, by = "region")

us.cities2 = arrange(us.cities, pop)
tail(us.cities2)


# Plot all states with ggplot
MainCities <- filter(us.cities, long >= -130)


# Creating base plot
g <- ggplot()

# Creating a map with state population
g <- g + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = population/1000000),
                       color = "black", size = 0.2) + 
  
  scale_fill_continuous(name="State Population", low = "lightblue", 
                        high = "darkblue",limits = c(0,40), breaks=c(5,10,15,20,25,30,35), 
                        na.value = "grey50") +
  
  labs(title = "Aircraft Accidents in the Mainland United States from 2008 to 2022 (8862 Accidents)")


# Creating dots on map
g <- g + geom_point(data = MainCities, aes(x=long, y=lat, size = pop/1000000), 
                    color = "gold", alpha = 0.5) + scale_size(name = "City Population")

g <- g + geom_point(data = AccidentLongLat, aes(x = longitude, y = latitude), 
                    color = "red", size = 0.5)
g

# Zoom into a particular region of the plot
a  <- g + coord_cartesian(xlim=c(-80, -65), ylim = c(38, 46))
a







