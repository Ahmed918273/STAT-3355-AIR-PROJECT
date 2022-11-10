library(gapminder)

library(ggplot2)
library(gganimate)

# Create a scatter plot of ev_date as the x-axis and accident_age as the y-axis
scatter_plot_all <- ggplot(aircraft_events_post_2008,
    aes(x = ev_date, y = accident_age, color = acft_make)) +
    geom_point() +
    labs(
    title = "Scatter Plot of Aircrafe Accidents by Aircraft Make (The Four Most Common Aircraft Make by Number of Accidents)", # nolint
    x = "Date of Accident",
    y = "Age of Aircraft",
    color = "Aircraft Make") +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~acft_make)

# Make the scatter plot points jittered
scatter_plot_all <- scatter_plot_all + geom_jitter()

# Create a line of best fit for each aircraft make
scatter_plot_all <- scatter_plot_all + geom_smooth(
    method = "loess", color = "black")






# Create a scatter plot of ev_date as the x-axis and accident_age as the y-axis
# using the cessna_accidents data frame

scatter_plot_cessna <- ggplot(cessna_accidents,
                           aes(x = ev_date, y = accident_age, color = damage)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Cessna Aircraft Accidents",
    x = "Date of Accident",
    y = "Age of Aircraft",
    color = "Damage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~damage)

# Make the scatter plot points jittered
scatter_plot_cessna <- scatter_plot_cessna + geom_jitter()

# Create a line of best fit for each aircraft make
scatter_plot_cessna <- scatter_plot_cessna + geom_smooth(
  method = "loess", color = "black")

# Set the y limit to be 100
scatter_plot_cessna <- scatter_plot_cessna + ylim(0, 80)





# Create a scatter plot of ev_date as the x-axis and accident_age as the y-axis
# using the cessna_accidents_subs data frame
scatter_plot_cessna_subs <- ggplot(cessna_accidents_subs,
    aes(x = ev_date, y = accident_age, color = damage)) +
    geom_point() +
    labs(
    title = "Scatter Plot of Cessna Aircraft Accidents with Substantial Damage from 2012 to 2022 (2601 Accidents 89.5 Percent of Total)", # nolint
    x = "Date of Accident",
    y = "Age of Aircraft",
    color = "Damage") +
    theme(plot.title = element_text(hjust = 0.5))

# Make the scatter plot points jittered
scatter_plot_cessna_subs <- scatter_plot_cessna_subs + geom_jitter()

# Create a loess line of best fit
scatter_plot_cessna_subs <- scatter_plot_cessna_subs + geom_smooth(
    method = "loess", color = "Black", size = 2)

# Create an animated scatter plot of ev_month as the x-axis and
# accident_age as the y-axis using the cessna_accidents_subs data frame
# and have the transition be ev_year

animated_cessna_subs <- ggplot(gapminder, 
  aes(x = ev_month, y = accident_age, color = damage)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  labs(
  title = "Animated Scatter Plot of Cessna Aircraft Accidents with Substantial Damage from 2012 to 2022 (2601 Accidents 89.5 Percent of Total)", # nolint
  x = "Date of Accident",
  y = "Age of Aircraft",
  color = "Damage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  transition_time(ev_year)
