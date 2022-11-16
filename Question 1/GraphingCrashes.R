# Bar plot of the crash data frame x being Percentage and y being Freq
bar_plot <- ggplot(crash_df, aes(x = reorder(Var1, -Percentage),
    y = Freq, fill = Percentage)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Aircraft Crashes in the US by State from 2000 to 2022",
    x = "State (Abbreviated)", y = "Number of Accidents")

bar_plot <- bar_plot + scale_fill_gradient(high = "Darkgreen", low = "Lightgreen", name = "Percentage")

# Create a duplicate of state_population called state_population_2
state_population_2 <- state_population

# Sort the state_population_2 data frame by the percentage column
state_population_2 <- state_population_2[order(state_population_2$percentage,
    decreasing = TRUE), ]

# Only include the top 10 states in the state_population_2 data frame
state_population_2 <- state_population_2[1:50, ]

# Create a bar plot of state_population$percentage
bar_plot_state <- ggplot(state_population_2,
    aes(x = reorder(abbreviation, -population), y = population / 1000000, fill = percentage)) +
    geom_bar(stat = "identity") +
    labs(title = "Population of the US by State 2020 Census",
    x = "State", y = "Population (Millions)")

bar_plot_state <- bar_plot_state + scale_fill_gradient(high = "Darkblue", low = "Lightblue", name = "Percentage")

# Change the font size of bar_plot to 20 and center the title
bar_plot <- bar_plot + theme(plot.title = element_text(size = 20, hjust = 0.5))

# Make the x axis labels bold
bar_plot <- bar_plot + theme(axis.text.x = element_text(face = "bold"))

# Change the font size of bar_plot_state to 20 and center the title
bar_plot_state <- bar_plot_state + theme(plot.title = element_text(size = 20, hjust = 0.5))

# Make the x axis labels bold
bar_plot_state <- bar_plot_state + theme(axis.text.x = element_text(face = "bold"))

# Change the font size of bar_plot_all to 20 and center the title
bar_plot_all <- bar_plot_all + theme(plot.title = element_text(size = 20, hjust = 0.5))

# Make the x axis labels bold
bar_plot_all <- bar_plot_all + theme(axis.text.x = element_text(face = "bold"))

# Change the font size of bar_plot_two to 20 and center the title
bar_plot_two <- bar_plot_two + theme(plot.title = element_text(size = 20, hjust = 0.5))

# Make the x axis labels bold
bar_plot_two <- bar_plot_two + theme(axis.text.x = element_text(face = "bold"))
