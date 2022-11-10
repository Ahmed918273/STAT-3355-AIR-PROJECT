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

# Create a bar plot of the using the crash_df_merged data frame and have the
# x-axis be the abbreviation and the y-axis be the percentage of accidents and
# percentage of population

bar_plot_all <- ggplot(crash_df_merged, 
  aes(x = reorder(abbreviation, -Percentage_Accidents), 
      y = Percentage_Accidents & crash_df_merged$Percentage_Popluation, 
      fill = Percentage_Popluation)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Percentage of Accidents and Percentage of Population in the Mainland United States from 2000 to 2022 (19060 Accidents)",
       x = "State Abbreviation",
       y = "Percentage")








# Only include the top 10 rows based on Freq
crash_df <- crash_df[1:50, ]

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
