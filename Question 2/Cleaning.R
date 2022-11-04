# Cleaning for question 2
#download pre2008 files
acft_pre_df <- read.csv("./aircraft_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
events_pre_df <- read.csv("./events_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
engines_pre_df <- read.csv("./engines_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
#download post2008 files
acft_post_df <- read.csv("./aircraft_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
events_post_df <- read.csv("./events_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
engines_post_df <- read.csv("./engines_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))

#remove two columns to match the number of columns for row-bining 
events_post_df = subset(events_post_df, select = -c(dec_latitude, dec_longitude) )

#combine pre and post files
acft_df <- rbind(acft_pre_df, acft_post_df)
events_df <- rbind(events_pre_df, events_post_df)
engines_df <- rbind(engines_pre_df, engines_post_df)

#merge acft file and events file
acft_events_merged_df = merge(acft_df, events_df, by = "ev_id")
#merge acft file and engines file
acft_engines_merged_df = merge(acft_df, engines_df, by = "ev_id")

#for styling some of the graphs
style <- geom_text(stat = "count", aes(label = ..count..), 
                   position = position_dodge(width=0.9), 
                   vjust = -.25, size = 2)

#Total number of accident by category
acft_df %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = acft_category)) + 
  geom_bar() + 
  geom_text(stat ='count', aes(label = ..count..), vjust=-1) +
  ylim(0,81000) +
  labs(title = "Number of Accident by Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#type by aircraft category (accident or incident)
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = ev_type)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "Number of Accident by Event Type",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Event Type"))

#Phase of operations where events occurred by aircraft category (off or on airport) 
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  filter(!is.na(ev_nr_apt_loc)) %>%
  ggplot(aes(x = acft_category, fill = ev_nr_apt_loc)) + 
  geom_bar(position = "dodge") + 
  style + 
  labs(title = "Number of Accident by location",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#Engine types by aircraft category
acft_engines_merged_df%>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "AIR") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "BALL") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "BLIM") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "GLI") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "GYRO") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "HELI") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "PLFT") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "PPAR") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "RCKT") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "ULTR") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "WSFT") %>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + style

#frequency of event by time
acft_events_merged_df$ev_time <- acft_events_merged_df$ev_time %/% 100
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_time, colour = acft_category)) +
  geom_density() +
  xlim(0, 24) +
  labs(title = "Number of Accident by Category",
       x = "Aircraft Type", y = "Density") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#Event frequency by light condition
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  filter(!is.na(light_cond)) %>%
  ggplot(aes(x = acft_category, fill = light_cond)) + 
  geom_bar(position = "dodge") + style

#on ground collision by aircraft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(on_ground_collision)) %>% 
  ggplot(aes(x = acft_category, fill = on_ground_collision)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "Number of on ground collision by Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "On Ground Collision"))

#Sky condition
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(sky_cond_ceil)) %>% 
  ggplot(aes(x = acft_category, fill = sky_cond_ceil)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "Number of accident by Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Sky Condition"))

#Precipitation
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(wx_int_precip)) %>% 
  ggplot(aes(x = acft_category, fill = wx_int_precip)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "Number of Accident by Weather Condition",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#Wind condition
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(wind_vel_ind)) %>% 
  ggplot(aes(x = acft_category, fill = wind_vel_ind)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "Number of Accident by Wind Condition",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Wind Condition"))

#Second pilot
acft_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(second_pilot)) %>% 
  ggplot(aes(x = acft_category, fill = second_pilot)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "Number of Accident by Second Pilot",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Second Pilot"))

#Damage
acft_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(damage)) %>% 
  ggplot(aes(x = acft_category, fill = damage)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "Number of Accident by Damage",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Damage"))

#Type of flight
subset(acft_engines_merged_df, acft_category %in% "AIR") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "HELI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "BALL") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "BLIM") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "GYRO") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "PLFT") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style

subset(acft_engines_merged_df, acft_category %in% "ULTR") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style
