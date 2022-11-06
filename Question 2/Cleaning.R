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

#Q2 
#Which aircraft are most susceptible to crashing? 
#Are there specific aircraft characteristics 
#that correlate to aircraft accidents?

# 1.Total number of events by air craft category
# 2.Event type by air craft category 
# 3.Phase of operations where event occurred
# 4.Engine type
# 5.Frequency of event by time
# 6.Event frequency by light condition
# 7.On ground collision by aircraft category
# 8.Sky condition by aircraft category
# 9.Precipitation by aircraft category
#10.Wind condition by aircraft category
#11.Second pilot by aircraft category
#12.Damage by aircraft category
#13.Type of flight by aircraft category
#14.Number of Injury level of each damage level

#1.Total number of accident by category
acft_df %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = acft_category)) + 
  geom_bar() + 
  geom_text(stat ='count', aes(label = ..count..), vjust=-1) +
  ylim(0,81000) +
  labs(title = "1.Total Number of Accident by Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#2.Event type by air craft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = ev_type)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "2.Event Type by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Event Type"))

#3.Phase of operations where event occurred
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  filter(!is.na(ev_nr_apt_loc)) %>%
  ggplot(aes(x = acft_category, fill = ev_nr_apt_loc)) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~acft_category, scale = "free") +
  labs(title = "3.Phase of Operations where Event Occurred",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Location")) 

#4.Engine type
acft_engines_merged_df%>% 
  filter(!is.na(eng_type)) %>%
  ggplot(aes(x = acft_category, fill = eng_type)) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~acft_category, scale = "free") +
  labs(title = "4.Engine Type by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Engine Type")) 


# 5.Frequency of event by time
acft_events_merged_df$ev_time <- acft_events_merged_df$ev_time %/% 100
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_time, color = acft_category)) +
  geom_density() +
  xlim(0, 24) +
  labs(title = "5.Frequency of Event by Time of Day",
       x = "Aircraft Type", y = "Density") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#6.Event frequency by light condition
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>%
  filter(!is.na(light_cond)) %>%
  ggplot(aes(x = acft_category, fill = light_cond)) + 
  geom_bar(position = "dodge") +
  facet_wrap(~acft_category, scale = "free") +
  labs(title = "Event frequency by light condition",
       x = "Aircraft Type", y = "Density") +
  guides(fill = guide_legend(title = "Aircraft Type"))


#7.On ground collision by aircraft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(on_ground_collision)) %>% 
  ggplot(aes(x = acft_category, fill = on_ground_collision)) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~acft_category, scale = "free") +
  labs(title = "7.On-Ground Collision by Aircraft Category",
       x = "Aircraft Type", y = "Density") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#8.Sky condition by aircraft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(sky_cond_ceil)) %>% 
  ggplot(aes(x = acft_category, fill = sky_cond_ceil)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "8.Sky Condition by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Sky Condition")) + 
  facet_wrap(~acft_category, scale = "free")

#9.Precipitation by aircraft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(wx_int_precip)) %>% 
  ggplot(aes(x = acft_category, fill = wx_int_precip)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "9.Precipitation by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Aircraft Type"))

#10.Wind condition by aircraft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(wind_vel_ind)) %>% 
  ggplot(aes(x = acft_category, fill = wind_vel_ind)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "10.Wind Condition by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Wind Condition")) + 
  facet_wrap(~acft_category, scale = "free")

#11.Second pilot by aircraft category
acft_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(second_pilot)) %>% 
  ggplot(aes(x = acft_category, fill = second_pilot)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "11.Second pilot by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Second Pilot")) + 
  facet_wrap(~acft_category, scale = "free")

#12.Damage by aircraft category
acft_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(damage)) %>% 
  ggplot(aes(x = acft_category, fill = damage)) + 
  geom_bar(position = "dodge") +
  style +
  labs(title = "12.Damage by Aircraft Category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Damage"))+ 
  facet_wrap(~acft_category, scale = "free")


#13.Type of flight by aircraft category
acft_engines_merged_df %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style+
  facet_wrap(~acft_category, scale = "free") +
  labs(title = "13.Type of flight by aircraft category",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "AIR") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Airplane)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "HELI") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Helicopter)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "BALL") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (balloon)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "BLIM") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Blimp)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Glider)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "GYRO") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Gyrocraft)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "PLFT") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Powered-Lift)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

subset(acft_engines_merged_df, acft_category %in% "ULTR") %>% 
  filter(!is.na(acft_category)) %>%
  filter(!is.na(type_fly)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style +
  labs(title = "13.Type of flight (Ultralight)",
       x = "Aircraft Type", y = "Count") +
  guides(fill = guide_legend(title = "Type of Flight"))

#14.Number of Injury level of each damage level by Aircraft Category 
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(damage)) %>% 
  filter(!is.na(ev_highest_injury)) %>% 
  ggplot(aes(x = damage, fill = ev_highest_injury)) +
  geom_bar() +
  labs(x = "Damage Level", y = "Count",
       title = "14.Number of Injury level in each damage level") + 
  guides(fill = guide_legend(title = "Injury Level"))+ 
  facet_wrap(~acft_category, scale = "free")


subset(acft_engines_merged_df, acft_category %in% "ULTR") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = acft_category, fill = type_fly)) + 
  geom_bar(position = "dodge") + style
