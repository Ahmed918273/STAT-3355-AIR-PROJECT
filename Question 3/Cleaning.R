# Cleaning for question 3

#download pre2008 files
acft_pre_df <- read.csv("./aircraft_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
events_pre_df <- read.csv("./events_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
injury_pre_df <- read.csv("./injury_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
#download post2008 files
acft_post_df <- read.csv("./aircraft_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
events_post_df <- read.csv("./events_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
injury_post_df <- read.csv("./injury_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))

#remove two columns from pre file to match the number of comlumns in both pre and post file for row-bining
events_post_df = subset(events_post_df, select = -c(dec_latitude, dec_longitude) )

#modify the strings that represent "passenger" in injury files
injury_pre_df$inj_person_category[injury_pre_df$inj_person_category == "PAX"] <- "PASS"
injury_post_df$inj_person_category[injury_post_df$inj_person_category == "Pass"] <- "PASS"

#combine pre and post files
acft_df <- rbind(acft_pre_df, acft_post_df)
events_df <- rbind(events_pre_df, events_post_df)
injury_df <- rbind(injury_pre_df, injury_post_df)

#merge acft and events files
acft_events_merged_df = merge(acft_df, events_df, by = "ev_id")
#merge acft and injury files
inj_acft_merged_df = merge(injury_df, acft_df, by ="ev_id")

#for styling some of the graphs
style <- geom_text(stat = "count", aes(label = ..count..), 
                   position = position_dodge(width=0.9), 
                   vjust = -.25, size = 2)

#Q3
#Is there a distinct injury type when an aircraft accident occurs? 
#What are the most common and the least common injury types? 

#1.Total number of injury by injury level
#2.Injury level by Aircraft category
#3.Types of person injured
#4.Ratio of person in injured by aircraft category
#5.The change in the total injury over times by category

#1.Total number of injury by types
subset(acft_events_merged_df, !(ev_highest_injury %in% "UNKN")) %>%
  filter(!is.na(ev_highest_injury)) %>%
  ggplot(aes(ev_highest_injury, fill = ev_highest_injury)) + 
  geom_bar() + 
  ylim(0,50000) +
  labs(title="1.Total number of injury by types",
       x ="Injury Level", y = "Count") + style +
  guides(fill = guide_legend(title = "Injury Level"))

#2.Total number of the injury by category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(inj_tot_t)) %>% 
  group_by(acft_category) %>%
  summarise(inj_tot_t = sum(inj_tot_t, na.rm=TRUE)) %>%
  ggplot(aes(x = acft_category, y = inj_tot_t, fill = acft_category)) + 
  geom_col() +
  geom_text(aes(label = inj_tot_t), vjust = -0.5) +
  labs(title="2.Total Number of the Injury by Category",
       x ="Aircraft Category", y = "Total Injury") +
  guides(fill = guide_legend(title = "Aircraft Category"))

#3.Injury level by Aircraft category
subset(acft_events_merged_df, !(ev_highest_injury %in% "UNKN")) %>%
  filter(!is.na(ev_highest_injury)) %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = ev_highest_injury)) + 
  geom_bar(position = "dodge") +
  facet_wrap(~acft_category, scale = "free") +
  labs(title="3.Injury Level by Aircraft Category",
       x ="Aircraft Category", y = "Count") + style +
  guides(fill = guide_legend(title = "Injury Level"))

#4.Types of person injured
ggplot(injury_df, aes(inj_person_category, fill = inj_person_category)) + 
  geom_bar() + style + 
  labs(title="4.Types of Person Injured",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

#5.Ratio of person in injured by aircraft category
inj_acft_merged_df%>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge") + style +
  facet_wrap(~acft_category, scale = "free") + 
  labs(title="5.Ratio of Person in Injured by Aircraft Category",
       x ="Aircraft Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))


#6.The change in the total injury over times by category
inj_year_acft_df <- data.frame(acft_events_merged_df$ev_year,
                               acft_events_merged_df$acft_category,
                               acft_events_merged_df$inj_tot_t)

colnames(inj_year_acft_df) <- c('year','catg', 'inj_tot')

inj_year_acft_cleaned_df <- na.omit(inj_year_acft_df)

sum_df <- inj_year_acft_cleaned_df %>% 
  group_by(catg, year) %>% 
  summarise(total = sum(inj_tot))

labs <- c("Airplane", "Balloon","Glider",
          "Blimp","Gyrocraft","Helicopter",
          "Powered Lift","Ultralight","Unknown",
          "Powered parachute","Weight shift","Rocket")

names(labs) <- c("AIR", "BALL","GLI",
                 "BLIM","GYRO","HELI",
                 "PLFT","ULTR","UNK",
                 "PPAR","WSFT","RCKT")

p <- ggplot(data = sum_df, aes(x = year, y = total, color = catg)) + 
  geom_line(size=1.1) +
  theme_light() +
  xlim(1982, 2020)

p + facet_wrap(~catg, scale = "free", 
               labeller = labeller(catg = labs)) +
  labs(title = "6.The Change in the Total Injury over Times") + 
  guides(fill = guide_legend(title = "Aircraft Type")) +
  theme(strip.text.x = element_text(
    size = 10, color = "black"
  ), strip.background = element_rect(
    color="gray", fill="lightgray", size=1.2))
