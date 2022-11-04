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

#Number of Injury level in each damage level 
acft_events_merged_df %>%
  filter(!is.na(damage)) %>% 
  filter(!is.na(ev_highest_injury)) %>% 
  ggplot(aes(x = damage, fill = ev_highest_injury)) +
  geom_bar() +
  labs(x = "Damage Level", y = "Count",
       title = "Number of Injury level in each damage level") + 
  guides(fill = guide_legend(title = "Injury Level"))

#Total number of injury by injury types
subset(acft_events_merged_df, !(ev_highest_injury %in% "UNKN")) %>%
  filter(!is.na(ev_highest_injury)) %>%
  ggplot(aes(ev_highest_injury, fill = ev_highest_injury)) + 
  geom_bar() + 
  ylim(0,50000) +
  labs(title="Number of Injury",
       x ="Injury Level", y = "Count") + style

#Injury level by Aircraft category
subset(acft_events_merged_df, !(ev_highest_injury %in% "UNKN")) %>%
  filter(!is.na(ev_highest_injury)) %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = ev_highest_injury)) + 
  geom_bar(position = "dodge") + 
  ylim(0,50000) +
  labs(title="Injury level by aircraft type",
       x ="Injury Level", y = "Count") + style

#Types of person injured
ggplot(injury_df, aes(inj_person_category, fill = inj_person_category)) + 
  geom_bar() + style + 
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))



#Ratio of person in injured by aircraft category
subset(inj_acft_merged_df, acft_category %in% "AIR") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "BALL") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "BLIM") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "GYRO") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "HELI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "PLFT") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "PPAR") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))
subset(inj_acft_merged_df, acft_category %in% "RCKT") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "ULTR") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

subset(inj_acft_merged_df, acft_category %in% "WSFT") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(acft_category, fill = inj_person_category)) + 
  geom_bar(position = "dodge")+
  labs(title="Number of Injury by person category",
       x ="Person Category", y = "Count") +
  guides(fill = guide_legend(title = "Person Category"))

#Total number of injury by aircraft category
acft_events_merged_df %>%
  filter(!is.na(acft_category)) %>% 
  filter(!is.na(inj_tot_t)) %>% 
  group_by(acft_category) %>%
  summarise(inj_tot_t = sum(inj_tot_t, na.rm=TRUE)) %>%
  ggplot(aes(x = acft_category, y = inj_tot_t, fill = acft_category)) + 
  geom_col() +
  geom_text(aes(label = inj_tot_t), vjust = -0.5) +
  labs(title="Total Injury by person category",
       x ="Aircraft Category", y = "Total Injury") +
  guides(fill = guide_legend(title = "Aircraft Category"))

#The change in the total injury over times by category
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
  labs(title = "The change in the total injury over times") + 
  guides(fill = guide_legend(title = "Aircraft Type")) +
  theme(strip.text.x = element_text(
    size = 10, color = "black"
  ), strip.background = element_rect(
    color="gray", fill="lightgray", size=1.2))

#The change in the total injury over times by category(without Rocket)
sum_df2 <- subset(sum_df, !(catg %in% 'RCKT'))
labs2 <- c("Airplane", "Balloon","Glider",
           "Blimp","Gyrocraft","Helicopter",
           "Powered Lift","Ultralight","Unknown",
           "Powered parachute","Weight shift")
names(labs2) <- c("AIR", "BALL","GLI",
                  "BLIM","GYRO","HELI",
                  "PLFT","ULTR","UNK",
                  "PPAR","WSFT")
p2 <- ggplot(data = sum_df2, aes(x = year, y = total, color = catg)) + 
  geom_line(size=1.1) +
  theme_light() +
  xlim(1982, 2020)

p2 + facet_wrap(~catg, scale = "free", 
                labeller = labeller(catg = labs)) +
  labs(title = "The change in the total injury over times") + 
  guides(fill = guide_legend(title = "Aircraft Type")) +
  theme(strip.text.x = element_text(
    size = 10, color = "black"
  ), strip.background = element_rect(
    color="gray", fill="lightgray", size=1.2))
