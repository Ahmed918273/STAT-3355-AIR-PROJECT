# Cleaning for question 8

#download pre files
acft_pre_df <- read.csv("./aircraft_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
events_pre_df <- read.csv("./events_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
eventsq_pre_df <- read.csv("./Events_Sequence_pre.csv", header = TRUE, sep = ",", na.strings=c("","NA"))

#download post files
acft_post_df <- read.csv("./aircraft_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
events_post_df <- read.csv("./events_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
eventsq_post_df <- read.csv("./Events_Sequence_post.csv", header = TRUE, sep = ",", na.strings=c("","NA"))

#remove two columns to match the number of column for row-binding
events_post_df = subset(events_post_df, select = -c(dec_latitude, dec_longitude) )

#row-bind pre and post files
acft_df <- rbind(acft_pre_df, acft_post_df)
events_df <- rbind(events_pre_df, events_post_df)
eventsq_df <- rbind(eventsq_pre_df, eventsq_post_df)

#merge acft and event files
acft_events_merged_df = merge(acft_df, events_df, by = "ev_id")

#frequency of accident by phase of flight code
acft_df %>%
  filter(!is.na(phase_flt_spec)) %>%
  ggplot(aes(y = fct_infreq(as.character(phase_flt_spec)))) + 
  geom_bar(fill="lightblue") + 
  theme_grey(base_size = 5) + 
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width=0.9), 
            vjust = -.5, hjust = -0.3, size = 2)

#creating a dataframe for frequency of accident by phase of flight(more splecific)
ae_merged_df <- merge(acft_df, eventsq_df, by = "ev_id")
aee_merged_df <- merge(ae_merged_df, events_df, by = "ev_id")
esq_df <- subset(aee_merged_df, select = c(ev_id, ev_year, acft_category, Occurrence_Code))

esq_df$digLow <- esq_df$Occurrence_Code %% 1000
esq_df$digUp <- floor(esq_df$Occurrence_Code / 1000)

Low <- c(091,092,093,094,095,096,097,100,110,120,130,140,150,000,010,020,
         030,040,050,060,070,080,081,082,090,160,170,180,190,191,192,193,
         194,200,210,220,230,231,232,240,241,242,243,244,245,250,260,270,
         271,280,290,300,310,320,330,331,332,333,334,335,336,337,338,340,
         341,342,343,350,360,361,362,370,380,390,400,401,402,410,420,430,
         440,441,450,460,470,480,490,500,510,600,900,901,990)

Up <- c(152,153,154,200,201,202,203,204,250,251,252,253,300,301,100,150,
        151,350,400,401,402,403,404,405,450,451,452,453,500,501,502,503,
        504,505,506,507,508,509,550,551,552,600,650,700,750,800,990)

evUp <- c("Standing-Engine(s) Start-up","Standing-Engine(s) Operating",
          "Standing-Engine(s) Shutdown","Pushback/Towing",
          "Pushback/Tow-Engine Not Oper","Pushback/Tow-Engine Start-up",
          "Pushback/Tow-Engine Oper","Pushback/Tow-Engine Shutdown","Taxi",
          "Taxi-to Runway","Taxi-into Takeoff Position","Taxi-from Runway",
          "Takeoff","Takeoff-Rejected Takeoff","Prior to Flight","Standing",
          "Standing-Engine(s) Not Oper","Initial Climb","Enroute",
          "Enroute-Climb to cruise","Enroute-Cruise",
          "Enroute-Change of cruise level","Enroute-Descent",
          "Enroute-Holding (IFR)","Maneuvering","Maneuvering-Aerobatics",
          "Maneuvering-Low-alt flying","Maneuvering-Hover","Approach",
          "Approach-IFR Initial Approach","Approach-IFR Final Approach",
          "Approach-Circling (IFR)","Approach-IFR Missed Approach",
          "Approach-VFR Pattern Crosswind","Approach-VFR Pattern Downwind",
          "Approach-VFR Pattern Base","Approach-VFR Pattern Final",
          "Approach-VFR Go-Around","Landing","Landing-Flare/Touchdown",
          "Landing-Landing Roll","Emergency Descent","Uncontrolled Descent",
          "Post-Impact","After Landing","Other","Unknown")

evLow <- c("Tailstrike","Hard landing","Dragged wing/rotor/float/other",
           "Landing gear collapse","Landing gear not configured",
           "Nose over/nose down","Roll over","Air traffic event",
           "Cabin safety event","Control flight into terr/obj",
           "Emergency descent initiated","Engine shutdown",
           "Fire/smoke (non-impact)","Unknown or undetermined",
           "Aircraft loading event","Aircraft servicing event",
           "Preflight or dispatch event","Aircraft maintenance event",
           "Aircraft inspection event","Attempted remediation/recovery",
           "Airport occurrence","Ground handling event",
           "AC/prop/rotor contact w person","Prop/jet/rotor blast/suction",
           "Abnormal runway contact","Explosion (non-impact)",
           "Fire/smoke (post-impact)","Explosion (post-impact)","Fuel related",
           "Fuel starvation","Fuel exhaustion","Fuel contamination",
           "Wrong fuel","Ground collision","Icing encounter",
           "Low altitude operations","Loss of control on ground",
           "Dynamic Rollover","Ground resonance","Loss of control in flight",
           "Aerodynamic stall/spin","VFR encounter with IMC",
           "Retreating blade stall","Settling with power/vortex ring state",
           "Mast bumping","Midair collision","Near midair collision",
           "Abrupt maneuver","Inflight upset","Course deviation","Altitude deviation",
           "Runway excursion","Runway incursion animal",
           "Runway incursion veh/AC/person","Sys/Comp malf/fail (non-power)",
           "Pressure/environ sys malf/fail","Electrical system malf/failure",
           "Flight control sys malf/fail","Flight instrument malf/fail",
           "Nav system malfunction/failure","Comm system malf/failure",
           "Aircraft structural failure","Part(s) separation from AC",
           "Powerplant sys/comp malf/fail","Loss of engine power (total)",
           "Loss of engine power (partial)","Uncontained engine failure",
           "Security/criminal event","Turbulence encounter",
           "Aircraft wake turb encounter","Clear air turbulence encounter",
           "Landing area undershoot","Landing area overshoot",
           "Windshear or thunderstorm","Other weather encounter",
           "VFR encounter with IMC","Loss of visual reference",
           "Terrain avoidance alert","Collision avoidance alert",
           "Stall warn/stick-shaker/pusher","Off-field or emergency landing",
           "Ditching","Hazardous material leak/spill","Evacuation",
           "Collision with terr/obj (non-CFIT)","External load event (Rotorcraft)",
           "Collision during takeoff/land","Loss of lift","Glider tow event",
           "Simulated/training event","Miscellaneous/other","Birdstrike",
           "Missing aircraft")

occLow_df <- data.frame(Low, evLow)
occUp_df <- data.frame(Up, evUp)

esq_df2 <- merge(esq_df, occLow_df, by.x = "digLow", by.y= "Low")
esq_df3 <- merge(esq_df2, occUp_df, by.x = "digUp", by.y= "Up")

#frequency of accident by phase of flight(more splecific)(occurrence code first three digit)
subset(esq_df3, acft_category %in% "AIR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "BALL") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "BLIM") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "GYRO") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "HELI") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "PLFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "PPAR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "RCKT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "ULTR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "WSFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evUp, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

#frequency of accident by phase of flight(more splecific)(occurrence code last three digit)
subset(esq_df3, acft_category %in% "AIR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "BALL") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "BLIM") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "GYRO") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "HELI") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "PLFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "PPAR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "RCKT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "ULTR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

subset(esq_df3, acft_category %in% "WSFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(evLow, fill = acft_category)) + 
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 5))

#frequency of accident by phase of flight over times(more splecific)(event code last three digit)
subset(esq_df3, acft_category %in% "AIR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "BALL") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "AIR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "GYRO") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "HELI") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "PLFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "PPAR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "RCKT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "ULTR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "WSFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evLow)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

#frequency of accident by phase of flight over times(more splecific)(event code fisrt three digit)

subset(esq_df3, acft_category %in% "AIR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "BALL") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "AIR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "GLI") %>% 
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "GYRO") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "HELI") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "PLFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "PPAR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "RCKT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "ULTR") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")

subset(esq_df3, acft_category %in% "WSFT") %>%
  filter(!is.na(acft_category)) %>%
  ggplot(aes(x = ev_year,  color = evUp)) + 
  geom_freqpoly(size=1.1) +
  theme_light() +
  theme(legend.key.size = unit(0.1, 'cm'),
        legend.text=element_text(size=5),
        legend.position="bottom") +
  guides(fill=guide_legend(ncol = 2, title="Cause")) +
  xlab("Year") + ylab("Count")
