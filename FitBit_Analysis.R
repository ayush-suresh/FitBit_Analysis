library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(tidyr)
library(skimr)
library(ggplot2)
library(scales)
library(reshape2)
library(DataExplorer)
library(psych)

daily_activity <- read.csv("./FitBit_Data/dailyActivity_merged.csv")
hourly_сalories <- read.csv("./FitBit_Data/hourlyCalories_merged.csv")
hourly_intensities <- read.csv("./FitBit_Data/hourlyIntensities_merged.csv")
hourly_steps <- read.csv("./FitBit_Data/hourlySteps_merged.csv")
daily_sleep <- read.csv("./FitBit_Data/sleepDay_merged.csv")


head(daily_activity, 1)
head(hourly_сalories, 1)
head(hourly_intensities, 1)
head(hourly_steps, 1)
head(daily_sleep, 1)

library(dplyr)
glimpse(daily_activity)
glimpse(hourly_сalories)
glimpse(hourly_intensities)
glimpse(hourly_steps)
glimpse(daily_sleep)

n_distinct(daily_activity$Id)
n_distinct(hourly_сalories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(daily_sleep$Id)

sum(duplicated(daily_activity))
sum(duplicated(hourly_сalories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))
sum(duplicated(daily_sleep))

sum(is.na(daily_activity))
sum(is.na(hourly_сalories))
sum(is.na(hourly_intensities))
sum(is.na(hourly_steps))
sum(is.na(daily_sleep))

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

sum(duplicated(daily_sleep))


daily_activity<- daily_activity %>% 
  rename(Date = ActivityDate)
daily_sleep<- daily_sleep %>% 
  rename(Date = SleepDay)


daily_activity$Date = as.POSIXct(daily_activity$Date, format="%m/%d/%Y", tz = Sys.timezone())
daily_sleep$Date = as.POSIXct(daily_sleep$Date, format = "%m/%d/%Y", tz = Sys.timezone())
hourly_сalories$ActivityHour = as.POSIXct(hourly_сalories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
hourly_intensities$ActivityHour = as.POSIXct(hourly_intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
hourly_steps$ActivityHour = as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())

glimpse(daily_activity)
glimpse(hourly_сalories)
glimpse(hourly_intensities)
glimpse(hourly_steps)
glimpse(daily_sleep)

hourly <- merge(hourly_сalories, hourly_steps, by = c("Id", "ActivityHour")) %>% merge(hourly_intensities, by = c("Id", "ActivityHour")) %>%
  separate(ActivityHour, into = c("Date", "Time"), sep= " ") %>%
  mutate(Date = ymd(Date))  
glimpse(hourly)

daily <- merge(daily_activity, daily_sleep, by = c ("Id", "Date"), all = TRUE) 
glimpse(daily)


# build density plot




daily %>%  
  select(TotalSteps,
         TotalMinutesAsleep,
         SedentaryMinutes) %>%
  summary()


sum(daily$TotalSteps == 0)
sum(daily$SedentaryMinutes == 0)
sum(daily$SedentaryMinutes == 1440)

sum(daily$TotalSteps == 0 & daily$SedentaryMinutes == 1440)

daily <- subset(daily, !(TotalSteps == 0 & SedentaryMinutes == 1440))
glimpse(daily)

daily_users <- daily %>%
  group_by(Id) %>%
  summarise(DaysInUse = sum(TotalSteps > 0))
summary(daily_users$DaysInUse)
daily_users %>% group_by(DaysInUse) %>% summarise(count = n())


#split into 3 groups by median value: median, above median and below median - I will use this method in the analysis
daily_users_2 <- daily_users %>%
  mutate(usage_frequency = case_when (DaysInUse > 30 ~ "every day", DaysInUse == 30 ~ "regularly", DaysInUse < 30 ~ "sporadically",))
daily_users_2 %>% group_by(usage_frequency) %>% summarise(groups_by_median = n())




g1 <- daily_users_2 %>%
  group_by(usage_frequency) %>%
  summarise(users = n_distinct(Id)) %>%
  ggplot(aes(x="", y=users, fill=usage_frequency)) + 
  geom_bar(stat="identity", width=1, color="white") + 
  coord_polar("y", start=0) + theme_void() + 
  theme(legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        plot.title = element_text(size=15, margin = margin(10, 0, 0, 0, "pt")),
        plot.subtitle = element_text(size=15, margin = margin(10, 30, 30, 0, "pt")),
        plot.margin = margin(5, 50, 5, 100)) +
  geom_text(aes(label = percent(users/sum(users))), position = position_stack(vjust = 0.5), color="white", size=5) + 
  scale_fill_brewer(palette = "Set1", label = c(
    "14 IDs: 31 days in use", "6 IDs: 30 days in use", "13 IDs: < 30 days in use"))  + 
  labs(
    fill = "User Groups",
    title = "How Often Do Users Wear Their Trackers?", 
    subtitle = "User Groups by Days in Use" 
  )
print(g1)

daily_2 <- daily %>%
  left_join(daily_users_2, by = "Id") %>%
  select(-DaysInUse)

daily_2_summary <- daily_2 %>%
  group_by(usage_frequency) %>%
  summarise("Steps Taken, steps" = mean(TotalSteps), 
            "Calories Burnt, ccal" = mean(Calories), 
            "Sleep Time, min" = mean(TotalMinutesAsleep, na.rm=TRUE), 
            "Time in Bed, min" = mean(TotalTimeInBed, na.rm=TRUE), 
            "Sedentary Time, min" = mean(SedentaryMinutes), 
            "Light Activity, min" = mean(LightlyActiveMinutes), 
            "Fair Activity, min" = mean(FairlyActiveMinutes), 
            "High Activity, min" = mean(VeryActiveMinutes)) %>%
  reshape2::melt (id=1, measure=c(2:9)) %>%
  group_by(variable) %>%
  mutate(percent_fmt = round(value/ sum(value), 3)) 


ggplot(daily_2_summary, aes(fill=usage_frequency, x=usage_frequency, y=value)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_void() + 
  theme(legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        plot.margin = margin(5, 100, 5, 100),
        strip.text = element_text(size=5, margin = margin(10, 10, 10, 10, "pt")),
        axis.text.x = element_text(
          size = 5,  hjust = 0.5, vjust = 0.5, angle = 90), 
        axis.text.y = element_text(size = 5, hjust = 1, vjust = 1)) + 
  scale_fill_brewer(palette = "Set1") + facet_wrap( ~ variable, scale="free_y")




g2 <- ggplot(daily_2_summary, aes(fill=usage_frequency, y=value, x=variable)) + geom_bar(position="fill", stat="summary", fun="mean", color = "white", lwd = 0.25) + 
  scale_y_continuous(labels = percent_format())  +
  theme_void() + 
  theme(legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        plot.title = element_text(size=15, margin = margin(10, 0, 0, 0, "pt")),
        plot.subtitle = element_text(size=15, margin = margin(10, 30, 30, 0, "pt")),
        plot.margin = margin(5, 50, 5, 50), 
        #axis.title.y = element_text("Activity During a Day", size = 14, angle= 90, vjust=2), 
        axis.text.x = element_text(size = 8, angle = 0, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 8, hjust = 1, vjust = 1)) + 
  #guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(breaks = rev, direction = -1, palette = "Set1", label = c("13 IDs: < 30 days in use", "6 IDs: 30 days in use", "14 IDs: 31 days in use"))  + coord_flip()  + scale_x_discrete(limits = rev(levels(daily_2_summary$variable))) + 
  labs(
    x = "Activity During a Day",
    fill = "User Groups",
    title = "What Kind of Activity Do Users Prefer?",
    subtitle = "Daily Activity by User Groups" 
  ) + geom_text(aes(label = percent(percent_fmt, accuracy=0.1)), position = position_fill(vjust=0.5), color="white", size=3.5)
print(g2)



weekdays_summary <- daily_2 %>%
  mutate(weekday = weekdays(Date)) %>%
  group_by(weekday, usage_frequency) %>%
  summarise("Steps Taken, steps" = mean(TotalSteps), "Calories Burnt, ccal" = mean(Calories), "Sleep Time, min" = mean(TotalMinutesAsleep, na.rm=TRUE), "Time in Bed, min" = mean(TotalTimeInBed, na.rm=TRUE), "Sedentary Time, min" = mean(SedentaryMinutes), "Light Activity, min" = mean(LightlyActiveMinutes), "Fair Activity, min" = mean(FairlyActiveMinutes), "High Activity, min" = mean(VeryActiveMinutes), .groups = "drop") %>%
  reshape2::melt(id=c(1:2), measure=c(3:10)) 
#faceted data
g3 <- ggplot(weekdays_summary, aes(fill=usage_frequency, y=value, x=factor(weekday, weekdays(as.Date('1970-01-03') + 1:7)))) + 
  geom_bar(position="dodge", stat="summary", fun="mean", color = "white", lwd = 0.5) + 
  theme_void() + 
  theme(legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        plot.title = element_text(size=15, margin = margin(10, 0, 0, 0, "pt")),
        plot.subtitle = element_text(size=15, margin = margin(10, 30, 30, 0, "pt")),
        plot.margin = margin(5, 50, 5, 50),
        strip.text.x = element_text(size=10, margin = margin(3, 3, 3, 3, "pt")), 
        axis.text.x = element_text(size = 5, angle = 90, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 5, hjust = 1, vjust = 1)) + 
  facet_wrap(~ variable, scale="free_y", ncol=2) + 
  scale_fill_brewer(palette = "Set1", label = c("14 IDs: 31 days in use", "6 IDs: 30 days in use", "13 IDs: < 30 days in use")) + 
  labs(
    fill = "User Groups",
    title = "Does Activity Depend on the Day of the Week?",
    subtitle = "Daily Activity by User Groups and Days of the Week" 
  )
print(g3)


g4 <- daily_2 %>%
  group_by(Id, usage_frequency) %>%
  summarise(TotalSteps = mean(TotalSteps), .groups = "drop") %>% 
  arrange(desc(TotalSteps)) %>%
  mutate(TotalSteps_f = case_when (TotalSteps > 10000 ~ "10K+ steps", between(TotalSteps, 8000, 10000) ~ "8 - 10K steps", between(TotalSteps, 6000, 8000) ~ "6 - 8K steps", TotalSteps < 6000 ~ "< 6K steps")) %>%
  mutate(TotalSteps_f = fct_relevel(TotalSteps_f,c("< 6K steps", "6 - 8K steps", "8 - 10K steps", "10K+ steps"))) %>%
  group_by(TotalSteps_f, usage_frequency) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(y=count, x=usage_frequency, fill=TotalSteps_f)) +
  geom_bar(stat="identity", position="dodge", color = "white") + 
  scale_fill_brewer(palette = "Set1", label = c("< 6K steps", "6 - 8K steps", "8 - 10K steps", "10K+ steps")) + 
  labs(
    y = "Count of Users",
    x = "User Group",
    fill = "Steps per Day",
    title = "Do the Users Take More Steps if They Wear the Tracker Every Day?",
    subtitle = "Steps per Day by User Groups" 
  ) + 
  theme_light() + 
  theme(legend.text = element_text(size=5), 
        legend.title = element_text(size=5),
        plot.title = element_text(size=10, margin = margin(10, 0, 0, 0, "pt")),
        plot.subtitle = element_text(size=10, margin = margin(10, 20, 20, 0, "pt")),
        plot.margin = margin(5, 100, 5, 100),
        axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5, angle= 90),
        axis.text.x = element_text(size = 5, hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(size = 5, hjust = 1, vjust = 1)) +
  scale_x_discrete(label = c("14 IDs: 31 days in use", "6 IDs: 30 days in use", "13 IDs: < 30 days in use")) #+ theme(strip.text.x = element_text(margin = margin(3, 3, 3, 3, "pt")), axis.text.x = element_text(size = 5, angle = 90, hjust = 1, vjust = 1), axis.text.y = element_text(size = 5, hjust = 1, vjust = 1))
print(g4)


#corPlot(hourly[,4:7], cex = 0.8, cex.axis=0.8,upper=FALSE, scale=TRUE, main="Correlations in hourly activity")



hourly_2 <- hourly %>%
  left_join(daily_users_2, by = "Id") %>%
  select(-DaysInUse)
hourly_2 <- hourly_2 %>%
  mutate(weekday = weekdays(Date)) %>%
  separate(Time, into = c("hour", "minute", "second"), sep= ":") %>% select(-minute,-second) %>%
  mutate_if(is.character, str_replace, pattern = '^0', replacement = '') %>%
  mutate(hour = as.integer(hour)) %>%
  mutate(hour = replace(hour,is.na(hour),0))

g5 <- hourly_2 %>% add_count(Id, Date, name = "hours_in_use") %>%
  group_by(usage_frequency) %>%
  summarise(hours_in_use = mean(hours_in_use), .groups = "drop") %>%
  ggplot(aes(fill=usage_frequency, x=hours_in_use, y="")) + 
  geom_bar(stat = "identity", width=1, color="white")  + 
  theme_void()  + 
  theme(legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        plot.title = element_text(size=15, margin = margin(10, 0, 0, 0, "pt")),
        plot.subtitle = element_text(size=15, margin = margin(10, 30, 30, 0, "pt")),
        plot.margin = margin(5, 30, 5, 30)) +
  coord_polar("x", start=0)  + 
  geom_text(aes(label = paste0(formatC(hours_in_use), " hours")), position = position_stack(vjust = 0.5), color="white", size=5) + 
  scale_fill_brewer(palette = "Set1", label = c("14 IDs: 31 days in use", "6 IDs: 30 days in use", "13 IDs: < 30 days in use"))  + 
  labs(
    fill = "User Groups",
    title = "How Many Hours per Day Do the Users Wear the Trackers?",
    subtitle = "Hours of Wearing per Day by User Group" 
  )
print(g5)



hours_summary <- reshape2::melt(hourly_2, id=c(3,8:9), measure=c(4:7))
hours_summary %>% group_by(hour) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  ggplot(aes(x = hour, y= value)) +
  geom_bar(width = 0.9, stat = "summary", fun="mean", fill="#377eb8", colour="white", position="dodge") +
  theme_void() + theme(axis.text.x = element_text(size = 15,  hjust = 1, vjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  coord_polar(theta = "x", start = -.13, clip = "on") +
  scale_x_continuous(breaks = 0:24, expand = c(.001,0)) + scale_y_reverse() #+ ylim(190, 0)



g6 <- hours_summary %>% group_by(hour, usage_frequency) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  ggplot(aes(x = hour, y= value, fill=usage_frequency)) +
  geom_bar(width = 0.9, stat = "summary", fun="mean", colour="white", position="dodge") +
  theme_void()  + 
  theme(legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        plot.title = element_text(size=18, margin = margin(8, 0, 0, 0, "pt")),
        plot.subtitle = element_text(size=18, margin = margin(8, 20, 20, 0, "pt")),
        plot.margin = margin(5, 30, 5, 30),
        strip.text = element_text(size=0), 
        legend.position = "bottom", axis.text.x = element_text(size = 10,  hjust = 1, vjust = 1)) +
  coord_polar(theta = "x", start = -.13)  +
  scale_x_continuous(breaks = 0:24, expand = c(.001,0)) + facet_grid(cols=vars(usage_frequency)) + scale_y_reverse() + scale_fill_brewer(palette = "Set1", label = c("14 IDs: 31 days in use", "6 IDs: 30 days in use", "13 IDs: < 30 days in use"))  + 
  labs(
    fill = "User Groups",
    title = "What Hours Are the Most Active During the Day?",
    subtitle = "Activity Hours per Day by User Group" 
  )
print(g6)


