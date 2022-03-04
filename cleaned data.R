install.packages("tidyverse")
library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("lubridate")
library(lubridate)
install.packages("hrbrthemes")
library(hrbrthemes)

## import data 

daily_activity <- read.csv(file.choose())
daily_calories <- read.csv(file.choose())
daily_intensities <- read.csv(file.choose()) ## choose from file
daily_steps <- read.csv(file.choose())
hourly_calories <- read.csv(file.choose())
hourly_intensities <- read.csv(file.choose())
hourly_steps <- read.csv(file.choose())
sleep_day <- read.csv(file.choose())
weight_loginfo <- read.csv(file.choose())


## check if there's any missing data and extreme values 

library(skimr)

skim(daily_activity)
skim(daily_calories)
skim(daily_intensities)
skim(daily_steps)
skim(hourly_calories)
skim(hourly_intensities)
skim(hourly_steps)
skim(sleep_day)


## preview dataset 


head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_steps)
head(hourly_calories)
head(hourly_intensities)
head(hourly_steps)
head(sleep_day)


## check duplicates 

sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(daily_steps))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_day))


## Remove duplicate
library(tidyverse) 

sleep_day <- sleep_day %>% 
  distinct()

##check
sum(duplicated(sleep_day))


##check NA data

sum(is.na(daily_activity))
sum(is.na(daily_calories))
sum(is.na(daily_intensities))
sum(is.na(daily_steps))
sum(is.na(hourly_calories))
sum(is.na(hourly_intensities))
sum(is.na(hourly_steps))
sum(is.na(sleep_day))


## 3. PROCESS 

## Understand the dataset 

n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_steps$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_loginfo$Id)


## There are 33 ID's in daily active/calories/intensities/steps, hourly calories/intensities/steps 
## 24  ID's in sleep and only 8 ID's in weight , which means not every one tracks all the data  


str(daily_activity)
str(daily_calories)
str(daily_intensities)
str(daily_steps)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)
str(sleep_day)

## Convert string data type into datetime data type

daily_activity$ActivityDate <- parse_date_time(daily_activity$ActivityDate, "%m/%d/%y")
str(daily_activity)

daily_calories$ActivityDay <- parse_date_time(daily_calories$ActivityDay, "%m/%d/%y")
daily_intensities$ActivityDay <- parse_date_time(daily_intensities$ActivityDay, "%m/%d/%y")
daily_steps$ActivityDay <- parse_date_time(daily_steps$ActivityDay, "%m/%d/%y")
hourly_calories$ActivityHour <- parse_date_time(hourly_calories$ActivityHour, "%m/%d/%y %H:%M:%S,%p")
hourly_intensities$ActivityHour <- parse_date_time(hourly_intensities$ActivityHour, "%m/%d/%y %H:%M:%S,%p")
hourly_steps$ActivityHour <- parse_date_time(hourly_steps$ActivityHour, "%m/%d/%y %H:%M:%S,%p")
sleep_day$SleepDate <- parse_date_time(sleep_day$SleepDay, "%m/%d/%y %H:%M:%S,%p")
weight_loginfo$WeightDate <- parse_date_time(weight_loginfo$Date, "%m/%d/%y %H:%M:%S,%p")



str(daily_activity)
str(daily_calories)
str(daily_intensities)
str(daily_steps)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)
str(sleep_day)
str(weight_loginfo)

## Merge datasets 

## Merge all hourly data frames 

hourly_data <- merge(hourly_calories, hourly_intensities) %>% 
  left_join(hourly_steps, by = c("Id", "ActivityHour")) %>% 
  separate(ActivityHour,sep=" ", into = c("date","time"),remove = FALSE)

## convert hourly data frames data format

library(lubridate)
library(tidyverse)
library(lubridate)

hourly_data$date_format <- hourly_data$date
hourly_data$Subject_Id <- as.character(hourly_data$Id)
glimpse(hourly_data)

str(hourly_data)


## Merge all daily data
library(tidyverse)
library(skimr)


daily_data_1 <- merge(daily_activity, daily_calories)
daily_data_2 <- merge(daily_data_1, daily_intensities)
daily_data <- merge(daily_data_2, daily_steps)


## convert ID's data type to string as subject Id 

daily_data$Subject_Id <- as.character(daily_data$Id)
head(daily_data,3)

str(daily_data)

## remove duplicate columns 

daily_data <- select(daily_data, -c(TotalDistance, Id, StepTotal, ActivityDay))

str(sleep_day)

sleep_day$Subject_Id <- as.character(sleep_day$Id)
sleep_day$ActivityDate <- sleep_day$SleepDate

str(sleep_day)

## Merge sleep_day and daily_activity data

daily_activity_sleep <- merge(sleep_day, daily_data, by=c("Subject_Id", "ActivityDate"))

str(daily_data)

daily_activity_sleep <- select(daily_activity_sleep -c(StepTotal,Id.x))
daily_activity_sleep <- select(daily_activity_sleep, -c(StepTotal, Id.x,Id.y))

str(daily_activity_sleep)

## 4.ANALYZE 
## Check overall users activity data

daily_activity_sleep %>% 
  select(TotalSteps,TrackerDistance, SedentaryMinutes,VeryActiveMinutes,FairlyActiveMinutes, LightlyActiveMinutes, TotalTimeInBed, TotalMinutesAsleep) %>% 
  summary()

## Sleep day diff across week days 
## calculate total hours

sleep_day$TotalHoursSleep <- sleep_day$TotalMinutesAsleep/60

## Add week day to column 

sleep_day$sleepWeekday <- weekdays(as.Date(sleep_day$SleepDate))

## Groupby Weekday 
weekly_sleep_hour<- sleep_day %>% 
  group_by(sleepWeekday) %>% 
  summarise(Average_Sleep_Hour = mean(TotalHoursSleep))

Week_Day <- factor(weekly_sleep_hour$sleepWeekday, levels = c('Sunday', 'Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

weekly_sleep_hour
ggplot(weekly_sleep_hour, aes(x=Week_Day,y=Average_Sleep_Hour))+geom_col()


## Group subjects by activity levels 

data_by_usertype <-daily_activity_sleep %>% 
  summarise(
    TotalHoursInBed <- TotalTimeInBed/60,
    TotalHoursSleep <- TotalMinutesAsleep/60,
    activity_level = factor(case_when(
      SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes)  & VeryActiveMinutes < mean(VeryActiveMinutes)~"Sedentary",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes)  & VeryActiveMinutes < mean(VeryActiveMinutes)~"Lightly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes)  & VeryActiveMinutes < mean(VeryActiveMinutes)~"Fairly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes)  & VeryActiveMinutes > mean(VeryActiveMinutes)~"Very Active",
      
    ), levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active") ), Calories,TotalSteps,TrackerDistance,TotalHoursInBed,TotalHoursSleep,Subject_Id) %>% drop_na()


## convert the total user types by percentage 

user_percentage <- data_by_usertype %>% 
  group_by(activity_level) %>% 
  summarise(total=n()) %>% 
  mutate(totals=sum(total)) %>% 
  group_by(activity_level) %>% 
  summarise(total_percentage =total/totals)


## User distribution of diff levels of activity type 

ggplot(data=user_percentage)+ 
  geom_col(mapping =aes(x=activity_level, y=total_percentage),fill="lightblue")+
  labs(title ="Percentage of Different Activity level",size=50)


## Total steps and Tracker distance Differ across activity groups 

ggplot(data=data_by_usertype)+ 
  geom_col(mapping =aes(x=activity_level, y=TotalSteps),fill="lightblue")+
  labs(title ="Total steps across diff activity level",size=50)

ggplot(data=data_by_usertype)+ 
  geom_col(mapping =aes(x=activity_level, y=TrackerDistance),fill="lightblue")+
  labs(title ="Total steps and Tracker distance",size=50)


## User group assigned by usage days 


user_frequency <- daily_activity_sleep %>%
  group_by(Subject_Id) %>% 
  summarise(ActivityDate=sum(n())) %>% 
  mutate(Usage=case_when(
    ActivityDate >= 1 & ActivityDate <=7 ~"Low Use",
    ActivityDate >= 7 & ActivityDate <=21 ~"Moderate Use",
    ActivityDate >= 21 & ActivityDate <=31 ~"High Use")) %>% 
  mutate(Usage= factor(Usage, levels = c("Low Use","Moderate Use","High Use"))) %>% 
  rename(Use_days=ActivityDate) %>% 
  group_by(Usage)

head(user_frequency)


## convert user frequency by percentage

freq_percentage <- user_frequency %>% 
  group_by(Usage) %>% 
  summarise(total=n()) %>% 
  mutate(totals= sum(total)) %>% 
  group_by(Usage) %>% 
  summarise(total_percent=total/totals) %>% 
  drop_na()
  

## user distribution of different level of activity type
ggplot(data=freq_percentage)+
  geom_col(mapping = aes(x=Usage,y=total_percent),fill="grey")+
  labs(title = "Activity level vs Total Percentage", size=50)

## weight log info

weight_loginfo$Subject_Id <- as.character(weight_loginfo$Id)
weight_activity_all <-merge(x=weight_loginfo,y=daily_data, by="Subject_Id",all=TRUE)

## Adding column based

weight_activity_all <- weight_activity_all %>% 
  mutate(WeightPeople=case_when(
         is.na(WeightDate)~"No Weight Log",
         !is.na(WeightDate)~"People With Weight Log"
         ))
data_histogram <- weight_activity_all %>% 
  group_by(WeightPeople) %>% 
  summarise(mean_activeminutes= round(mean(VeryActiveMinutes),2))


ggplot(data_histogram, aes(x=WeightPeople,y=mean_activeminutes))+
  geom_bar(stat = "identity",width=0.5)+
  theme_classic()


## Relation between days of week and average intensity

## create daily intensity dataset 

Intensities_day <- hourly_data %>% 
  group_by(Subject_Id, ActivityHour) %>% 
  drop_na() %>% 
  summarise(sum_TotalIntensities= sum(TotalIntensity))

## Add weekday column 

Intensities_day$Intensity_weekday <-weekdays(as.Date(Intensities_day$ActivityHour))


## Grouping to by Day of the weeks 

Intensity_weekday_group <- Intensities_day %>% 
  group_by(Intensity_weekday) %>% 
  drop_na() %>% 
  summarise(mean_sum_TotalIntensities = mean(sum_TotalIntensities))

## How users do activity with diff intensities during a week 

ggplot(data=Intensity_weekday_group)+
  geom_col(mapping = aes(x=Intensity_weekday,y=mean_sum_TotalIntensities,fill=mean_sum_TotalIntensities),width = 0.8,position="dodge")+
  labs(title = "Intensity during Week",x="Day of week",y="Average Intensity",fill="Intensity")


## Workout Pattern during Day 
library(tidyverse)
library(skimr)
library(lubridate)

hourly_data$hour <- format(as.POSIXct(hourly_data$ActivityHour,"%m/%d/%y %H:%M:%S"),"%H")
Intensity_hourly <- hourly_data %>% 
  group_by(ActivityHour,hour) %>% 
  drop_na() %>% 
  summarise(mean_TotalIntensity=mean(TotalIntensity),StepTotal)


## Add weekday column 

Intensity_hourly$Intensity_Weekday <- weekdays(as.Date(Intensity_hourly$ActivityHour))


## Plot


ggplot(data=Intensity_hourly)+
  geom_col(mapping = aes(x=hour,y=mean_TotalIntensity))+
  labs(title="Hourly Intensity in a Day",x="Time",y="Average Intensity")



  




