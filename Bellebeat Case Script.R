#install various packages in r

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")

library(tidyverse)
library(ggplot2)
library(ggpubr)


# exploring data. read following dataframe for our analysis

dailyActivity <- read_csv("bellebeat data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
View(dailyActivity)

dailyCalories <- read_csv("bellebeat data/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
View(dailyCalories)

dailyIntensities <- read_csv("bellebeat data/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
View(dailyIntensities)

dailySteps <- read_csv("bellebeat data/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
View(dailySteps)

sleepDay <- read_csv("bellebeat data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
View(sleepDay)

weightLogInfo <- read_csv("bellebeat data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
View(weightLogInfo)


# closer look at data
summary(dailyActivity)
str(dailyActivity)

summary(dailyCalories)
str(dailyCalories)

summary(dailyIntensities)
str(dailyIntensities)

summary(dailySteps)
str(dailySteps)

summary(sleepDay)
str(sleepDay)

summary(weightLogInfo)
str(weightLogInfo)

#distinct entries in all columns

sapply(dailyActivity, function(x) length(unique(x)))
sapply(dailyCalories, function(x) length(unique(x)))
sapply(dailyIntensities, function(x) length(unique(x)))
sapply(dailySteps, function(x) length(unique(x)))
sapply(sleepDay, function(x) length(unique(x)))
sapply(weightLogInfo, function(x) length(unique(x)))


#data cleaning & transforming
## select dailyActivity, sleepday files for trends

# 1.The clean_names() function makes sure that the column names are unique and consistent, contains only characters, numbers, and underscores in the names.
# 2.remove_empty remove null value in all the rows and columns
# 3.identify then remove duplicates
# 4.Convent data variable from character to date in format "%m/%d/%Y" and rename as date
# 5. Add weekday column
# 6. Create one merge file, clean merge file and then export to tableau for viz

daily_activity <- clean_names(dailyActivity) %>% 
  remove_empty(which = c("rows")) %>% 
  remove_empty(which = c("cols")) %>% 
  rename(date=activity_date)

duplicated(daily_activity)

daily_activity$date <- as.Date(daily_activity$date, format="%m/%d/%Y")
daily_activity$weekday <- wday(daily_activity$date, label=TRUE)


sleep_day <- clean_names(sleepDay) %>%
  remove_empty(which = c("rows")) %>% 
  remove_empty(which = c("cols"))
duplicated(sleep_day)  

sleep_Day <- distinct(sleep_day) %>% 
  rename(date=sleep_day)

sleep_Day$date <-as.Date(sleep_Day$date, format="%m/%d/%Y")

weight_Log <- clean_names(weightLogInfo) %>% 
  remove_empty(which = c("rows")) %>% 
  remove_empty(which = c("cols"))

duplicated(weight_Log)
weight_Log$date <- as.Date(weight_Log$date, format="%m/%d/%Y")



merge_data1 <-merge(daily_activity,sleep_Day,by=c("id","date"), all=TRUE)
merge_data <- merge(merge_data1,weight_Log,by=c("id","date"), all=TRUE)


duplicated(merge_data)
str(merge_data)
sapply(merge_data, function(x) length(unique(x)))


write_csv(merge_data, "merge_data.csv")




## DATA ANALYSIS

## q1. more active, more calories burn?  yes, coefficient increase as the user more active

## coeff of very active minutes vs calories 0.62

ggscatter(merge_data, x = "very_active_minutes", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "VeryActiveMinutes", ylab = "Calories")
c1 <- cor.test(merge_data$very_active_minutes, merge_data$calories, 
                method = "pearson")
c1

## coef=0.3 fairly_active_minutes vs calories

ggscatter(merge_data, x = "fairly_active_minutes", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "FairlyActiveMinutes", ylab = "Calories")
c2 <- cor.test(merge_data$fairly_active_minutes, merge_data$calories, 
               method = "pearson")
c2

##  coef=0.29 lightly_active_minutes vs calories

ggscatter(merge_data, x = "lightly_active_minutes", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "LightlyActiveMinutes", ylab = "Calories")
c3 <- cor.test(merge_data$lightly_active_minutes, merge_data$calories, 
               method = "pearson")
c3


## coef= -0.1   sedentary minutes vs calories

ggscatter(merge_data, x = "sedentary_minutes", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SedentaryyActiveMinutes", ylab = "Calories")
c4 <- cor.test(merge_data$sedentary_minutes, merge_data$calories, 
               method = "pearson")
c4


## q2.  how active daily?

#pie chart

sedentary <-mean(merge_data$sedentary_minutes)
veryactive <- mean(merge_data$very_active_minutes)
lightactive <- mean(merge_data$lightly_active_minutes)
fairlyactive <- mean(merge_data$fairly_active_minutes)

minutes <-c(sedentary, veryactive,lightactive, fairlyactive)

intensity <- c('Sedentary','VeryActive','LightlyActive','FairlyActive')

piedf<- data.frame(intensity,minutes)

## export to tableau
write_csv(piedf,"piedf.csv")


slices <- c(sedentary,veryactive,lightactive,fairlyactive)
lbls <- c("Sedentary", "VeryActive", "LightlyActive","FairlyActive")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="How Active Total")




## q3 which day in the week most active, which day least?
##tableau viz


## q4 more active, more sleep?
# various active minutes vs total minute asleep

# no, activity level and sleep is not correlated, however, the less sedentary minutes, the more sleep

## coeff of very active minutes vs sleep is -0.088

sum(is.na(merge_data$total_minutes_asleep))#there are count 530 "NA" in total_minutes_sleep column merge data


ggscatter(merge_data, x = "very_active_minutes", y = "total_minutes_asleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "VeryActiveMinutes", ylab = "TotalMinutesAsleep")

s1 <- cor.test(merge_data$very_active_minutes, merge_data$total_minutes_asleep, 
               method = "pearson")
s1

## coeff of very fairly active minutes vs sleep is -0.25
ggscatter(merge_data, x = "fairly_active_minutes", y = "total_minutes_asleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "FairlyActiveMinutes", ylab = "TotalMinutesAsleep")

s2 <- cor.test(merge_data$fairly_active_minutes, merge_data$total_minutes_asleep, 
               method = "pearson")
s2

## coeff of very lightly active minutes vs sleep is 0.028
ggscatter(merge_data, x = "lightly_active_minutes", y = "total_minutes_asleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "LightlyActiveMinutes", ylab = "TotalMinutesAsleep")

s3 <- cor.test(merge_data$lightly_active_minutes, merge_data$total_minutes_asleep, 
               method = "pearson")
s3

## coeff of sedentary minutes vs sleep is -0.6
ggscatter(merge_data, x = "sedentary_minutes", y = "total_minutes_asleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SedentaryMinutes", ylab = "TotalMinutesAsleep")

s4 <- cor.test(merge_data$sedentary_minutes, merge_data$total_minutes_asleep, 
               method = "pearson")
s4

merge_data$total_active_minutes <- merge_data$very_active_minutes+merge_data$lightly_active_minutes+merge_data$fairly_active_minutes

## coeff of total active minutes vs sleep is -0.069
ggscatter(merge_data, x = "total_active_minutes", y = "total_minutes_asleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "TotalActiveMinutes", ylab = "TotalMinutesAsleep")

s5 <- cor.test(merge_data$total_active_minutes, merge_data$total_minutes_asleep, 
               method = "pearson")
s5

## q5 which day in the week, most sleep, which day least?
## tableau


























