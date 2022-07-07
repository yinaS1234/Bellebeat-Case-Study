# CASE STUDY: Bellabeat Data Analysis-Google Data Analytics Capstone
##### Author: Yina Qiao

##### Date: July 6, 2022

##### [Tableau Dashboard](https://public.tableau.com/app/profile/yina7051/viz/BellabeatCaseStudy_16567238920690/Dashboard1#1)

##### [Tableau Story Presentation to Skateholders](https://public.tableau.com/app/profile/yina7051/viz/BellabeatCaseStudy_16567238920690/Story1)

Langauage used: R


_The case study follows the six steps of data analysis process:_

### [Ask](#1-ask)
### [Prepare](#2-prepare)
### [Process](#3-process)
### [Analyze](#4-analyze)
### [Share](#5-share)
### [Act](#6-act)

## Introduction
Bellabeat, a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the
global smart device market. I've been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. These insights I discover will help guide company marketing strategy.

## 1. Ask

The task is to analyze smart device usage data in order to
reveal new growth opportunities for Bellabeat.By looking at these trends, I can provide recommendations for Bellabeat marketing strategy.

Key Stakeholder:
*Urška Sršen: Bellabeat’s cofounder and Chief Creative Officer
*Sando Mur: Mathematician and Bellabeat’s cofounder; key member of the Bellabeat executive team
*Bellabeat marketing analytics team


## 2. Prepare
###   Data Source
-Publicly available on Kaggle: https://www.kaggle.com/arashnic/fitbit FitBit Fitness Tracker Data consists of 18 CSV files
-Data sources consists of 30 FitBit users fitness tracker data from Mobius
-Generated by respondents from a survey via Amazon Mechanical Turk between 12 March 2016 to 12 May 2016.
-Data collected includes physical activity recorded in minutes, heart rate, sleep monitoring, daily activity and steps

### Data Limitations:

-Data was collected in 2016. Users's daily activities, sleeping habits, diet, food consumptions may not be relevant now

-Only 30 users is available.  Sample sizes equal to or greater than 30 are often considered sufficient for the CLT to hold. For more accurate analysis, a larger sample size is prefered. 

-Data was collected from a survey, unable to ascertain its integrity and accuracy.


### Is the data ROCCC (Reliable, Original, Comprehensive, Current, and Cited)?
A good data source should follow ROCCC guidline.

-Reliable — LOW — Not reliable as it only has 30 users data

-Original — LOW — Third party provider (Amazon Mechanical Turk)

-Comprehensive — LOW — The sample size is small and most data is recorded during certain days of the week

-Current — LOW — Not current, and may not be relevant

-Cited — LOW — Collection party unknown


## 3. Process

This process aims to clean data to make sure it is accurate, relevant, complete and free of outlier by the following steps:

   - Explore and observe data

   - Identify and treat missing, null values and duplicates


   - Transform data — format data type, consistent column names, create one merged file

   - Perform preliminary statistical analysis


### install various R packages
```
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")

library(tidyverse)
library(ggplot2)
library(ggpubr)
```

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



## 4. Analyze
A summary of your analysis






## 5. Share 






## 6. Act

Supporting visualizations and key findings


