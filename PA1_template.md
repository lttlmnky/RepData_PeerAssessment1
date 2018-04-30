---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
###load packages
```{}
knitr::opts_chunk$set(echo = TRUE)

library(timeDate)
library(plyr)
library(dplyr)
library(data.table)
```
## Loading and preprocessing the data
-Read file
-Add "time" column with converted "interval" column values to time format
-Add "day" column to display weekday
-Add "wday" logical column for weekend or not

```{}
data <- read.csv("activity.csv")
data$day <- weekdays(as.Date(data$date))
data$subset_key <- paste(data$day, data$interval, sep="_")
```

# Assignment Questions
## What is mean total number of steps taken per day?
### Calculate total steps, mean and median steps per day
```{}
steps_day <- group_by(data, date)
steps_sum <- summarize(steps_day, day_sum = sum(steps, na.rm = TRUE))
steps_sum_mean <- mean(steps_sum$day_sum)
steps_sum_median <- median(steps_sum$day_sum)
```

### Histogram of total steps per day
```{}
hist(steps_sum$day_sum, breaks = 10, col = "green", 
     main="Histogram of Total Steps Per Day", xlab = "Total Steps Per Day")
```

### Mean of total steps per day
```{}
steps_sum_mean
```

### Median of total steps per day
```{}
steps_sum_median
```

## What is the average daily activity pattern?
### plot of steps taken by intervals
```{}
steps_interval <- group_by(data, interval)
steps_by_interval <- summarize(steps_interval, interval_mean = mean(steps, na.rm = TRUE))
plot(steps_by_interval$interval, steps_by_interval$interval_mean, main="Average Steps Per 5 Minute Interval",
     xlab="5 Minute Time Intervals", ylab="Average Steps", col = "red", type = "l")
```

###Maximum average steps by interval
```{}
step_interval_max <- steps_by_interval[which.max(steps_by_interval$interval_mean),]
step_interval_max$interval
```

## Imputing missing values
-Total number of rows with NAs
```{}
sum(is.na(data$steps))
```

-Create subset Data Frame for NA rows
```{}
data_NA <- data[is.na(data$steps),]
```

-Create subset Data Frame excluding NA rows
```{}
data_xNA <- step_data[!is.na(data$steps),]
```

-Calculate imputed value as average steps per day with intervals
```{}
step_impute <- group_by(data_xNA, subset_key)
step_impute_mean <- summarize(step_impute, impute_val = mean(steps, na.rm = TRUE))
```

-Substitute imputed value for NAs based on day/interval key
```{}
new_data <- merge(data_NA[,c("date", "interval" , "day", "subset_key")], step_impute_mean, by= "subset_key")
```

-Combine imputed xNA and NA to create new Data Frame
```{}
colnames(new_data)[5] <- "steps"
new_data <- new_data[c(5,2,3,4,1)]
new_data <- rbind(new_data, data_xNA)
```

-Histogram of total steps per day
```{}
new_steps_day <- group_by(new_data, date)
new_steps_sum <- summarize(new_steps_day, new_day_sum = sum(steps))
hist(new_steps_sum$new_day_sum, breaks = 10, col = "blue", main="Histogram of Total Steps Per Day (Imputed)", xlab = "Total Steps Per Day (Imputed)")
```

-The mean and median total steps per day
_Mean_
```{}
new_steps_sum_mean <- mean(new_steps_sum$new_day_sum)
new_steps_sum_mean
```
_Median_
```{}
new_steps_sum_median <- median(new_steps_sum$new_day_sum)
new_steps_sum_median
```

-Differences from before adn after imputing values mean and median increased
_Mean steps per day difference_
```{}
new_steps_sum_mean - steps_sum_mean
```
_Median steps per day difference_
```{}
new_steps_sum_median - steps_sum_median
```

## Are there differences in activity patterns between weekdays and weekends?
-Add weekday/weekend factor
```{}
new_data$wday = ifelse(new_data$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
new_data$wday <- as.factor(new_data$wday)
```

-Panel plot for weekday vs. weekend activity
```{}
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
## weekday plot
new_step_data_week <- filter(new_data, wday == "weekday")
new_steps_interval_week <- group_by(new_step_data_week, interval)
new_steps_by_interval_week <- summarize(new_steps_interval_week, new_interval_mean_week = mean(steps))
plot(new_steps_by_interval_week$interval, new_steps_by_interval_week$new_interval_mean_week,
     main="Average Weekday Steps", xlab="5 Minute Time Interval", ylab="Average Steps", 
     col = "red", type = "l")
## weekend plot
new_step_data_wend <- filter(new_data, wday == "weekend")
new_steps_interval_wend <- group_by(new_step_data_wend, interval)
new_steps_by_interval_wend <- summarize(new_steps_interval_wend, new_interval_mean_wend = mean(steps))
plot(new_steps_by_interval_wend$interval, new_steps_by_interval_wend$new_interval_mean_wend,
     main="Average Weekend Steps", xlab="5 Minute Time Interval", ylab="Average Steps", 
     col = "blue", type = "l")
```
