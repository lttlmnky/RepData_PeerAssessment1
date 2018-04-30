---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
###load packages

```r
knitr::opts_chunk$set(echo = TRUE)
install.packages("timeDate", repos = "http://cran.us.r-project.org")
```

```
## Installing package into 'C:/Users/hourui/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'timeDate' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\hourui\AppData\Local\Temp\RtmpeC7rj0\downloaded_packages
```

```r
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.4.4
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.4.4
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```
## Loading and preprocessing the data
*Read file*
*Add "time" column with converted "interval" column values to time format*
*Add "day" column to display weekday*
*Add "wday" logical column for weekend or not*


```r
data <- read.csv("activity.csv")
data$day <- weekdays(as.Date(data$date))
data$subset_key <- paste(data$day, data$interval, sep="_")
```

# Assignment Questions
## What is mean total number of steps taken per day?
### Calculate total steps, mean and median steps per day

```r
steps_day <- group_by(data, date)
steps_sum <- summarize(steps_day, day_sum = sum(steps, na.rm = TRUE))
steps_sum_mean <- mean(steps_sum$day_sum)
steps_sum_median <- median(steps_sum$day_sum)
```

### Histogram of total steps per day

```r
hist(steps_sum$day_sum, breaks = 10, col = "green", main="Histogram of Total Steps Per Day", xlab = "Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Mean of total steps per day

```r
steps_sum_mean
```

[1] 9354.23

### Median of total steps per day

```r
steps_sum_median
```

[1] 10395

## What is the average daily activity pattern?
### plot of steps taken by intervals

```r
steps_interval <- group_by(data, interval)
steps_by_interval <- summarize(steps_interval, interval_mean = mean(steps, na.rm = TRUE))
plot(steps_by_interval$interval, steps_by_interval$interval_mean, main="Average Steps Per 5 Minute Interval", xlab="5 Minute Time Intervals", ylab="Average Steps", col = "red", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

###Maximum average steps by interval

```r
step_interval_max <- steps_by_interval[which.max(steps_by_interval$interval_mean),]
step_interval_max$interval
```

[1] 835

## Imputing missing values
*Total number of rows with NAs*

```r
sum(is.na(data$steps))
```

[1] 2304

*Create subset Data Frame for NA rows*

```r
data_NA <- data[is.na(data$steps),]
```

*Create subset Data Frame excluding NA rows*

```r
data_xNA <- data[!is.na(data$steps),]
```

*Calculate imputed value as average steps per day with intervals*

```r
step_impute <- group_by(data_xNA, subset_key)
step_impute_mean <- summarize(step_impute, impute_val = mean(steps, na.rm = TRUE))
```

*Substitute imputed value for NAs based on day/interval key*

```r
new_data <- merge(data_NA[,c("date", "interval" , "day", "subset_key")], step_impute_mean, by= "subset_key")
```

*Combine imputed xNA and NA to create new Data Frame*

```r
colnames(new_data)[5] <- "steps"
new_data <- new_data[c(5,2,3,4,1)]
new_data <- rbind(new_data, data_xNA)
```

*Histogram of total steps per day*

```r
new_steps_day <- group_by(new_data, date)
new_steps_sum <- summarize(new_steps_day, new_day_sum = sum(steps))
hist(new_steps_sum$new_day_sum, breaks = 10, col = "blue", main="Histogram of Total Steps Per Day (Imputed)", xlab = "Total Steps Per Day (Imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

*The mean and median total steps per day*
_Mean_

```r
new_steps_sum_mean <- mean(new_steps_sum$new_day_sum)
new_steps_sum_mean
```

[1] 10821.21
_Median_

```r
new_steps_sum_median <- median(new_steps_sum$new_day_sum)
new_steps_sum_median
```

[1] 11015

*Differences from before adn after imputing values mean and median increased*
_Mean steps per day difference_

```r
new_steps_sum_mean - steps_sum_mean
```

[1] 1466.98
_Median steps per day difference_

```r
new_steps_sum_median - steps_sum_median
```

[1] 620

## Are there differences in activity patterns between weekdays and weekends?

```r
new_data$date <- as.Date(new_data$date)
MonToFri <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new_data$weekDay <- factor((weekdays(new_data$date) %in% MonToFri), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
avgStepInterval <- aggregate(steps ~ interval+weekDay, new_data, mean)
ggplot(avgStepInterval, aes(x=interval, y=steps, color=weekDay))+ geom_line() + facet_wrap(~weekDay, ncol=1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
