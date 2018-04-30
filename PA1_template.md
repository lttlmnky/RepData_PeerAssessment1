---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
###load packages

```r
knitr::opts_chunk$set(echo = TRUE,results = "asis")

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
h<-hist(steps_sum$day_sum, breaks = 10, col = "green", main="Histogram of Total Steps Per Day", xlab = "Total Steps Per Day")
```

![](_1_PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
print(h)
```

$breaks
 [1]     0  2000  4000  6000  8000 10000 12000 14000 16000 18000 20000
[12] 22000

$counts
 [1] 10  2  3  3  7 16 10  7  1  0  2

$density
 [1] 8.196721e-05 1.639344e-05 2.459016e-05 2.459016e-05 5.737705e-05
 [6] 1.311475e-04 8.196721e-05 5.737705e-05 8.196721e-06 0.000000e+00
[11] 1.639344e-05

$mids
 [1]  1000  3000  5000  7000  9000 11000 13000 15000 17000 19000 21000

$xname
[1] "steps_sum$day_sum"

$equidist
[1] TRUE

attr(,"class")
[1] "histogram"

png 
  2 
png 
  4 
png 
  2 
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
p<-plot(steps_by_interval$interval, steps_by_interval$interval_mean, main="Average Steps Per 5 Minute Interval", xlab="5 Minute Time Intervals", ylab="Average Steps", col = "red", type = "l")
```

![](_1_PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
print(p)
```

NULL

png 
  2 
png 
  5 
png 
  2 

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
h<-hist(new_steps_sum$new_day_sum, breaks = 10, col = "blue", main="Histogram of Total Steps Per Day (Imputed)", xlab = "Total Steps Per Day (Imputed)")
```

![](_1_PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

png 
  2 
png 
  6 
png 
  2 

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
new_data$DayoftheWeek <- ifelse(weekdays(as.Date(new_data$date)) %in% c("Monday","Tuesday","Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
newMeanPerInterval <- new_data %>% na.omit() %>% group_by(interval, DayoftheWeek) %>% summarize(newMeanSteps=mean(steps))
g <- ggplot(newMeanPerInterval, aes(x=interval, y=newMeanSteps)) + geom_line(color="blue")+ facet_grid(DayoftheWeek~.) + xlab("Interval") + ylab("Average Steps") + ggtitle("Average Steps - Comparison between Weekday and Weekend")
print(g)
```

![](_1_PA1_template_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

png 
  2 
png 
  7 
png 
  2 
