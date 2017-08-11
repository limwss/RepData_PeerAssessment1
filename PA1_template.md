---
title: "Reproducible Research Assignment 1"
output:
  word_document: default
  html_document: default
---



## R Markdown



##Loading and pre-processing the data

1. Load the data (i.e. read.csv())


```r
# Load the raw activity data
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)
```


2. Process/transform the data (if necessary) into a format suitable for analysis

```r
## read the dataset 	

# Transform the date attribute to an actual date format
activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

# Compute the weekdays from the date attribute
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

# Compute the day type (weekend or weekday)
# for answering final question too, weekday and weekend as day type
activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                     activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))

# Create the final data.frame
activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)


head(activity)
```

```
##         date weekday daytype interval steps
## 1 2012-10-01  monday weekday        0    NA
## 2 2012-10-01  monday weekday        5    NA
## 3 2012-10-01  monday weekday       10    NA
## 4 2012-10-01  monday weekday       15    NA
## 5 2012-10-01  monday weekday       20    NA
## 6 2012-10-01  monday weekday       25    NA
```
##What is mean total number of steps taken per day?

For this part of the assignment, the missing values are ignored in the dataset.

1. Calculate the total number of steps taken per day


```r
		steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
		colnames(steps_per_day) <- c("date","steps")
		head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

	2. Make a histogram of the total number of steps taken each day
	

```r
hist(steps_per_day$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```

![plot of chunk histogram of no of steps](figure/histogram of no of steps-1.png)

The mean and median :


```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```


##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
 avg_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

# Rename the attributes
names(avg_data) <- c("interval", "mean")


#plot

plot(avg_data$interval, 
     avg_data$mean, 
     type="l", 
     col="orange", 
     lwd=2, 
     xlab="Interval in minutes", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

![plot of chunk time series plot](figure/time series plot-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#max no of steps
   average_steps <- aggregate(steps ~ interval, data = activity, FUN = mean)
   max_interval <- average_steps[which.max(average_steps$steps),]
   max_interval 
```

```
##     interval    steps
## 104      835 206.1698
```
the 835 interval, on average across all the days in the dataset, contains the maximum number of steps.



##Imputing the missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's)

1. Total number of missing values:


```r
    missing_val <- sum(is.na(activity))
    missing_val
```

```
## [1] 2304
```

2.. Devise a strategy for filling in all of the missing values in the dataset

```r
    # Find the NA positions
    na_pos <- which(is.na(activity$steps))

    # Create a vector of means
    mean_vc <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))

    # Replace the NAs by the means
    activity[na_pos, "steps"] <- mean_vc
```

Make sure there is no more missing values    


```r
    No_missing_val <- activity
    sum(is.na(No_missing_val$steps))
```

```
## [1] 0
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


1. Calculate total number of steps taken each day after missing values are imputed and plot a histogram


```r
        tot_step <- aggregate(steps ~ date, data = No_missing_val, sum, na.rm = TRUE)
        summary(tot_step)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
        hist(tot_step$steps, main = "Total number of steps taken each day (NAs imputed)", xlab = "day",ylab = "Frequency", col = "orange")
```

![plot of chunk calculate total no of steps no NA](figure/calculate total no of steps no NA-1.png)

The mean and median are computed like


```r
mean(tot_step$steps)
```

```
## [1] 10766.19
```

```r
median(tot_step$steps)
```

```
## [1] 10766.19
```

These formulas gives a mean and median of 10766 and 10766 respectively.

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.


##Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekdays" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# The new factor variable "daytype" was already in the activity data frame
head(activity)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
avg_data <- aggregate(activity$steps, 
                      by=list(activity$daytype, 
                              activity$weekday, activity$interval), mean)

# Rename the attributes
names(avg_data) <- c("daytype", "weekday", "interval", "mean")

head(avg_data)
```

```
##   daytype  weekday interval     mean
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000
```

```r
#create the timeseries plot



xyplot(mean ~ interval | daytype, avg_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![plot of chunk weekend and weekday plot](figure/weekend and weekday plot-1.png)
