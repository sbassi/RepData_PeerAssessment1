---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Loading and preparing the data for analysis


```r
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

Histogram of steps by day


```r
stepsDay = aggregate(steps ~ date, data=activity, FUN=sum)
hist(stepsDay$steps, main="Daily steps", breaks=nrow(stepsDay),
      xlab = "STEPS", ylab= "DAYS", col = "Red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Get mean, formula:


```r
mean(stepsDay[,2])
```

```
## [1] 10766.19
```


Get mediam



```r
median(stepsDay[,2])
```

```
## [1] 10765
```



## What is the average daily activity pattern?


Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)





```r
stepsInterval <- aggregate(steps~interval, data=activity, FUN=mean)
plot(stepsInterval, xlab = "INTERVAL", 
     ylab ="STEPS TAKEN", type ="l", main= "AVERAGE STEPS PER INTERVAL", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max <- stepsInterval[which.max(stepsInterval$steps), ]
interval <- max[,1]
interval
```

```
## [1] 835
```


## Imputing missing values



Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

Missing data in steps in the activity data set.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityTMP <- activity
activityTMP$steps[is.na(activityTMP$steps)] <- stepsInterval$steps
stepsDaywithNA <- aggregate(steps ~ date, data = activityTMP, sum, na.rm = TRUE)
activityCompleted <- activityTMP
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
hist(stepsDaywithNA$steps, breaks = 18)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(stepsDaywithNA$steps)
```

```
## [1] 10766.19
```

```r
median(stepsDaywithNA$steps)
```

```
## [1] 10766.19
```




## Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.



```r
activityCompleted$date <- as.Date(activityCompleted$date,"%Y-%m-%d")
days <- weekdays(activityCompleted$date)
activityCompleted$dayType <- ifelse(days == "Saturday" | days == "Sunday",  "Weekend", "Weekday")
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).




```r
intervalMeansNEW=aggregate(steps~interval+dayType, activityCompleted, mean)
library(lattice)
xyplot(steps~interval|factor(dayType),data = intervalMeansNEW, aspect=1/2, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



