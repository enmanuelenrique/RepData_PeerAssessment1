# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format    suitable for your analysis



```r
library(lubridate)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.1
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.1
```

```r
activity <- read.csv("activity.csv")
activity <- na.omit(activity)
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
activity$date <- ymd(activity$date)
qplot(date, data = activity, geom = "histogram", weight = steps, binwidth = 1, col =I("red"), ylab = "Steps", main = "Steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken per day


```r
day_steps <- aggregate(steps ~ date, activity, sum )
mean(day_steps$steps)
```

```
## [1] 10766.19
```

```r
median(day_steps$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
 interval_steps <- aggregate(steps ~ interval, activity, mean)
plot(interval_steps, type = "l", main = "Average Step by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_steps[which.max(interval_steps$steps),1]
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
activity <- read.csv("activity.csv")
activity$date <- ymd(activity$date)
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
avrg_interval <- aggregate(steps ~ interval, activity, mean)
activity <- merge(activity, avrg_interval, by = "interval")
imputed_activity <- activity %>% mutate( steps.x = ifelse(is.na(steps.x), steps.y, steps.x))
qplot(date, data = imputed_activity, geom = "histogram", weight = steps.x, binwidth = 1, col =I("red"), ylab = "Steps", main = "Steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
 day_steps_2 <- aggregate(steps.x ~ date, imputed_activity, sum)
mean(day_steps_2$steps.x)
```

```
## [1] 10766.19
```

```r
median(day_steps_2$steps.x)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.




```r
activity_weekday <- mutate(imputed_activity, weekday = factor(1 * (wday(imputed_activity$date) == 7 | wday(imputed_activity$date) == 1), labels = c("weekday", "weekend") ))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).



```r
week_interval <- activity_weekday %>% group_by(weekday, interval) %>% summarise(steps = mean(steps.x))
qplot(interval, steps, data = week_interval, facets = weekday~., geom = "line", main = "Average steps by intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->






