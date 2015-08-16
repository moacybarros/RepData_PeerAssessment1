# Reproducible Research: Peer Assessment 1
## Loading libs

```r
library(lattice)
```

## Loading and preprocessing the data 

```r
unzip("activity.zip")
actdata <- read.csv("activity.csv")
actdata$date <- as.Date(actdata$date,"%Y-%m-%d")
head(actdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
totsteps <- tapply(actdata$steps, actdata$date,sum)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(totsteps,col="green",xlab="Total Steps per Day", 
      ylab="Frequency", main="Histogram of Total Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(totsteps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meansteps <- tapply(actdata$steps,actdata$interval,
                                 mean,na.rm=TRUE)
```

2. Plot 5 minute interval and average number of steps taken, across all days.

```r
plot(row.names(meansteps),meansteps,type="l",
     xlab="Time Intervals (5 min)", 
     ylab="Mean number of steps taken (all days)", 
     main="Average Steps Taken",
     col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

3. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_num <- which.max(meansteps)
interval_max_steps <- names(interval_num)
interval_max_steps
```

```
## [1] "835"
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 




```r
num_na_values <- sum(is.na(actdata))
num_na_values 
```

```
## [1] 2304
```

```r
na_indices <-  which(is.na(actdata))
imputed_values <- meansteps[as.character(actdata[na_indices,3])]
names(imputed_values) <- na_indices
for (i in na_indices) {
    actdata$steps[i] = imputed_values[as.character(i)]
}
sum(is.na(actdata)) 
```

```
## [1] 0
```

```r
totsteps <- tapply(actdata$steps, actdata$date,sum)
hist(totsteps,col="red",xlab="Total Steps per Day", 
      ylab="Frequency", main="Histogram of Total Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Note: **refer to the next section for min|max|mean|median calc**


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
days <- weekdays(actdata$date)
actdata$day_type <- ifelse(days == "Saturday" | days == "Sunday", 
                                "Weekend", "Weekday")
meansteps <- aggregate(actdata$steps,
                                    by=list(actdata$interval,
                                            actdata$day_type),mean)
names(meansteps) <- c("interval","day_type","steps")
xyplot(steps~interval | day_type, meansteps,type="l",
       layout=c(1,2),xlab="Interval",ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
Computing mean, median, max and min for each step across all intervals and days by Weekdays/Weekends

```r
tapply(meansteps$steps,meansteps$day_type,
       function (x) { c(MINIMUM=min(x),MEAN=mean(x),
                        MEDIAN=median(x),MAXIMUM=max(x))})
```

```
## $Weekday
##   MINIMUM      MEAN    MEDIAN   MAXIMUM 
##   0.00000  35.61058  25.80314 230.37820 
## 
## $Weekend
##   MINIMUM      MEAN    MEDIAN   MAXIMUM 
##   0.00000  42.36640  32.33962 166.63915
```
