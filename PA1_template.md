# Reproducible Research: Peer Assessment 1
Sunday, February 07, 2016  


## Loading libraries & Reading the data

```r
library(ggplot2) # For Number of Steps
library(lattice) # For Daily Activity Pattern
repData <- read.csv(unz("activity.zip", "activity.csv"), sep=",", na.strings="NA", header = TRUE)
```

## What is the mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
steps.date <- aggregate(steps ~ date, data=repData, FUN=sum) # Sum steps for each day
steps.date$newdate <- as.Date(steps.date$date, format="%Y-%m-%d") # Convert to class date
graph <- ggplot(data=steps.date, aes(x=newdate, y=steps)) + # Bar better than Hist here
      geom_bar(colour="gray", fill="#ff0000", stat="identity") +
      xlab("Date") + ylab("Number of Steps") +
      ggtitle("Number of steps each Day")
graph + scale_x_date(date_labels = "%b %d") # Make dates easily read
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

2. Calculate and report the **mean** and **median** total number of
   steps taken per day


```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data=repData, FUN=mean)
xyplot(steps ~ interval, steps.interval, type = "s",
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(repData))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

I average the 5-minute intervals and use those as fillers for missing values.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
repData <- merge(repData, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(repData$steps)
repData$steps[nas] <- repData$steps.y[nas]
repData <- repData[,c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data=repData, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="Date", ylab="Number of Steps", col=c("red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10766.19
```

There is remarkably little difference between the two estimates, however the overall bias of the data has diminished.
Overall, the input of missing data had small impact.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
day <- weekdays(as.Date(repData$date))
dayType <- vector()
for (i in 1:nrow(repData)) {
    if (day[i] == "Week") {
        dayType[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        dayType[i] <- "Weekend"
    } else {
        dayType[i] <- "Weekday"
    }
}
repData$dayType <- dayType
repData$dayType <- factor(repData$dayType)

stepsByDay <- aggregate(steps ~ interval + dayType, data = repData, mean)
names(stepsByDay) <- c("interval", "dayType", "steps")
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
xyplot(steps ~ interval | dayType, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)
