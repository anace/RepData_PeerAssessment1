---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
data <- read.csv(unzip("activity.zip"))
names(data)
```

```
## [1] "steps"    "date"     "interval"
```


## What is mean total number of steps taken per day?

```r
sdate <- split(data, data$date)
totalstepsbydate <- sapply(sdate, function(x) sum(x[,"steps"]))
hist(totalstepsbydate, main="Histogram of total daily steps", xlab = "total daily steps")
```

![plot of chunk calculatemean](figure/calculatemean-1.png) 

```r
summary(totalstepsbydate, na.rm = TRUE)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
mean(totalstepsbydate, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalstepsbydate, na.rm = TRUE)
```

```
## [1] 10765
```
The mean is slightly larger than the median, showing a small degree of skewness.

## What is the average daily activity pattern?

```r
 sinterval <- split(data, data$interval)
 averagebyinterval<- sapply(sinterval, function(x) mean(x[, "steps"], na.rm = TRUE))
 plot(averagebyinterval, type = "l", main="Average steps by interval", xlab= "5-minute interval", ylab="average steps")
```

![plot of chunk dailypattern](figure/dailypattern-1.png) 

```r
 maxstepsday <- max(averagebyinterval)
 rounded <-round(maxstepsday, digits=2)
 interval <- names(which(averagebyinterval == maxstepsday))
 if(nchar(interval) == 3) time <- paste(substr(interval,1,1), substr(interval, 2, 3), sep = ":")
 if(nchar(interval) == 4) time <- paste(substr(interval,1,2), substr(interval, 3, 4), sep = ":")
```
The highest average number of steps by interval is 206.17 and it happens in the 835 interval, i.e. the interval that starts at 8:35.

## Imputing missing values

```r
missing <- !complete.cases(data)
totalmissing <- sum(missing)
```
There are 2304 rows with missing data.
The missing values will be replaced by the average-by-interval values obtained when calculating the average daily activity pattern. The new values will be stored in a dataframe called ```newvalues```. The dataset without missing values will be called ```newdata```.


```r
newdata <- data
newvalues <- data.frame(average = averagebyinterval, name = names(averagebyinterval), row.names = NULL)
names(newvalues)
```

```
## [1] "average" "name"
```

```r
##This is the block of code that replace missing values by the average-by-interval values.
for (i in 1:sum(missing)){
	if(newdata[missing, "interval"][i] %in% newvalues$name){
		newdata[missing, "steps"][i] <-
		newvalues$average[which(newvalues$name == newdata[missing,"interval"][i])]
		}
	}
```


```r
sdate2 <- split(newdata, newdata$date)
totalstepsbyday <- sapply(sdate2, function(x) sum(x[,"steps"]))
hist(totalstepsbyday, main="Histogram of total daily steps", xlab = "total steps by day")
```

![plot of chunk calculationswithnewdata](figure/calculationswithnewdata-1.png) 

```r
mean(totalstepsbyday)
```

```
## [1] 10766.19
```

```r
median(totalstepsbyday)
```

```
## [1] 10766.19
```
When replacing missing values by the average-by-interval values the mean and the median become equal, which means the distribution becomes symmetric.


## Are there differences in activity patterns between weekdays and weekends?

```r
newdata$weekday <- weekdays(as.Date(newdata$date))
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
logicalWeekday <- newdata$weekday %in% weekday
weekdaydata <- newdata[logicalWeekday,]
weekenddata <- newdata[!logicalWeekday,]
s1 <- split(weekdaydata, weekdaydata$interval)
s2 <- split(weekenddata, weekenddata$interval)
weekdaypattern<- sapply(s1, function(x) mean(x[, "steps"]))
weekendpattern<- sapply(s2, function(x) mean(x[, "steps"]))
par(mfrow = c(2,1))
plot(weekdaypattern, type = "l", main="Steps by interval on weekdays", xlab= "5-minute interval", ylab="average steps")
plot(weekendpattern, type = "l", main="Steps by interval on weekends", xlab= "5-minute interval", ylab="average steps")
```

![plot of chunk weekdayactivitypatterns](figure/weekdayactivitypatterns-1.png) 

There is a difference in the activity pattern between weekdays and weekends. The weekday pattern looks very much like the average pattern, i.e. more steps in the morning, while the weekend pattern shows the steps more distributed along the day.
