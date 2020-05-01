---
output: 
  html_document: 
    keep_md: yes
---
# Course Project 1
======================

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(data)
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

## 1. What is mean total number of steps taken per day?


```r
totsteps <- aggregate(steps~date, data, sum)
hist(totsteps$steps, xlab="Steps per day", ylab="Number of days", main="Total Number of Steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
m <- mean(totsteps$steps)
m
```

```
## [1] 10766.19
```

```r
medianSteps <- median(totsteps$steps)
medianSteps
```

```
## [1] 10765
```

## 2. What is the average daily activity pattern?


```r
intervalSteps <- aggregate(steps~interval, data, mean, na.rm=TRUE)
plot(steps~interval, data=intervalSteps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxSteps <- intervalSteps[which.max(intervalSteps$steps),]$interval
maxSteps
```

```
## [1] 835
```

## 3. Imputing Missing Values


```r
missingSteps <- sum(is.na(data$steps))
missingSteps
```

```
## [1] 2304
```

```r
meanStepsInterval<-function(interval){
    intervalSteps[intervalSteps$interval==interval,]$steps
}

newdata<-data
for(i in 1:nrow(newdata)){
    if(is.na(newdata[i,]$steps)){
        newdata[i,]$steps <- meanStepsInterval(newdata[i,]$interval)
    }
}

newdataSteps <- aggregate(steps~date, newdata, sum)
hist(newdataSteps$steps, xlab="Steps per day", ylab="Number of days", main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 4. Are there differences in activity pattern between weekdays and weekends?


```r
newdata$date <- as.Date(strptime(newdata$date, format ="%Y-%m-%d"))
newdata$day <- weekdays(newdata$date)
for (i in 1:nrow(newdata)) {
    if (newdata[i,]$day %in% c("Saturday","Sunday")) {
        newdata[i,]$day<-"weekend"
    }
    else{
        newdata[i,]$day<-"weekday"
    }
}

stDays <- aggregate(newdata$steps ~ newdata$interval + newdata$day, newdata, mean)

names(stDays) <- c("interval", "day", "steps")
library(lattice)

xyplot(steps ~ interval | day, stDays, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
