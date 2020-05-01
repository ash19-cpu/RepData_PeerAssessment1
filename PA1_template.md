# Course Project 1
======================

## Loading and preprocessing the data

```{r}
data <- read.csv("activity.csv")
summary(data)
head(data)
```

## 1. What is mean total number of steps taken per day?

```{r}
totsteps <- aggregate(steps~date, data, sum)
hist(totsteps$steps, xlab="Steps per day", ylab="Number of days", main="Total Number of Steps taken each day")

m <- mean(totsteps$steps)
m

medianSteps <- median(totsteps$steps)
medianSteps
```

## 2. What is the average daily activity pattern?

```{r}
intervalSteps <- aggregate(steps~interval, data, mean, na.rm=TRUE)
plot(steps~interval, data=intervalSteps, type="l")

maxSteps <- intervalSteps[which.max(intervalSteps$steps),]$interval
maxSteps
```

## 3. Imputing Missing Values

```{r}
missingSteps <- sum(is.na(data$steps))
missingSteps

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

## 4. Are there differences in activity pattern between weekdays and weekends?

```{r}
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
