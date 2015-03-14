# Reproducible Research: Peer Assessment 1
=============================================================================================================================

## Data Analysis on Personal Movements

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks.

****


### Loading and preprocessing the data

This analysis uses the following R libraries:


```r
library(dplyr)
library(ggplot2)
library(gridExtra)
```

First, we read the CSV data into R and then subset of only the completed cases for our analysis:


```r
rawData <- read.csv('activity.csv')
completeData <- rawData[complete.cases(rawData), ]
```

****

### What is mean total number of steps taken per day?

Using `dplyr` library, we group the data by date and then sum the total number of steps taken by date. The mean number of steps taken per day is shown in the histogram generated below:


```r
completeData.totalStepsByDay <- completeData %>% group_by(date) %>% summarize(totalSteps=sum(steps))
hist(
  completeData.totalStepsByDay$totalSteps,
  main="Distribution of Total Steps per Day",
  xlab="Total Number of Steps taken per Day"
)
```

![](PA1_template_files/figure-html/completeCasesHist-1.png) 


The mean and median total number of steps taken per day are 10766.19 and 10765 respectively.

****

### What is the average daily activity pattern?

To find out the average daily activity pattern, the data is grouped by `interval` this time. A times series plot of the activity pattern is shown below:


```r
completeData.avgStepsByInterval <- completeData %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
with(completeData.avgStepsByInterval, plot(interval, avgSteps, type="l"))
```

![](PA1_template_files/figure-html/avgActivity-1.png) 




Comparing the average acitivity across all the days, the 5-minute interval 835 contains the maximum number of steps.

****

### Imputing missing values

The total number of missing values is:


```r
sum(!complete.cases(rawData))
```

```
## [1] 2304
```

In order to deal with the missing steps for some rows, we will replace the missing value with median steps obtained from that same interval. This enables us to obtain a whole number for the missing value, which would be close to what we expect for that missing data point.

Here we create a new dataset with the strategy mentioned above.


```r
medianStepsByInterval <- completeData %>% group_by(interval) %>% summarize(medianSteps=median(steps))
imputedData <- rawData
naRowIndexes <- which(is.na(rawData$steps))
imputedData[naRowIndexes, ]$steps <- medianStepsByInterval$medianSteps
```


Using the imputed data, a new histogram is plotted below:


```r
imputedData.totalStepsByDay <- imputedData %>% group_by(date) %>% summarize(totalSteps=sum(steps))
hist(
  imputedData.totalStepsByDay$totalSteps,
  main="Distribution of Total Steps per Day",
  xlab="Total Number of Steps taken per Day"
)
```

![](PA1_template_files/figure-html/imputedDataHist-1.png) 



The mean and median total number of steps taken per day are 9503.87 and 10395 respectively. Comparing with the previous results, we see that the mean steps have changed by 1262.32 and the median steps have shifted by 370. 

****

### Are there differences in activity patterns between weekdays and weekends?


To analysis if there are any difference between weekdays and weekends, a new data set with an additional factor variable `dayType` is introduced to indicate whether a day is a `Weekday` or a `Weekend`.


```r
isWeekday <- function(date) {
  weekends <- c("Saturday", "Sunday")
  date <- as.POSIXct(date)
  dayType <- "Weekday"
  if(weekdays(date) %in% weekends) { dayType <- "Weekend" }
  dayType
}

dayTypeData <- imputedData
dayTypeData$dayType <- factor(sapply(dayTypeData$date, isWeekday))
```

Then, split the data set into 2 sets for weekday and weekends, and summarize the data:


```r
dayTypeData.weekday <- dayTypeData %>% filter(dayType == 'Weekday') %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
dayTypeData.weekend <- dayTypeData %>% filter(dayType == 'Weekend') %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
```

The plot below shows 2 activity charts:

```r
p1 <- qplot(interval, avgSteps, data=dayTypeData.weekday, geom=c("line"), main="Weekday", ylab="Number of Steps")
p2 <- qplot(interval, avgSteps, data=dayTypeData.weekend, geom=c("line"), main="Weekend", ylab="Number of Steps")
grid.arrange(p1, p2, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

From the graph, we find that the subject has different activity patterns during weekdays and weekends.

****



