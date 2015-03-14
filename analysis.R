library(dplyr)
library(gridExtra)
library(ggplot2)

setwd('~/r_projects//RepData_PeerAssessment1')
rawData <- read.csv('activity.csv')


# Get only completed cases
completeData <- rawData[complete.cases(rawData), ]


# Mean total number of steps taken per day
completeData.totalStepsByDay <- completeData %>% group_by(date) %>% summarize(totalSteps=sum(steps))
hist(completeData.totalStepsByDay$totalSteps)
mean(completeData.totalStepsByDay$totalSteps)
median(completeData.totalStepsByDay$totalSteps)


# Average daily activity pattern
completeData.avgStepsByInterval <- completeData %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
with(completeData.avgStepsByInterval, plot(interval, avgSteps, type="l"))
maxIndex <- which(completeData.avgStepsByInterval$avgSteps == max(completeData.avgStepsByInterval$avgSteps))
completeData.avgStepsByInterval[maxIndex, ]$interval


# Inputing missing values
sum(!complete.cases(data))

medianStepsByInterval <- completeData %>% group_by(interval) %>% summarize(medianSteps=median(steps))
imputedData <- rawData
naRowIndexes <- which(is.na(rawData$steps))
imputedData[naRowIndexes, ]$steps <- medianStepsByInterval$medianSteps

imputedData.totalStepsByDay <- imputedData %>% group_by(date) %>% summarize(totalSteps=sum(steps))
hist(imputedData.totalStepsByDay$totalSteps)
mean(imputedData.totalStepsByDay$totalSteps)
median(imputedData.totalStepsByDay$totalSteps)


# Weekdays and Weekends
isWeekday <- function(date) {
  weekends <- c("Saturday", "Sunday")
  date <- as.POSIXct(date)
  
  dayType <- "Weekday"
  if(weekdays(date) %in% weekends) { dayType <- "Weekend" }
  dayType
}

dayTypeData <- imputedData
dayTypeData$dayType <- factor(sapply(dayTypeData$date, isWeekday))
dayTypeData.weekday <- dayTypeData %>% filter(dayType == 'Weekday') %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
dayTypeData.weekend <- dayTypeData %>% filter(dayType == 'Weekend') %>% group_by(interval) %>% summarize(avgSteps=mean(steps))

p1 <- qplot(interval, avgSteps, data=dayTypeData.weekday, geom=c("line"), main="Weekday", ylab="Number of Steps")
p2 <- qplot(interval, avgSteps, data=dayTypeData.weekend, geom=c("line"), main="Weekend", ylab="Number of Steps")
grid.arrange(p1, p2, ncol=1)