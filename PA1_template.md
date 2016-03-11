---
title: "Reproducible Research_Course Project 1"
author: "Matias Barrenechea"
date: "March 11th 2016"
output: html_document
---

## Assignement 1

Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
Review criteriamenos
Repo

Valid GitHub URL
At least one commit beyond the original fork
Valid SHA-1
SHA-1 corresponds to a specific commit
Commit containing full submission

Code for reading in the dataset and/or processing the data
Histogram of the total number of steps taken each day
Mean and median number of steps taken each day
Time series plot of the average number of steps taken
The 5-minute interval that, on average, contains the maximum number of steps
Code to describe and show a strategy for imputing missing data
Histogram of the total number of steps taken each day after missing values are imputed
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

## Load and transform data

```{r, echo=T}
setwd("C:/Users/barrenecheam/Desktop/Reproducible REsearch")
library(data.table)
library(ggplot2)

rData = read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))

rData$date = as.Date(rData$date, format = "%Y-%m-%d")
rData$interval = as.factor(rData$interval)
```

## What is mean total number of steps taken per day?
```{r, echo=T}
stepsPerDay = aggregate(steps ~ date, rData, sum)
colnames(stepsPerDay) = c("date","steps")

stepsMean   = mean(stepsPerDay$steps, na.rm=TRUE)
stepsMean 

stepsMedian = median(stepsPerDay$steps, na.rm=TRUE)
stepsMedian
```
## What is the average daily activity pattern?
```{r, echo=T}
stepsPerInterval = aggregate(rData$steps,
                                by = list(interval = rData$interval),
                                FUN=mean, na.rm=TRUE)

stepsPerInterval$interval =
  as.integer(levels(stepsPerInterval$interval)[stepsPerInterval$interval])
colnames(stepsPerInterval) = c("interval", "steps")

maxInterval = stepsPerInterval[which.max(stepsPerInterval$steps),]
maxInterval
```
## Imputing missing values

```{r, echo=T}
missingVals = sum(is.na(rData$steps))

naFill = function(data, pervalue) {
  na_index = which(is.na(data$steps))
  na_replace = unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  fill_steps = data$steps
fill_steps[na_index] = na_replace
  fill_steps
}

rDataFill = data.frame(
  steps = naFill(rData, stepsPerInterval),
  date = rData$date,
  interval = rData$interval)

sum(is.na(rDataFill$steps))

fillStepsPerDay = aggregate(steps ~ date, rDataFill, sum)
colnames(fillStepsPerDay) = c("date","steps")

stepsMeanFill   = mean(fillStepsPerDay$steps, na.rm=TRUE)
stepsMedianFill = median(fillStepsPerDay$steps, na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?

```{r, echo=T}
weekdaysSteps = function(data) {
weekdaysSteps = aggregate(data$steps, by=list(interval = data$interval),
FUN=mean, na.rm=T)
# convert to integers for plotting
weekdaysSteps$interval =
as.integer(levels(weekdaysSteps$interval)[weekdaysSteps$interval])
colnames(weekdaysSteps) = c("interval", "steps")
weekdaysSteps
}

dataByWeekdays = function(data) {
data$weekday =
as.factor(weekdays(data$date)) # weekdays
weekendData = subset(data, weekday %in% c("sábado","domingo"))
weekdayData = subset(data, !weekday %in% c("sábado","domingo"))

weekendSteps = weekdaysSteps(weekendData)
weekdaySteps = weekdaysSteps(weekdayData)

weekendSteps$dayofweek = rep("weekend", nrow(weekendSteps))
weekdaySteps$dayofweek = rep("weekday", nrow(weekdaySteps))

dataByWeekdays = rbind(weekendSteps, weekdaySteps)
dataByWeekdays$dayofweek = as.factor(dataByWeekdays$dayofweek)
dataByWeekdays
}

data_weekdays = dataByWeekdays(rDataFill)
```

## Plots

```{r, echo=T,fig.show=T}
ggplot(stepsPerDay, aes(x = steps)) +
       geom_histogram(fill = "gray", binwidth = 1000) +
        labs(title="Histogram of Steps per Day",
             x = "Steps per day", y = "Number of times in a day") + theme_bw()
```

```{r, echo=T}
ggplot(stepsPerInterval, aes(x=interval, y=steps)) +
  geom_line(color="gray") +
  labs(title="Average Daily Activity Pattern", x="5 minute intervals", y="steps") +
  theme_bw()
```

```{r, echo=T}
ggplot(fillStepsPerDay, aes(x = steps)) +
  geom_histogram(fill = "gray", binwidth = 1000) +
  labs(title="Histogram of Steps per Day",
       x = "Steps per Day", y = "Number of times in a day") + theme_bw()
```

```{r, echo=T}
ggplot(data_weekdays, aes(x=interval, y=steps)) +
geom_line(color="gray") +
facet_wrap(~ dayofweek, nrow=2, ncol=1) +
labs(x="Interval", y="Number of steps") +
theme_bw()
```
