---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
# Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA  
date: The date on which the measurement was taken in YYYY-MM-DD format  
interval: Identifier for the 5-minute interval in which measurement was taken  

First unzip and read the data. Also change the date variable from char to date class.
## Loading and preprocessing the data

```r
unzip("activity.zip")
actdata<-read.csv("activity.csv")
actdata[,"date"]<-as.Date(actdata[,"date"])
```

## What is mean total number of steps taken per day?

```r
sumsteps<-aggregate(steps ~ date, data = actdata, sum, na.rm = TRUE)

hist(sumsteps$steps,main = "Total number of Steps Per Day",xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(sumsteps$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sumsteps$steps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
meaninterval<-aggregate(steps ~ interval, data = actdata, mean, na.rm = TRUE)
plot(meaninterval$interval,meaninterval$steps,type = "l",main = "Average number of steps by interval",xlab = "Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxind<-which(max(meaninterval$steps)==meaninterval$steps)

meaninterval[maxind,]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
The NA values will me replace with the mean of the total number of steps per day

```r
sum(is.na(actdata))
```

```
## [1] 2304
```

```r
actdatanew<-data.frame(actdata)
actdatanew[is.na(actdatanew)] <- mean(sumsteps$steps,na.rm=TRUE)/288

sumstepsnew<-aggregate(steps ~ date, data = actdatanew, sum, na.rm = TRUE)

hist(sumstepsnew$steps,main = "Total number of Steps Per Day",xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(sumstepsnew$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sumstepsnew$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```
The means are the same and the median change.  
  
## Are there differences in activity patterns between weekdays and weekends?

```r
weekdayfactor<-ifelse(weekdays(actdatanew$date) %in% c("sábado","domingo"),
"weekend","weekday")

meanweekday <- aggregate(steps ~ interval + weekdayfactor, actdatanew, mean)

library(lattice)
xyplot(steps ~ interval | weekdayfactor, meanweekday, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
