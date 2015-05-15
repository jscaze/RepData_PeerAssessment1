---
title: "Reproducible Research: Peer Assessment 1"
author: Jean-Sebastien CAZE
output: 
  html_document:
    keep_md: true
---




```r
opts_chunk$set(echo=TRUE, results="asis")
```

## Preliminary Steps : Configuration of execution envirronment

Set aspects of the local for the R process. (For none American users)

```r
Sys.setlocale(category = "LC_ALL", locale = "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

Initialisation of seed for Random Number Generation.

```r
set.seed(1024)
```

Get information from execution environment:

```r
sessionInfo()
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lattice_0.20-29 plyr_1.8.1      xtable_1.7-4    knitr_1.9      
## 
## loaded via a namespace (and not attached):
## [1] codetools_0.2-8 digest_0.6.8    evaluate_0.6    formatR_1.1    
## [5] grid_3.1.1      Rcpp_0.11.4     stringr_0.6.2   tools_3.1.1
```

Loading libraries utils:

```r
library(knitr)
library(plyr)
library(xtable)
library(datasets)
library(lattice)
```

## Loading and preprocessing the data

First of all, we have to download data file and unzip the data zip file.

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "./ProjectData/activity.zip")
```

```
## Error in download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", : schéma URL non supporté
```

```r
unzip("./ProjectData/activity.zip", files = NULL, list = FALSE, 
      overwrite = TRUE, junkpaths = FALSE, exdir = "./ProjectData", 
      unzip = "internal", setTimes = FALSE)
```

Now we read the CSV file

```r
dfActivity <- read.csv("./ProjectData/activity.csv", sep=",", header=TRUE, fill = TRUE, quote = "\"",na.strings = "NA")
```



Format the column date into a correct format

```r
dfActivity$newdate <- strptime(as.character(dfActivity$date), "%Y-%m-%d")
dfActivity$txtdate <- format(dfActivity$newdate, "%d/%m/%Y")
```


## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```r
dfActAgg <- aggregate(data.frame(sumSteps = dfActivity$steps),
                      by=list(ActivityDate=dfActivity$date),
                      FUN=sum, 
                      na.rm=TRUE)
```




Draw the histogram of the total number of steps taken each day and add the mean to the plot.


```r
hist(dfActAgg$sumSteps,
     xlab = 'number of steps per day',
     ylab = 'Frequency',
     main = 'Frequency of number of Steps per day',
     col='lightblue')
abline(v = mean(dfActAgg$sumSteps), col = "red", lwd = 2)
abline(v = median(dfActAgg$sumSteps), col = "blue", lwd = 2)
legend("topright", c("mean", "median"), lty = c(1, 1), col = c("red", "blue"))
```

![plot of chunk HistogramSumStep](figure/HistogramSumStep-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
meanActivity <- round(mean(dfActAgg$sumSteps, na.rm = TRUE),2)

medianActivity <- round(median(dfActAgg$sumSteps, na.rm = TRUE),2)
```

* Mean of activity : 9 354.23

* Median of activity : 10 395


## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dfActAggInt <- aggregate(data.frame(meanSteps = dfActivity$steps),
                      by=list(ActivityInterval=dfActivity$interval),
                      FUN=mean, 
                      na.rm=TRUE)


plot( 
  x=dfActAggInt$ActivityInterval,
  y=dfActAggInt$meanSteps,
  col="dark blue", 
  type="l",
  main="Average number of steps taken across all days",
  ylab="Mean of Step",
  xlab="Interval") 
abline(v = dfActAggInt[dfActAggInt$meanSteps==max(dfActAggInt$meanSteps),"ActivityInterval"], col = "red", lwd = 2)
legend("topright", c("max"), lty = c(1), col = c("red"))
```

![plot of chunk plot](figure/plot-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#dfActAggInt[dfActAggInt$meanSteps==max(dfActAggInt$meanSteps),"ActivityInterval"]
dfActAggInt[which.max(dfActAggInt$meanSteps),]
```

```
##     ActivityInterval meanSteps
## 104              835  206.1698
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

* total number of records : 

```r
nrow(dfActivity)
```

```
## [1] 17568
```

* number of Missing Values :


```r
nrow(dfActivity[is.na(dfActivity$steps),])
```

```
## [1] 2304
```


detail on missing values : 


```r
dfActNA<-dfActivity[is.na(dfActivity$steps),]

dfActAggCountNA <- count(dfActNA, c('date'))

dfActAggCountAll <- count(dfActivity, c('date'))

dfActAggCount  <- merge(x = dfActAggCountNA, y = dfActAggCountAll, by = "date", all.x = TRUE)

dfActAggCount <- rename(dfActAggCount, c("freq.x"="nbMissingValues", "freq.y"="nbAll"))
```

Missing values are distributed like this : 
<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat May 16 00:22:45 2015 -->
<table border=1>
<tr> <th>  </th> <th> date </th> <th> nbMissingValues </th> <th> nbAll </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-01 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-08 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-11-01 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-11-04 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-11-09 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-11-10 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> 2012-11-14 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> 2012-11-30 </td> <td align="right"> 288 </td> <td align="right"> 288 </td> </tr>
   </table>


Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
runApplyMean <- function(xact) { 
    if(is.na(xact)==TRUE) { 
        t<- meanActivity 
      } 
    else { 
        t<-xact 
      } 
    t
  }

runApplyMedian <- function(xact) { 
    if(is.na(xact)==TRUE) { 
        t<- medianActivity 
      } 
    else { 
        t<-xact 
      } 
    t
  }

dfTinyActivity <- dfActivity

dfTinyActAgg <- aggregate(data.frame(sumSteps = dfTinyActivity$steps),
                      by=list(ActivityDate=dfTinyActivity$date),
                      FUN=sum)

for(i in 1:nrow(dfTinyActAgg)) {
  dfTinyActAgg$sumSteps[i] <- runApplyMean(dfTinyActAgg[i,"sumSteps"])  
  }
```

Histogram of the total number of steps taken each day with NA filled with the mean


```r
hist(dfTinyActAgg$sumSteps,
     xlab = 'number of steps per day',
     ylab = 'Frequency',
     main = 'Frequency of number of Steps per day with NA removed',
     col='lightblue')
abline(v = mean(dfTinyActAgg$sumSteps), col = "red", lwd = 2)
abline(v = median(dfTinyActAgg$sumSteps), col = "blue", lwd = 2)
legend("topright", c("mean", "median"), lty = c(1, 1), col = c("red", "blue"))
```

![plot of chunk HistogramTinySumStep](figure/HistogramTinySumStep-1.png) 

Calculation of the mean and median total number of steps taken per day.


```r
meanTinyActivity <-   round(mean(dfTinyActAgg$sumSteps, na.rm = TRUE),2)

medianTinyActivity <- round(median(dfTinyActAgg$sumSteps, na.rm = TRUE),2)
```

* Mean of activity after suppress NA : 10 581.01

* Median of activity after suppress NA : 10 395

We can see that there is no impact on median
Results are not very different of the first result.
But now mean and median are very close.
And the mean is now greater than the median.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dfTinyActivity$weekday <- as.factor(
                              ifelse((   
                                  weekdays(dfTinyActivity$newdate)=="Saturday"
                                | weekdays(dfTinyActivity$newdate)=="Sunday")
                                  ,"weekend", "weekday" ))
```

Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
dfTinyActAggMeanPlot <- aggregate(data.frame(Numberofsteps = dfTinyActivity$steps),
                      by=list(ActWeekDay=dfTinyActivity$weekday, 
                              Interval=dfTinyActivity$interval),
                      FUN=mean, 
                      na.rm=TRUE)

xyplot( Numberofsteps ~  Interval | ActWeekDay, data = dfTinyActAggMeanPlot,layout=c(1, 2), type="l")
```

![plot of chunk plotWeekEnd](figure/plotWeekEnd-1.png) 
