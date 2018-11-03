
---
title: "PA1_Template"
output:
  html_document:
    keep_md: true
---

# Reproducible Research: Peer Assessment 1

## Introduction
This is the first peer assessment for the Coursera course Reproducible Research.

#### Required packages

```r
library(tidyverse)
library(lubridate)
library(rmarkdown)
library(knitr)
```



## Loading and preprocessing the data
#### Load the Data

```r
activity <- read.csv(file="activity.csv", header=TRUE)
```
#### Understand the Data

```r
head(activity, 2)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
#### Transform the data for analysis

```r
tot_steps <- activity %>% group_by(date) %>% filter(!is.na(steps)) %>%
      summarize(tot_steps = sum(steps, na.rm = TRUE))
```

#### What is the mean total number of steps taken per day
#### and the median total number of steps taken per day?

```r
summary(tot_steps$tot_steps) ##  or 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
avg_steps <- mean(tot_steps$tot_steps, na.rm = TRUE)
median_steps <- median(tot_steps$tot_steps, na.rm = TRUE)
```
#### Make a histogram of the total number of steps taken per day

```r
ggplot(tot_steps, aes(tot_steps)) + geom_histogram(binwidth = 2500, fill = "darkgreen", col = "lightgreen") +
      labs(title = "Daily Steps Histogram", x = "Total Steps per Day", y = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## What is the average daily activity pattern?
#### Transform the data for analysis

```r
step_int <- activity %>% group_by(interval) %>% filter(!is.na(steps)) %>%
      summarize(int_steps = mean(steps, na.rm = TRUE))
head(step_int, 2)
```

```
## # A tibble: 2 x 2
##   interval int_steps
##      <int>     <dbl>
## 1        0     1.72 
## 2        5     0.340
```

```r
str(step_int)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	288 obs. of  2 variables:
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ int_steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
summary(step_int)
```

```
##     interval        int_steps      
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

#### Make a time series plot of the data

```r
ggplot(step_int, aes(interval, int_steps)) + geom_line() +
      labs(title = "Daily Average Activity", x = "Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


## Interval with most steps
#### Calculate which interval has the highest average steps per day

```r
step_int[which.max(step_int$int_steps), ] # 835 which means 8:35-8:40
```

```
## # A tibble: 1 x 2
##   interval int_steps
##      <int>     <dbl>
## 1      835      206.
```

## Imputing missing values
#### Summary of missing values

```r
colSums(is.na(activity)) # 2304 missing steps values, 0 missing date values, 0 missing interval values
```

```
##    steps     date interval 
##     2304        0        0
```

## Add mean value for that interval into original dataset replacing NA values
#### Transform the data for analysis

```r
fj_int <- activity
fj_int_steps <- full_join(fj_int, step_int, by = 'interval')
nas <- is.na(fj_int$steps)
fj_int_steps[nas,]$steps <- fj_int[nas, ]$int_steps
```

```
## Warning in `[<-.factor`(`*tmp*`, iseq, value = c(0L, 5L, 10L, 15L, 20L, :
## invalid factor level, NA generated
```

```r
filled_int_steps <- select(fj_int_steps, steps = steps, date, interval)
filled_int_steps_date <- filled_int_steps %>% group_by(date)
head(filled_int_steps_date, 5)
```

```
## # A tibble: 5 x 3
## # Groups:   date [1]
##   steps date  interval
##   <int> <fct>    <dbl>
## 1     1 <NA>    1.72  
## 2     1 <NA>    0.340 
## 3     1 <NA>    0.132 
## 4     1 <NA>    0.151 
## 5     1 <NA>    0.0755
```

```r
colSums(is.na(filled_int_steps_date))
```

```
##    steps     date interval 
##        0     2304        0
```

```r
filled_tot_steps <- summarize(filled_int_steps_date, total_steps = sum(steps))
```

#### Plot the daily steps data

```r
ggplot(filled_tot_steps, aes(total_steps)) + geom_histogram(binwidth = 2500, fill = "darkblue", col = "lightblue") +
      labs(title = "Daily Steps Histogram", x = "Total Steps per Day", y = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
summary(filled_tot_steps)
```

```
##          date     total_steps   
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8860  
##  2012-10-04: 1   Median :10890  
##  2012-10-05: 1   Mean   :11969  
##  2012-10-06: 1   3rd Qu.:13412  
##  (Other)   :48   Max.   :75744  
##  NA's      : 1
```

## Are there differences in activity patterns between weekdays and weekends?
#### Transform the data for analysis

```r
filled_int_steps_date <- mutate(filled_int_steps_date, weekend = factor(weekdays(ymd(date)) %in% c('Saturday', 'Sunday')))
filled_tot_steps_wk <- group_by(filled_int_steps_date, weekend, interval)
filled_tot_steps_wk_int <- summarize(filled_tot_steps_wk, steps = mean(steps))
```

#### Plots for weekend vs weeekday steps

```r
gplot1 <- filled_tot_steps_wk_int %>% filter(weekend == TRUE)
ggplot(gplot1, aes(interval, steps)) + geom_line() + labs(title = "Weekends", x = "Average Interval Steps per Day",
                                                          y = "# of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
gplot2 <- filled_tot_steps_wk_int %>% filter(weekend == FALSE)
ggplot(gplot2, aes(interval, steps)) + geom_line() + labs(title = "Weekdays", x = "Average Interval Steps per Day",
                                                          y = "# of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-2.png)<!-- -->









