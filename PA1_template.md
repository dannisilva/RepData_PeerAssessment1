---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("/Users/danielesilva/Documents/Curso de Ciencia de Dados/05 - Reproducible Research/RepData_PeerAssessment1")

temp <- "activity.zip"
unztemp <- unzip(temp)
data = read.csv("activity.csv", sep = ",", header = T)

data$date = as.Date(data$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
steps_day = data %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(sum_step = sum(steps))

ggplot(steps_day, aes(x=sum_step)) +
        geom_histogram(bins = 8, color = "black", fill = "#5DADE2")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
meanpday = mean(steps_day$sum_step)
medianpday = median(steps_day$sum_step)
```

Mean per day: 1.0766189\times 10^{4}
Median per day 10765

## What is the average daily activity pattern?


```r
daily_act = data %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(mean_daily_act = mean(steps))

ggplot(daily_act, aes(x=interval)) +
        geom_line(aes(y=mean_daily_act, color="#922B21")) +
        theme(legend.position = "none") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
max_step = daily_act[which.max(daily_act$mean_daily_act),"interval"]
```
The 5-minute intervalcontains the maximum number of stepsMedian per day is 835

## Imputing missing values


```r
nas = sum(is.na(data$steps))

data_nmiss =  merge(data,daily_act, by = "interval")

data_nmiss$new_steps = ifelse(is.na(data_nmiss$steps),data_nmiss$mean_daily_act,data_nmiss$steps)

steps_day_nmiss = data_nmiss %>%
        group_by(date) %>%
        summarise(sum_step = sum(new_steps))

ggplot(steps_day_nmiss, aes(x=sum_step)) +
        geom_histogram(bins = 8, color = "black", fill = "#5DADE2")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
meanpday_nmiss = mean(steps_day_nmiss$sum_step)
medianpday_nmiss  = median(steps_day_nmiss$sum_step)
```
The total number of missing values in the dataset is 2304

New Mean per day: 1.0766189\times 10^{4}
New Median per day 1.0766189\times 10^{4}

There are no big difference between the means or medians.


## Are there differences in activity patterns between weekdays and weekends?


```r
data_nmiss$type_day = ifelse(weekdays(data_nmiss$date) %in% c("Saturday","Sunday"),"Weekend","Weekday")

data_nmiss$type_day = factor(data_nmiss$type_day, levels = c("Weekday","Weekend"))

steps_day_weekdays = data_nmiss %>%
        group_by(type_day,interval) %>%
        summarise(mean_step = mean(new_steps))

pal = c("#943126","#21618C")
ggplot(steps_day_weekdays, aes(x=interval, color=type_day)) +
        geom_line(aes(y=mean_step)) +
        theme(legend.position = "none") +
        scale_colour_manual(values = pal) +
        facet_wrap(type_day~., nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
