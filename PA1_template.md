---
title:  '**Reproducible Research: Peer Assessment 1**'
author: "Bilmar Fernando H. H."
date: "12 de agosto de 2015"
output: 
  html_document: 
    fig_caption: yes
---
```{r}
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(ggthemes)
library(dplyr)
library(scales)
```

## **Loading and preprocessing the data**

Show any code that is needed to Load the data (i.e. <code>read.csv()</code>)  
Process/transform the data (if necessary) into a format suitable for your analysis

*Lubridate's wday function can occupy in the Weekday's full names as ordered factor variables.*  
*Create a column of these days of the week for each date for subsetting.*

```{r}
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
activity$date <- as.Date(activity$date)
activity$Weekday <- wday(activity$date, label = TRUE, abbr = FALSE)
head(activity)
```

## **What is mean total number of steps taken per day?**

For this part of the assignment, you can ignore the missing values in the dataset.

**1.-** Calculate the total number of steps taken per day

```{r}
totalStep <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
print(totalStep)
```

Makes a histogram of the total number of steps taken each day

```{r}
par(mar = c(5, 4, 1, 1), las = 1)
make.sums.ggplot <- function(activity.dataframe, RBrewers.colors = "Blues"){
  activity.sums <- activity.dataframe %>%
  group_by(date, Weekday) %>%
  summarise(totalStep = sum(steps))
  my.cols <- brewer.pal(7, RBrewers.colors)
  my.cols[1] <- my.cols[7]
  max.sum <- max(activity.sums$totalStep, na.rm = TRUE)
  ggplot(activity.sums, aes(x = date, y = totalStep, fill = Weekday)) + geom_bar(stat = "identity") +
    scale_x_date(breaks = "1 day", limits = as.Date(c('2012-10-02','2012-11-29'))) +
    theme_wsj() +    
    theme(axis.text.x  = element_text(size = 5, angle = 80, colour = "black", vjust = 1, hjust = 1)) + 
    scale_fill_manual(values = my.cols) + 
    geom_text(aes(x = date, y = totalStep, label = totalStep, angle  = 90, size = 3, 
                  hjust = -0.1), color = "purple", show_guide  = F) + 
    coord_cartesian(ylim = c(0, max.sum*1.15)) +
    ylab("Total Steps by Day") +
    xlab(NULL)
}
make.sums.ggplot(activity, "Blues")
```

**2.-** Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(totalStep$steps)
median(totalStep$steps)
```

## **What is the average daily activity pattern?**

**1.-** Make a time series plot (i.e. <code>type = "l"</code>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
print(time_series)
```

**2.-** Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
activity$Interval <- as.POSIXct(strptime(sprintf("%04d", activity$interval), "%H%M")) 
make.max.interval.ggplot <- function(activity.dataframe){
  activity.intervals <- activity.dataframe %>%
    group_by(Interval) %>%
    summarise(Average = mean(steps, na.rm = TRUE)) %>%
    arrange(Interval)
  max.activity <- activity.intervals[which.max(activity.intervals$Average),]
  max.interval <<- max.activity$Interval[1]
  max.average <<- round(max.activity$Average[1], 2)
  ggplot(activity.intervals, aes(x = Interval, y = Average)) +
    geom_line() +
    theme_economist() +
    theme(axis.text.x=element_text(angle=270, hjust=1, vjust=0.5, size = 10)) + 
    scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                     limits = c(activity.intervals$Interval[12], activity.intervals$Interval[286-10])) +
    ylab("Average Steps Across All Days") + 
    xlab("5-min Intervals labeled in chunks of 1 Hrs")
}
make.max.interval.ggplot(activity)
```

## **Imputing missing values**

_Note that there are a number of days/intervals where there are missing values (coded as <code>NA</code>). The presence of missing days may introduce bias into some calculations or summaries of the data._

**1.-** Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with <code>NA</code>s)
```{r}
summary(activity)
NA.activity <- subset(activity, !complete.cases(activity))
table(NA.activity$date)
```

**2.-** Devise a strategy for occupying in all of the missing values in the dataset. The strategy does not need to be sophisticated. _For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc._
```{r}
interval.summary <- function(activity_frame, interval_string = "all"){
  activity_frame$temp_time <- format(activity_frame$Interval, "%H:%M")
  if (class(interval_string) != "character") {
    stop("Please enter an interval as a string.")
    }
  if (!(interval_string %in% c( activity_frame$temp_time, "all" ) ) ) {
    stop("Please enter a 5 minute interval in the form of 00:00")
    }
  output <- activity_frame %>% 
    group_by(temp_time, Weekday) %>%
    summarise(Average = mean(steps, na.rm = TRUE))
  if (interval_string == "all") {
    return(output)
    } else {
      output<- output %>%
        filter(temp_time == interval_string)
      return(output)
      }
  }
interval.summary(activity, "08:05")
```

**3.-** Create a new dataset that is equal to the original dataset but with the missing data occupyed in.

```{r}
imputed <- activity %>%
  mutate(steps = as.numeric(steps)) %>%
  group_by(Interval, Weekday) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
  mutate(steps = round(steps, 2))
```

**4.-** Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
make2.sums.ggplot <- function(activity.dataframe, RBrewers.colors = "Reds"){
  activity.sums <- activity.dataframe %>%
  group_by(date, Weekday) %>%
  summarise(totalStep = sum(steps))
  my.cols <- brewer.pal(7, RBrewers.colors)
  my.cols[1] <- my.cols[7]
  max.sum <- max(activity.sums$totalStep, na.rm = TRUE)
  ggplot(activity.sums, aes(x = date, y = totalStep, fill = Weekday)) + geom_bar(stat = "identity") +
    scale_x_date(breaks = "1 day", limits = as.Date(c('2012-10-02','2012-11-29'))) +
    theme_wsj() +    
    theme(axis.text.x  = element_text(size = 6, angle = 70, colour = "black", vjust = 1, hjust = 1)) + 
    scale_fill_manual(values = my.cols) + 
    geom_text(aes(x = date, y = totalStep, label = totalStep, angle  = 90, size = 3, 
                  hjust = -0.1), color = "purple", show_guide  = F) + 
    coord_cartesian(ylim = c(0, max.sum*1.15)) +
    geom_hline(aes ( yintercept = mean(totalStep, na.rm = TRUE)), 
               color = "blue", size = 1.5, alpha = .70) + 
    geom_hline(aes ( yintercept = median(totalStep, na.rm = TRUE)), 
               color = "darkgreen",size = 1.5, alpha = .60) +
    geom_text(aes(label = paste("Mean = ", round(mean(totalStep, na.rm = TRUE), 4 )),
                  x = as.Date('2012-10-10'), y = 21800), color = "blue", size = 5) +
    geom_text(aes(label = paste("Median = ", round(median(totalStep, na.rm = TRUE), 4)),
                  x = as.Date('2012-10-09'), y = 20000), color = "darkgreen", size = 5) +
    ylab("Total Steps by Day") +
    xlab(NULL)
}
make2.sums.ggplot(imputed, "Reds")
```

# **Are there differences in activity patterns between weekdays and weekends?**

For this part the <code>weekdays()</code> function may be of some help here. Use the dataset with the filled-in missing values for this part.

**1.-** Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
imputed <- activity %>%
mutate(Weekend = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
table(imputed$Weekend, imputed$Weekday)
```

**2.-** Make a panel plot containing a time series plot (i.e. <code>type = "l"</code>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
make.Weekend.ggplot <- function(activity.dataframe){
  activity.intervals <- activity.dataframe %>%
    group_by(Weekend, Interval) %>%
    summarise(Average = mean(steps, na.rm = TRUE))
  ggplot(activity.intervals, aes(x = Interval, y = Average, group = Weekend, color = Weekend)) +
    theme_solarized(light = FALSE) +
    geom_line() +
    facet_grid(Weekend~.) +
    theme(axis.text.x = element_text(angle = 270, hjust = 1, vjust = 0.5, size = 9)) + 
    scale_x_datetime(breaks = date_breaks("30 mins"), labels = date_format("%H:%M"),
                     limits = c(activity.intervals$Interval[12], activity.intervals$Interval[286-10])) +
    ylab("Average Steps Across All Days") + 
    xlab("5-minute Intervals labeled in chunks of 30-minutes") +
    theme(legend.position="none")
}
make.Weekend.ggplot(imputed)
```
