---
title:  "Reproducible Research: Peer Assessment 1"
author: "Oscar de Lama"
date:   "May-17-2015"
output: 
  html_document:
    keep_md: true
---




## I. Loading and preprocessing the data

We will just load the data and keep it as is. This is the code needed to load the data:


```r
activity.df <- read.csv('activity.csv')
# An additional pre-processing is not required.
```

Additional pre-processing is not required.

## II. What is mean total number of steps taken per day?

For this part of the assignment, we can ignore the missing values in the dataset.

### II.1. Calculate the total number of steps taken per day

First, we will compute the total number of steps taken each day.


```r
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
total.steps.per.day.df <-
  subset(activity.df, !is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps.per.date = sum(steps))
```

### II.2. Plot the steps histogram

Now, we will plot a histogram of the total number of steps taken each day.


```r
library(ggplot2)
```

```
#> Use suppressPackageStartupMessages to eliminate package startup messages.
```

```r
ggplot(total.steps.per.day.df) +
  ggtitle('Total number of steps taken each day') +
  geom_histogram(aes(steps.per.date), binwidth=bin.width, fill='gray', color='darkgreen')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

### II.3. Calculate the mean and median of steps taken per day

We will calculate the mean and median of the total number of steps taken per day.


```r
mean.steps.per.day <- mean(total.steps.per.day.df$steps.per.date)
median.steps.per.day <- round(median(total.steps.per.day.df$steps.per.date))
```

We will report the mean and median of the total number of steps taken per day.


```r
cat('mean of steps per day:', mean.steps.per.day)
```

```
#> mean of steps per day: 10766
```

```r
cat('median of steps per day:', median.steps.per.day)
```

```
#> median of steps per day: 10765
```

The mean of steps per day is **10766.2**.
The median of steps per day is **10765**.

## III. What is the average daily activity pattern?

### III.1. Plot average number of steps per interval, averaged across all days

First we will compute the average number of steps taken, averaged across all days.

```r
mean.steps.per.interval.df <-
  subset(activity.df, !is.na(steps)) %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps))
```

Now we can make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days.


```r
ggplot(mean.steps.per.interval.df) +
  ggtitle('Number of steps averaged across all days') +
  geom_line(aes(interval, mean.steps))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

### III.2. Which 5-minute interval, contains the maximum number of steps?

Let's find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.


```r
max.mean.steps <- max(mean.steps.per.interval.df$mean.steps)
case.max.mean.steps <- subset(mean.steps.per.interval.df, mean.steps == max.mean.steps)
```

Now we can report the which one is the required 5-minute interval:


```r
cat('Interval with max average steps:', case.max.mean.steps$interval)
```

```
#> Interval with max average steps: 835
```

The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is **835**.

## IV. Imputing missing values

### IV.1. Calculate and report the total number of rows with NAs.

First, we will calculate the total number of missing values in the dataset.


```r
number.of.nas <- sum(is.na(activity.df))
```

Now, we can report the total number of missing values in the dataset:


```r
cat('The number of row with NAs is', number.of.nas)
```

```
#> The number of row with NAs is 2304
```

The number of row with NAs is **2304**.

### IV.2. Devise a strategy for filling in all of the missing values in the dataset.

We will replace the NA values in the steps variable (Dataset column) with the median number of steps in the same interval computed along all days. If there is no data for given interval, which just have NA values, we will use the overall median value along all the intervals in the data set.

### IV.3. Create dataset equal to the original one with the missing data filled in

We will create the *new.activity.df* data set with the missing steps filled using the strategy described above.


```r
new.activity.df <- activity.df

# Compute the median steps per date (ignoring the NA cases)
median.steps.per.interval.df <-
  subset(activity.df, !is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = median(steps))

# Overall median steps per interval
median.steps.per.interval <- median(median.steps.per.interval.df$steps)

# For each row in the activity data table:
for (row.index in 1:nrow(new.activity.df)) {
  # Read the current row
  row <- new.activity.df[row.index,]
  # If the row has a NA value
  if (is.na(row$steps)) {
    # Find row with the same date in median.steps.per.day.df table
    median.steps.row <- subset(median.steps.per.interval.df, interval == row$interval)
    # If no row was found...
    median.steps <-
      # ... use the total median per day...
      if (nrow(median.steps.row) == 0) median.steps.per.interval
      # ...otherwise, use the median for that day
      else median.steps.row$steps

    # Replace the NA value in the current row
    new.activity.df[row.index,'steps'] <- median.steps
  }
}
```

#### IV.4.1. Plot a  Histogram of the total number of steps taken each day.

Now we can make a histogram of the total number of steps taken each day from the data set with the missing data filled as described above.


```r
new.total.steps.per.day.df <-
  subset(new.activity.df) %>%
  group_by(date) %>%
  summarize(total.steps = sum(steps))

ggplot(new.total.steps.per.day.df) +  
  ggtitle('Total number of steps taken each day') +
  geom_histogram(aes(total.steps), binwidth=bin.width, fill='gray', color='darkgreen')
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 


#### IV.4.2. Calculate and report the mean and median total number of steps taken per day

Here we will calculate and report the mean and median total number of steps taken per day from the data set with the missing data filled as described above.


```r
new.mean.steps.per.day <- mean(new.total.steps.per.day.df$total.steps)
new.median.steps.per.day <- median(new.total.steps.per.day.df$total.steps)

cat('The (new) mean of steps per day is:', new.mean.steps.per.day)
```

```
#> The (new) mean of steps per day is: 9504
```

```r
cat('The (new) median of steps per day is:', new.median.steps.per.day)
```

```
#> The (new) median of steps per day is: 10395
```

The new mean of steps per day is **9503.9**.
The new median of steps per day is **10395**.

### IV.4.3 Do these values differ from the estimates from the first part of the assignment?

The histogram has changed, with an added new second bar with values that in the original data set were NA.

They differ, in the fisrt part of this assignment we got a mean of **10766.2**.
and a median of **10765**. 

The impact caused by imputing missing data is a difference of **1262.3** in the mean of steps per day and **370** in the median of steps per day. The impact is smaller in the median value because we have replaced the NANs with interval median values along all days.

## V. Are there differences in activity patterns between weekdays and weekends?

### V.1. Create a new factor variable in the dataset with two levels


```r
week.day <- weekdays(as.Date(activity.df$date))
weekends <- week.day == 'sábado' | week.day == 'domingo'
week.day[weekends] <- 'weekend'
week.day[!weekends] <- 'weekday'
activity.df$weekday.kind <- as.factor(week.day)
```

### V.2. Plot the interval and the steps average across weekday/weekend days


```r
mean.steps.df <-
  subset(activity.df, !is.na(steps)) %>%
  group_by(weekday.kind, interval) %>%
  summarize(steps = mean(steps))

ggplot(mean.steps.df, aes(interval, steps)) +
  ggtitle('Steps average across the kind of week day') +
  geom_line() +
  facet_grid(weekday.kind ~ .)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 


In the plots above, we can see there are differences in the activity patterns between weekdays and weekends. There more activity in the first intervals in the weekdays than in the weekends.


End of Report.
