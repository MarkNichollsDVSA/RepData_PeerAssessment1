---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(readr)
unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?


```r
library(dplyr)
part_one <- data %>% 
  group_by(date) %>%
  summarise(total_steps = sum(steps))

# create a histogram of the total number of steps taken each day
hist(part_one$total_steps, xlab = "", main = "Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# calculate and report the mean
mean(part_one$total_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
# calculate and report on the median
median(part_one$total_steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
part_two <- data %>%
  group_by(interval) %>%
  summarise(ave_steps = mean(steps, na.rm = TRUE))

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

with(part_two, plot(x = interval, y = ave_steps, type = "l",
     main = "Average Steps per Interval",
     xlab = "Interval",
     ylab = "Average Steps (mean)")
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

part_two[which.max(part_two$ave_steps),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```


## Imputing missing values


```r
# Calculate and report the total number of missing values in the dataset
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# I already have the interval averages calculated in part_tw0

part_three <- data %>%
  mutate(steps = ifelse(is.na(steps),
                        part_two[which(part_two$interval == interval),2],
                        steps ))
```

```
## Warning: Problem with `mutate()` input `steps`.
## i The `i` argument of ``[.tbl_df`()` must lie in [0, rows] if positive, as of tibble 3.0.0.
## Use `NA_integer_` as row index to obtain a row full of `NA` values.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
## i Input `steps` is `ifelse(...)`.
```

```
## Warning: The `i` argument of ``[.tbl_df`()` must lie in [0, rows] if positive, as of tibble 3.0.0.
## Use `NA_integer_` as row index to obtain a row full of `NA` values.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```


## Are there differences in activity patterns between weekdays and weekends?



