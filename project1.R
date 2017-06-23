library(ggplot2)
library(mice)
library(VIM)


# 1. Code for reading in the dataset and/or processing the data
activity <- read.csv("activity.csv",header = T)
# 2. Histogram of the total number of steps taken each day
days <- aggregate(steps~date, data = activity, FUN = sum)
hist(days$steps, breaks = 20)
# 3. Mean and median number of steps taken each day
mean(days$steps)
median(days$steps)
# 4. Time series plot of the average number of steps taken
ints <- aggregate(steps~interval, data = activity, FUN = mean)
qplot(interval, steps, data = ints, geom = "line")
# 5. The 5-minute interval that, on average, contains the maximum number of steps
ints[ints$steps == max(ints$steps),]
# 6. Code to describe and show a strategy for imputing missing data
md.pattern(activity) # show the missing values
activity_plot <- aggr(activity, col=c('green','gray'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(activity), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
activity_imputed <- mice(activity, m=5, maxit = 5, method = 'pmm')
activity_complete <- complete(activity_imputed)
# 7. Histogram of the total number of steps taken each day after missing values are imputed
days_complete <- aggregate(steps~date, data = activity_complete, FUN = sum)
hist(days_complete$steps, breaks = 20)
mean(days_complete$steps)
median(days_complete$steps)
# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activity_complete$date <- as.Date(activity_complete$date)
activity_complete$weekday <- weekdays(activity_complete$date)
activity_complete$dayType <- ifelse(activity_complete$weekday %in% c("Saturday", "Sunday"),
                                    "Weekend", "Weekday")
ints_complete <- aggregate(steps~interval+dayType, data = activity_complete, FUN = mean)
qplot(interval, steps, data = ints_complete, facets = dayType~., geom = "line")
# 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report