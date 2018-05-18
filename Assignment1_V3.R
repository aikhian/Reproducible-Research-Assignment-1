# Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
# Process/transform the data (if necessary) into a format suitable for your analysis

getwd()
setwd("C:/Users/leona/OneDrive/Desktop/Data Analytics/Courera/Course 5 - Reproducible Research/Assignment 1")
getwd()
DF <- read.csv("activity.csv")
library(dplyr)
head(DF)
tail(DF)
str(DF)

# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 
# Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day

dailytotalsteps <- with(DF, tapply(steps, DF$date, sum, na.rm = TRUE))

hist(dailytotalsteps, main = "Total Daily Steps", xlab = "Number of Steps", col = "wheat")
rug(dailytotalsteps)

meanoftotalsteps <- mean(dailytotalsteps)
medianoftotalsteps <- median(dailytotalsteps)

abline(v = meanoftotalsteps, lwd = 1, lty = 4, col = "magenta")
abline(v = medianoftotalsteps, lwd = 1, lty = 4, col = "dark blue")

meanoftotalsteps # Without the NA
medianoftotalsteps # Without the NA

## OR

summary(dailytotalsteps)

# What is the average daily activity pattern?
# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

excludeNA <- DF[!is.na(DF$steps),]

interval <- levels(as.factor(excludeNA$interval))
meanSteps <- with(excludeNA, tapply(steps, excludeNA$interval, mean))
medianSteps <- with(excludeNA, tapply(steps, excludeNA$interval, median))

plot(interval, meanSteps, type = "l", col = "red", main = "Average Number of Steps", xlab = "Interval", ylab = "Mean Steps", ylim = c(0, 250))
grid()

maxSteps <- which.max(excludeNA$steps)
excludeNA[maxSteps,]

summary(dailytotalsteps)
summary(excludeNA)

# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# 
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

sum(is.na(DF))

NAonly <- DF[is.na(DF$steps),]
NAonly$steps <- meanSteps

head(NAonly)

newDF <- rbind(NAonly, excludeNA)
newDF <- newDF[order(newDF$date),]

head(newDF)
tail(newDF)
str(newDF)

dailytotalsteps2 <- with(newDF, tapply(steps, as.factor(newDF$date), sum))

hist(dailytotalsteps2, main = "Total Daily Steps - N.A. Substituted by Interval Mean Values", xlab = "Number of Steps", col = "wheat")
rug(dailytotalsteps2)

meanoftotalsteps2 <- mean(dailytotalsteps2)
medianoftotalsteps2 <- median(dailytotalsteps2)

meanoftotalsteps2
medianoftotalsteps2

# Are there differences in activity patterns between weekdays and weekends?
# For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

newDF$days <- weekdays(as.Date(newDF$date))
head(newDF)

weekendDF <- newDF %>% 
        select(steps, date, interval, days) %>%
        filter(days %in% c("Saturday", "Sunday"))

weekdayDF <- newDF %>% 
        select(steps, date, interval, days) %>%
        filter(days %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

head(weekdayDF)
tail(weekdayDF)

meanweekendDF <- with(weekendDF, tapply(steps, weekendDF$interval, mean))
meanweekdayDF <- with(weekdayDF, tapply(steps, weekdayDF$interval, mean))

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(interval, meanweekdayDF, type = "l", col = "red", main = "Average Number of Steps - Weekdays", xlab = "Interval", ylab = "Mean Steps", ylim = c(0, 250), panel.first = grid())
plot(interval, meanweekendDF, type = "l", col = "red", main = "Average Number of Steps - Weekends", xlab = "Interval", ylab = "Mean Steps", ylim = c(0, 250), panel.first = grid())


