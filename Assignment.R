library(lattice)
# setwd("D:/Desktop/Data Science Classes/Reproducible Research/Assessment1/Week 1")

dataFile <- "Data/activity.csv"
data <- read.csv(dataFile)

#### Q1 : What is mean total number of steps taken per day?
# aggregate(data$steps ~ data$date, data, sum)
analysis1a <- tapply(data$steps, data$date, FUN=sum)
barplot(analysis1a)

## Report Mean
dayMean1a <- mean(analysis1a, na.rm = TRUE)
## Report Median
dayMedian1a <- median(analysis1a, na.rm = TRUE)


#### Q2 : What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

stepsPerInterval <- aggregate(data$steps ~ data$interval, data, mean)
plot(stepsPerInterval,
     type="l",
     xlab = "Interval",
     ylab = "Number of Steps")

## "Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?"
maxNumOfSteps <- max(stepsPerInterval["data$steps"], na.rm = TRUE)
intervalWithMaxAvgSteps <- stepsPerInterval[which(stepsPerInterval["data$steps"] == maxNumOfSteps),1]

#### Q3 : Imputing missing values
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
numberOfNAs <- sum(!complete.cases(data))

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## I will use the average steps per interval to fill in steps in missing intervals.
## Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputedData <- data
for ( ndx in 1:nrow(imputedData)) if ( is.na(imputedData[ndx,1]) ) imputedData[ndx,1] <- stepsPerInterval[which(stepsPerInterval$`data$interval`==imputedData[ndx,3]),2]

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
analysis3a <- tapply(imputedData$steps, imputedData$date, FUN=sum)
barplot(analysis3a)

## Report Mean
dayMean3a <- mean(analysis3a, na.rm = TRUE)
## Report Median
dayMedian3a <- median(analysis3a, na.rm = TRUE)

#### Q4 Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels -- "weekday" 
## and "weekend" indicating whether a given date is a weekday or weekend day.
data4 <- imputedData
data4$date <- as.POSIXlt(data4$date)
data4$dayType <- ifelse ( data4$date$wday %in% c(0,6),"weekend","weekday")
data4$dayType <- as.factor(data4$dayType)

## Make a panel plot containing a time series plot (i.e. type = "l") of the 
## 5-minute interval (x-axis) and the average number of steps taken, averaged 
## across all weekday days or weekend days (y-axis).
stepsPerInterval2 <- aggregate(data4$steps ~ data4$interval*data4$dayType, data4, mean)
xyplot(stepsPerInterval2$`data4$steps`~stepsPerInterval2$`data4$interval`|stepsPerInterval2$`data4$dayType`,
       layout=(c(1,2)),
       type="l",
       xlab = "Interval",
       ylab = "Number of Steps")

