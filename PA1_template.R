#Peer Assessment 1

library(ggplot2) 

setwd("D:/R/data/ReproducibleResearch")
cat("\014")  

###Loading and preprocessing the data
###Process/transform the data (if necessary) into a format suitable for your analysis

unzip("activity.zip")
data <- read.csv("activity.csv", header = TRUE, sep ="," , colClass=c("numeric", "character", "numeric"))
data$date <- as.Date(data$date, format="%Y-%m-%d")


#Return samples of dataset
head(data)
str(data)
summary(data)


## What is mean total number of steps taken per day?

#Make a subset, removing NA and aggregate step by date
steps_date <- aggregate(formula = steps~date, data = data, FUN = sum, na.rm=TRUE)
summary(steps_date )

### 1. Make a histogram of the total number of steps taken each day

qplot(
        x=date,
        y=steps,
        data=steps_date,
        stat="identity",
        geom="bar",
        ylab ="Total Number of Steps", 
        xlab="Date"       
        )
### 2. Make a histogram of the total number of steps taken each day

mean_by_date <- round(mean(steps_date$steps), 2)
median_by_date <- round(median(steps_date$steps), 2)


## What is the average daily activity pattern?

#Make a subset, removing NA and aggregate step by date
step_interval <- aggregate(formula = steps ~ interval, data = data, FUN = mean, na.rm=TRUE)
summary(step_interval )

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
meanplot <-
qplot(
        x=interval,
        y=steps,
        data=step_interval,
        stat="identity",
        geom="line",
        ylab ="Average Number of Steps", 
        xlab="5-minute interval"       
)
plot(meanplot)
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

interval_max_step <- step_interval[which.max(step_interval$steps),]$interval


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(!complete.cases(data))

##Missing steps
sum(is.na(data$steps))
##Missing date
dates_in_range <- seq.Date(from = min(data$date),
                           to = max(data$date),
                           by='1 day')
sum(!data$date[complete.cases(data)] %in% dates_in_range)

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### From the observations of total steps taken per day and average steps taken per interval, it seems that there is more variation between the number of steps taken day to day (Figure 1, above) than in the average of steps taken during each interval across the different days (Figure 2, above). Given this, the imputation strategy I will follow is to complete the missing cases using the average number of steps from the corresponding interval (rounded towards zero to avoid using fractional steps).

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

###Round steps in dataset step_interval to avoir fractional steps. 
step_interval$round_steps <- floor(step_interval$steps)

###Merge orignial dataset with step_interval dataset(mean by interval) by interval
merged <- merge(data, step_interval[,c('interval', 'round_steps')], by='interval')

###Replace missing value
merged$steps <- ifelse(is.na(merged$steps),
                                 merged$round_steps,
                                 merged$steps)
###Remove extra column
merged$round_steps <- NULL

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

steps_date_n <- aggregate(steps ~ date, data = merged, FUN = sum)
qplot(
        x=date,
        y=steps,
        data=steps_date_n,
        stat="identity",
        geom="bar",
        ylab ="Total Number of Steps", 
        xlab="Date"       
)
mean_by_date_n <- round(mean(steps_date_n$steps), 2)
mean_by_date_n
median_by_date_n <- round(median(steps_date_n$steps), 2)
median_by_date_n


## Are there differences in activity patterns between weekdays and weekends?

### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

#Create a function
factor_w <- function(date)
{
        if(weekdays(date) %in% c("Saturday", "Sunday")) {"Weekend"}
        else {"Weekday"}
}

merged$type <- as.factor(sapply(merged$date, factor_w))

### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

qplot(
        x=interval,
        y=steps,
        data=merged,
        
        stat="summary",
        fun.y = "mean",
        geom="line",
        ylab ="Average Number of Steps", 
        xlab="5-minute interval"        
) + facet_wrap (~type, nrow=2, ncol=1)