
# set the working directory
setwd("D:/ISB Co 2018/Coursera/Data Science Specialization/Reproducible Research/Week 2/Project 1")

actv1 <- read.csv("activity.csv")
str(actv1)

library(dplyr)
library(zoo)

actv1 <- mutate(actv1, date = as.Date(as.character(date), "%Y-%m-%d"))

is.regular(actv1$date)
unique(actv1$date)

# histogram of total steps taken each day

t_steps1 <- aggregate(steps~date,data=actv1,sum,na.rm = TRUE)
names(t_steps1) <- c("date","sum of steps")
hist(t_steps1$`sum of steps`, main = "histogram of the total steps taken each day", xlab = "steps each day", breaks = 20, col = "red")

# mean & median of total steps taken each day

mean(t_steps1$`sum of steps`)
median(t_steps1$`sum of steps`)

# time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

i_steps1 <- aggregate(steps~interval,data=actv1,mean,na.rm = TRUE)
names(i_steps1) <- c("interval", "mean")
plot(i_steps1,type="l")

# 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps

i_steps1$interval[which.max(i_steps1$mean)]

# number of missing values or NA in the dataset

sum(is.na(actv1))

# for missing values or with NA in dataset, plan to replace them with the respective mean of that 5-min interval

# creating the new dataset with missing values replaced

actv2 <- actv1
actv2$steps[is.na(actv2$steps)] <- mean(na.omit(actv2$steps))
actv2$date <- as.Date(actv2$date,format = "%Y-%m-%d")

# histogram of total steps taken each day

t_steps2 <- aggregate(steps~date,data=actv2,sum,na.rm = TRUE)
names(t_steps2) <- c("date","sum of steps")
hist(t_steps2$`sum of steps`, main = "histogram of the total steps taken each day", xlab = "steps each day", breaks = 20, col = "red")

# mean & median of total steps taken each day

mean(t_steps2$`sum of steps`)
median(t_steps2$`sum of steps`)

# difference in activity patterns between weekdays & weekends

library(lubridate)

# write a fucntion to check if date corresponnds to weekday or weekend

whatday <- function(d){
  c <- weekdays(d)
  ifelse (c=="Saturday"|c=="Sunday","weekend","weekday")
}

e <- sapply(actv2$date,whatday)
actv2$wkday <- as.factor(e)

# panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

library(lattice)
actv_wk <- aggregate(steps ~ wkday+interval, data=actv2, FUN=mean)
xyplot(steps ~ interval | factor(wkday),layout = c(1, 2), xlab="Interval", ylab="No of steps", type="l", lty=1, data=actv_wk)

