library(readr)
library(dplyr)
library(data.table)
library(lattice)
library(gridExtra)
# Unzip and read data from file in local directory
activityData <- read.csv(unz("activity.zip", "activity.csv"), header=TRUE,
                 na.strings = "NA")
# Convert date format
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")


# By day analysis, ignore missing values

# Find the total number of steps taken in a day
stepsByDay <- activityData %>% group_by(date) %>%
      summarise(totalSteps=sum(steps, na.rm = TRUE))
stepsByDay <- data.table(stepsByDay)

# Histogram of the number of steps per day
hist(stepsByDay$totalSteps, main = "Number of Steps per Day",
     xlab = "Number of Steps", ylab = "Frequency", ylim = c(0,40),
     breaks = c(0,2500,5000,7500,10000,12500,15000,17500,20000,22500,25000))

# Median and mean number of steps per day (single numbers)
medSteps <- median(stepsByDay$totalSteps)
meanSteps <- round(mean(stepsByDay$totalSteps),2)


# By time of day analysis, ignore missing values

# Find the number of steps per time period across dates
stepsByPeriod <- activityData %>% group_by(interval) %>%
      summarise(avgSteps=mean(steps, na.rm = TRUE))
stepsByPeriod <- data.table(stepsByPeriod)

# Plot of steps v. time of day
plot(stepsByPeriod, type = "l", xlab = "Minutes after Midnight",
     ylab = "Average Number of Steps",
     main = "Activity Level by Time of Day")

# Find the time period with the largest average number of steps
# Of course, you cannot apply which.max to a data frame, so first,
# stepsByPeriod has to be converted to a matrix...
stepsByPeriodMatrix <- as.matrix(stepsByPeriod)
maxStepPeriod <- stepsByPeriodMatrix[which.max(stepsByPeriodMatrix[,2]),1]
todHr <- floor(maxStepPeriod/60)
todMin <- maxStepPeriod - todHr*60


# Impute missing values in the initial data set

# First, find out how many rows have missing values
missingRowValues <- sum(!complete.cases(activityData))
missingStepsByDay <- activityData %>% group_by(date) %>%
      summarise(totalSteps=sum(steps))
missingDateValues <- sum(!complete.cases(missingStepsByDay))

# Use average number of steps per time period from stepsByPeriod to impute
# the missing values by time period (regardless of day of week)
naIdx <- which(is.na(activityData$steps))
crIdx <- match(activityData$interval,stepsByPeriod$interval)
actDataImputed <- activityData
actDataImputed$steps <- as.numeric(actDataImputed$steps)
actDataImputed[naIdx,1] <- stepsByPeriod[crIdx[naIdx],2]


# By day analysis, imputing missing values

# Find the total number of steps taken in a day
stepsByDayImputed <- actDataImputed %>% group_by(date) %>%
      summarise(totalSteps=sum(steps, na.rm = TRUE))
stepsByDayImputed <- data.table(stepsByDayImputed)

# Histogram of the number of steps per day
hist(stepsByDayImputed$totalSteps)

# Median and mean number of steps per day (single numbers)
medImp <- median(stepsByDayImputed$totalSteps)
meanImp <- mean(stepsByDayImputed$totalSteps)


# Analyze weekdays v. weekends

# Define weedayDays and weekendDays
weekdayDays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekendDays <- c("Saturday","Sunday")

# Classify data rows by weekday v. weekend
idxWk <- which(weekdays(actDataImputed$date) %in% weekdayDays)
idxWe <- which(weekdays(actDataImputed$date) %in% weekendDays)
actDataImputed$dayOfWk <- NA
actDataImputed$dayOfWk[idxWk] <- "weekday"
actDataImputed$dayOfWk[idxWe] <- "weekend"

stepsOnWeekdays <- actDataImputed[actDataImputed$dayOfWk == "weekday",]
stepsOnWeekends <- actDataImputed[actDataImputed$dayOfWk == "weekend",]

stepsByPeriodOnWD <- stepsOnWeekdays %>% group_by(interval) %>%
      summarise(avgSteps=mean(steps, na.rm = TRUE))
stepsByPeriodOnWD <- data.table(stepsByPeriodOnWD)

stepsByPeriodOnWE <- stepsOnWeekends %>% group_by(interval) %>%
      summarise(avgSteps=mean(steps, na.rm = TRUE))
stepsByPeriodOnWE <- data.table(stepsByPeriodOnWE)


# Plot of steps v. time of day
#dev.new(width = [8], height = [8])
plot1 <- xyplot(avgSteps ~ interval, data = stepsByPeriodOnWD, type = "b",
                xlim = c(0,2400), ylim = c(0,250))
plot2 <- xyplot(avgSteps ~ interval, data = stepsByPeriodOnWE, type = "b",
                xlim = c(0,2400), ylim = c(0,250))
grid.arrange(plot1,plot2, nrow=2)

#plot(stepsByPeriodOnWD, type = "l", xlab = "Minutes after Midnight", ylab = "Average Number of Steps",
#     main = "Activity Level by Time of Day", xlim = c(0,2400), ylim = c(0,250))
