## Loading and preprocessing the data
activity <- read.csv("activity/activity.csv", header = TRUE, sep = ",")
View(activity)
colnames(activity)
df1 <- na.omit(activity)
## What is mean total number of steps taken per day?
df2 <- aggregate(steps~date, data=activity, FUN=mean, na.rm=TRUE)
hist(df2$steps)
## ignore the missing values in the dataset.

# Calculate the total number of steps taken per day



# Make a histogram of the total number of steps taken each day



# Calculate and report the mean and median of the total number of steps taken per day

