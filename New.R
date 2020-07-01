library(knitr)
library(ggplot2)
library(data.table)
opts_chunk$set(echo = TRUE, results = 'hold')

activity <- unzip("repdata_data_activity.zip")
act_data <- read.csv("activity.csv", header=TRUE, sep=",")

str(act_data)

act_data$date <- as.Date(act_data$date, format="%Y-%m-%d")
act_data$interval <- as.factor(act_data$interval)

str(act_data)

head(act_data, 10)


steps_per_day <- aggregate(steps ~ date, data=act_data, FUN=sum)
colnames(steps_per_day) <- c("date", "steps")

head(steps_per_day, 10)

ggplot(steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title = "Histogram - Steps Taken Per Day", x = "Steps Per Day", y = "Frequency")

mean_steps_per_day <- mean(steps_per_day$steps)
mean_steps_per_day
median_steps_per_day <- median(steps_per_day$steps)
median_steps_per_day

steps_per_interval <- aggregate(steps ~ interval, data = act_data, FUN = mean, na.rm = TRUE)
steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")

head(steps_per_interval, 10)

ggplot(steps_per_interval, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Steps")
