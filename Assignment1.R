
data <- read.csv("./data/activity.csv")


#First question
steps_day <- tapply(data$steps, data$date, sum)

hist(steps_day, breaks = 10, xlab = "Steps by day", 
     main = "Frequency of steps taken by day", col = "red")

mean_steps <- mean(steps_day, na.rm = TRUE)
median_steps <- median(steps_day, na.rm = TRUE)

mean_steps
median_steps


#Second question
avg_interval <- aggregate(steps ~ interval,data = data, mean, na.rm = TRUE)

library(ggplot2)
g <- ggplot(avg_interval, aes(interval, steps))
g + geom_line(size = 1.5, color = "blue") + 
        labs(x = "Interval", y = "Average steps", 
             title = "Average steps per interval") 

max <- max(avg_interval$steps)
max

row_max <- which(avg_interval$steps == max)
max_steps <- avg_interval[row_max,1]
max_steps

#Third question

number_na <- which(is.na(data) == TRUE)
length(number_na)

mean_interval <- tapply(new_data$steps, new_data$interval, mean, na.rm=TRUE)

new_data <- data
new_data$steps[number_na] <- mean_interval[as.character(new_data$interval[number_na])]

steps_day2 <- tapply(new_data$steps, new_data$date, sum)

par(mfcol = c(2,1))
hist(steps_day2, breaks = 10, xlab = "Steps by day", col = "green",
          main = "Frequency of steps taken by day (NA removed)")

hist(steps_day, breaks = 10, xlab = "Steps by day", 
     main = "Frequency of steps taken by day (with NA´s)", col = "red")

mean_steps2 <- mean(steps_day2, na.rm = TRUE)
median_steps2 <- median(steps_day2, na.rm = TRUE)

mean_steps2
median_steps2


#Fourth question
new_data$date <- as.Date(new_data$date)

library(dplyr)

new_data <- mutate(new_data, 
                   week = ifelse(weekdays(new_data$date) == "sábado" | 
                             weekdays(new_data$date) == "domingo",
                   "weekend", "weekday"))

new_data$week <- as.factor(new_data$week)

weekType_avg <- aggregate(steps ~ interval + week, data = new_data, mean)

library(ggplot2)

g2 <- ggplot(weekType_avg, aes(interval, steps, color = week)) + 
        geom_line(size = 1.5) + 
        facet_grid(week~.) + labs(x = "Interval", y = "Average steps",
                                  title = "Mean steps per type of weekday")
g2
