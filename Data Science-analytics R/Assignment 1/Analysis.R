source('functions.r')
#libraries
library('data.table')
library('ggplot2')
library('dplyr')
library('tidyr')

#Dataset
dataset <- read.csv("/home/dennis/Desktop/Data-Science-analytics-R/david/full_trains.csv", header = TRUE)

#theme for plots
theme_set(theme_classic())
theme_update(plot.title = element_text((hjust = 0.5)))

#EDA
summary(dataset)
numeric_cols <- sapply(dataset,is.numeric)
numeric_dataset <- dataset[,numeric_cols]

#futher EDA
numeric_dataset <- numeric_dataset[, !colnames(numeric_dataset) %in% 'year']
numeric_dataset <- numeric_dataset[, !colnames(numeric_dataset) %in% 'month']
boxplot(numeric_dataset)

#preprocessing
#removing NA
which(is.na(dataset))

dataset <- subset.data.frame(dataset,select = -c(comment_cancellations,comment_delays_on_arrival,comment_delays_at_departure))
dataset <- drop_na(dataset)

#question 1
#Distribution of cancelled trains
distribution <- dataset %>% group_by(departure_station) %>% summarise(Total_cancelled = sum(num_of_canceled_trains))

#quetion 2
#Average trip times
average_trip_time <- dataset %>% group_by(departure_station) %>% summarise(Mean_trip_time = mean(journey_time_avg))

#question 3
#trips by month 
varying_trips <- dataset %>% group_by(year,month,departure_station) %>% summarise(trips_by_station = sum(total_num_trips))
varying_trips$Date <- as.Date(paste0(varying_trips$year, "-", varying_trips$month, "-01"))
varying_trips <- subset.data.frame(varying_trips,select = -c(year,month))


which.max(varying_trips$trips_by_station)
which.min(varying_trips$trips_by_station)
mean(varying_trips$trips_by_station)

#question 4
cancellations <- dataset %>% group_by(departure_station) %>% summarise(cancelled_trips= num_of_canceled_trains)
summary(cancellations)


cancellations <- remove_outliers(cancellations,"cancelled_trips", 1)
cancellations_anova <- aov(cancelled_trips ~ departure_station, data = cancellations)
summary(cancellations_anova)
