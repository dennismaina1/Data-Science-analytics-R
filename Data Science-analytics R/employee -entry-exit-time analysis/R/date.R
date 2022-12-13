library(data.table)
library(lubridate)
library(stringr)

#extract the na columns in both login and logout dataframes
data <- pharmalogin[, colSums(is.na(pharmalogin))<nrow(pharmalogin)]
data2 <-pharmalogout[, colSums(is.na(pharmalogout))<nrow(pharmalogout)]

#convert from wide format to long format
pharmloginlong <- melt(setDT(data),id.vars='EmployeeID',variable.name='day')
pharmlogoutlong <- melt(setDT(data2),id.vars='EmployeeID',variable.name='day')

#parse the dates for calculate hours
pharmloginlong$date <- parse_date_time(pharmloginlong$value,'dmY HMS')
pharmlogoutlong$date <- parse_date_time(pharmlogoutlong$value, 'dmy HMS' )

# remove na rows
pharmloginlong <- na.omit(pharmloginlong)
pharmlogoutlong<-na.omit(pharmlogoutlong)


#extract the day variable for dataframe merging
pharmloginlong$day2 <-str_sub(pharmloginlong$day,3,7)
pharmlogoutlong$day2 <- str_sub(pharmlogoutlong$day,4,8)

#remove redundant columns
pharmloginlong <-pharmloginlong[,-2:-3]
pharmlogoutlong <- pharmlogoutlong[,-2:-3]

#merge two dataframes based o Employee Id and day
pharm <- merge(pharmloginlong,pharmlogoutlong, by= c('EmployeeID','day2'))
pharm$hours <- pharm$date.y- pharm$date.x


pharm$overtime <- pharm$hours-8

#get total overtime hours
pharmovertime <- pharm %>% group_by(EmployeeID) %>% summarise(overtime = sum(overtime))
pharmovertime$overtime <- trunc(pharmovertime$overtime)

