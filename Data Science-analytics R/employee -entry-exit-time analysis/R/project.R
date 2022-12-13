#imports
library(RMySQL)
library(RMySQL)
library(janitor)
library(dplyr)
library(tidyverse)
library(reshape)
library(ggplot2)
library(caret)
library(mltools)
library(mlbench)
library(gam)
library(MASS)
library(caTools)
library(class)
library(data.table)
library(lubridate)
library(stringr)
library(dummies)


#connect to db
RMySQL::dbListTables(employee)
RMySQL::dbListFields(employee, 'employee_database')

#import data from db
rs <- RMySQL::dbSendQuery(employee, "select * from employee_database")
rs2 <- RMySQL::dbSendQuery(employee, 'select * from manager_evaluation')
rs3 <- RMySQL::dbSendQuery(employee, 'select * from employee_satisfaction')



PharmaEmployee <- fetch(rs, n=-1)
pharmamanagerevaluation <- fetch(rs2, n=-1)
pharmaEmployeesatisfatcion <- fetch(rs3, n=-1)


#import the data 
#change the path according to your file location
#pharmaEmployee <- read.csv('C:/Users/hp/Downloads/employee/PharmaHR_Employee_Database4.csv',header = TRUE)
pharmaEmployeesatisfatcion <-read.csv('C:/Users/hp/Downloads/employee/PharmaHR_employee_satisfaction_survey_data4.csv',header = TRUE)
pharmamanagerevaluation <- read.csv('C:/Users/hp/Downloads/employee/PharmaHR_manager_evaluation_data4(1).csv',header=TRUE)
pharmalogin <-read.csv('C:/Users/hp/Downloads/employee/PharmaHR_employee_login_time4.csv',header = TRUE)
pharmalogout <-read.csv('C:/Users/hp/Downloads/employee/PharmaHR_employee_logout_time4.csv',header = TRUE)


#cleaning date data

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
rm(pharm,pharmloginlong,pharmlogoutlong,data,data2,pharmalogin,pharmalogout)



#merge dataframes
Employee <- merge.data.frame(pharmaEmployeesatisfatcion,pharmaEmployee, by='EmployeeID')
Employee <- merge.data.frame(pharmamanagerevaluation,Employee, by='EmployeeID')
Employee <- merge(pharmovertime,Employee,by='EmployeeID')



#EDA
summary(Employee)


par(mfrow=c(2,2))
meltData <- melt(Employee2)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")


#PRE-PROCESSING

#clean column names
Employee <- clean_names(Employee)

#1. NA data treatement
Employee <- na.omit(Employee)

#outlier treatment: using user defined functions
Employee2 <- remove_outliers(Employee, c('years_with_curr_manager','years_at_company',
                                         'monthly_income','age',
                                         'years_since_last_promotion','total_working_years',
                                         'stock_option_level','performance_rating','overtime'))


#2. Feature Engineering

#2.1 Change categorical variables to factors
Employee2[sapply(Employee2, is.character)] <- lapply(Employee2[sapply(Employee2, is.character)], as.factor)

#2.2 one hot encoding
Employee2 <- Employee2[,-22]
dummy <- dummyVars('~ .', data = Employee2)
Employee3 <- data.frame(predict(dummy, newdata = Employee2))


#3 feature selection
#3.1 univariate feature selection using pycaret package and random forest classifier :takes time
set.seed(0)
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
rfWithFilter <- sbf(attrition.Yes~.,data = Employee3, sbfControl = filterCtrl)
rfWithFilter
predictors(rfWithFilter)


#3.2 select the best features
Employee5 <- Employee3 %>%  dplyr::select(all_of(predictors(rfWithFilter)))
Employee5 <- clean_names(Employee5)

#4 scale data
Employee5$overtime <- scale(Employee5$overtime)

#5 Test Train split
set.seed(0)
split <- sample.split(Employee5)
Train <- subset(Employee5,split==TRUE)
Test <- subset(Employee5,split==FALSE)


#6classification models (LDA,KNN,logistic)

#6.1.1 LDA
lda <- lda(attrition_no~.,data = Employee5)

#predLDA <- predict(lda,Test)

classLDA <- predLDA$class
#6.1.2 confusion matrix
table(classLDA,Test$attrition_no)
#6.1.3 accuracy around 86%
(20+655)/(20+655+11+76)




#6.2.1 knn (best predictor)
trainx <- Train[-7]
testx<- Test[-7]
trainy <- Train$attrition_no
testy <- Test$attrition_no

# values already small no need for scaling
#trainxs <-scale(trainx)
#testxs <- scale(testx)

#6.2.2 append error rate values for knn
dataframe<-data.frame()
for(i in c(1:20)){
  
  knn =knn(trainx,testx,trainy,k=i)
  error_rate <- mean(knn!=Test$attrition_no)
  dataframe = rbind(dataframe,c(i,error_rate))
  
  
  
}
names(dataframe)[1]<-'index'
names(dataframe)[2]<-"errorrate"


ggplot(data = dataframe, aes(x = index, y =errorrate )) +
  geom_line()+geom_point()+geom_text(aes(label=index),vjust = 1.3, hjust = .5,
                                     show.legend = FALSE)

#6.2.3 optimal va;ue of k= 1
knn1 <- knn(trainx,testx,trainy,k=1)

#6.2.4 confusion Matrix
table(knn1,testy)

#6.2.5 performance = 98%
(657+84)/(657+84+12+9)



#6.3.1 logistic
logistic <- glm(Attrition~.,data = Employee5, family = 'binomial')

predict <- predict(logistic,Test,type = "response")

predicted.classes <- ifelse(predict > 0.5, "1", "0")

#6.3.2 confusion matrix
table(predicted.classes,Test$Attrition)

#6.3.3 coefficients
summary(logistic)$coef


