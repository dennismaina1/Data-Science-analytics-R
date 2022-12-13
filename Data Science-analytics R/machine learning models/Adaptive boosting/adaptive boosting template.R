
#adaptive boosting


movie <- read.csv("F:/Dennis Personal Files/Learning/machine learning/datasets/Files/Movie_classification.csv",header=TRUE)

movie$Time_taken[is.na(movie$Time_taken)]<- mean(movie$Time_taken,na.rm = TRUE)

library(caTools)
set.seed(0)

split <- sample.split(movie,.8)

Train <- subset(movie, split==TRUE)
Test <- subset(movie,split==FALSE)

#install and initialize necessary libraries
install.packages('adabag')
library(adabag)

#change response variable to categorical
Train$Start_Tech_Oscar <- as.factor(Train$Start_Tech_Oscar)
#model
adaboost <- boosting(Start_Tech_Oscar~., Train, boos = TRUE)

#predict accuracy
predict <- predict.boosting(adaboost,Test)

#confusion matrix
table(Test$Start_Tech_Oscar,predict$class)



