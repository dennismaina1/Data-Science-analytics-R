

movie <- read.csv("F:/Dennis Personal Files/Learning/machine learning/datasets/Files/Movie_classification.csv", header=TRUE)


movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm = TRUE)

set.seed(0)
library(caTools)

split <- sample.split(movie,.8)

Train <- subset(movie, split==TRUE)
Test <-subset(movie,split==FALSE)


#classification tree
library(rpart)
library(rpart.plot)

#classification tree
classtree <- rpart(Start_Tech_Oscar~.,data = Train,method = "class",control = rpart.control(maxdepth =3))

#plot
rpart.plot(classtree,digits = -3)

#predict
Test$predict <- predict(classtree,Test,type = "class")

#contigency table
table(Test$Start_Tech_Oscar,Test$predict)






