movie <- read.csv("F:/Dennis Personal Files/Learning/machine learning/datasets/Files/Movie_regression.csv",header=TRUE)

movie$Time_taken[is.na(movie$Time_taken)]<- mean(movie$Time_taken,na.rm = TRUE)

library(caTools)
set.seed(0)

split <- sample.split(movie,.8)

Train <- subset(movie, split==TRUE)
Test <- subset(movie,split==FALSE)

Train$Genre <- as.factor(Train$Genre)
Train$X3D_available <- as.factor(Train$X3D_available)

#gradient boosting
library(gbm)

set.seed(0)
gbm <- gbm(formula = Collection~.,data = Train,distribution ="gaussian",shrinkage = 1, n.trees = 1,verbose = F,interaction.depth = 6)

test1 <-predict.gbm(gbm,newdata = Test,n.trees = 2,type = 'link')

mse <- mean((Test$Collection-test1)^2)


