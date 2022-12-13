rm(datasplit,Spl)
movie <- read.csv("F:/Dennis Personal Files/Learning/machine learning/Rdata/datasets/Files/Movie_regression.csv", header = TRUE)

summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken, na.rm = TRUE)

library('rpart')
library('rpart.plot')


set.seed(0)
Split=sample.split(movie,.8)

train <- subset(movie,Split==TRUE)
test <- subset(movie,Split==FALSE)

#build constrained tree
tree <- rpart(formula=Collection~., data = train, control=rpart.control(maxdepth = 3))
#plot tree
plot <- rpart.plot(tree,digits = 3)


#pruning a tree
tree2 <- rpart(formula=Collection~., data = train, control=rpart.control(cp=0))
plot <- rpart.plot(tree2,digits = 3)

printcp(tree2)
plotcp(tree)

#prune parameter
mincp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#prune tree
prunedtree <- prune(tree2,mincp)
plot2 <- rpart.plot(prunedtree)


#predict
#tree with 3 levels
test$level3 <- predict(tree,test,type = "vector")
#fulltree
test$full <- predict(tree2,test,type = "vector")
#prunedtree
test$pruned <- predict(prunedtree,test, type = "vector")


#MSE


