install.packages("tidyverse")

library(tidyverse)
library(caret)
library(leaps)

movie <- read.csv("F:/Dennis Personal Files/Learning/machine learning/datasets/data files/Data Files/Linear Regression Dataset/Movie_collection_train.csv", header = TRUE)

summary(movie)



plot(movie$Trailer_views,movie$Collection)

movie2 <- movie


#preprocessing (polynomial relationship)
movie2$Trailer_views <- poly(movie2$Trailer_views)
plot(movie2$Trailer_views,movie2$Collection)

plot(movie2$X3D_available,movie2$Collection)

#bar plot using ggplot
bar <- data.frame(
  
  x3d=movie$Genre,
  collection =movie$Collection)

ggplot(bar, aes(x=x3d, y=collection))+ geom_bar(stat = "identity")

#remove useless variable
tble <- table(movie2$MPAA_film_rating)
barplot(tble)
movie2 <- movie2[-17]

#outlier treatment
#marketing
quantile(movie2$Marketin_expense,.99)
plot(movie2$Marketin_expense,movie2$Collection)
ulm <- 1.5*quantile(movie2$Marketin_expense,.99)
movie2$Marketin_expense[movie2$Marketin_expense>ulm]<- ulm


#timetaken
plot(movie2$Time_taken,movie2$Collection)
summary(movie2)
mean(movie2$Time_taken, na.rm = TRUE)
movie2$Time_taken[is.na(movie2$Time_taken)]<-mean(movie2$Time_taken, na.rm = TRUE)
quantile(movie2$Time_taken,0.01)
lltime <- .9*quantile(movie2$Time_taken,0.01)
movie2$Time_taken[movie2$Time_taken<lltime]<- lltime

#twitter hastags
plot(movie2$Twitter_hastags,movie2$Collection)
quantile(movie2$Twitter_hastags,.99)
ultwit <- 1.2*quantile(movie2$Twitter_hastags,.99)
movie2$Twitter_hastags[movie2$Twitter_hastags>ultwit]<-ultwit

#average age
movie2 <- movie2[-19]
movie2$Average_age <- movie$Avg_age_actors
plot(movie2$Average_age,movie2$Collection)
quantile(movie2$Average_age,.01)
llage <- .8*quantile(movie2$Average_age,.01)
movie2$Average_age[movie2$Average_age<llage]<-llage

#dummy variable
movie3 <- dummy.data.frame(movie2)
movie3<- movie3[-12]
genre<- table(movie3$Genre)
barplot(genre)

#correlation check
data <- movie3
data2 <- round(cor(data),3)
diag(data2)<- NA
data3 <- which(data2>.9, arr.ind = TRUE)
data.frame(row_name = row.names(data2)[data3[,1]],column_name= colnames(data2)[data3[,2]])

#check correlation with dependent variable
round(cor(movie3$Lead_.Actor_Rating,movie3$Collection),2)
round(cor(movie3$Lead_Actress_rating,movie3$Collection),2)
round(cor(movie3$Producer_rating,movie3$Collection),2)
round(cor(movie3$Director_rating,movie3$Collection),2)
round(cor(movie3$Average_age,movie3$Avg_age_actors),2)
movie3 <- movie3[-18]
rm(Train,Test)

#test train split
library(caTools)
set.seed(0)
split <- sample.split(movie3,.8)
Train<-subset(movie3,split==TRUE)
Test <- subset(movie3,split==FALSE)


#OLS MODEL
ols <- lm(Collection~., data =Train)
summary(ols)
Test2 <- predict(ols,Test)
Train2 <- predict(ols,Train)
predictols<-predict(ols,movie3,type = "response")
MSE.ols.test <- mean((Test2-Test$Collection)^2)
MSE.ols.train <-mean((Train2-Train$Collection)^2)
Train<- Train[-17]

#subset regression
#best subset
bestsubset <- regsubsets(Collection~.,data = Train,nvmax = 17)
summary(bestsubset)$adjr2
which.max(summary(bestsubset$adjr2))
coefficients(best,13)

  
best <- summary(bestsubset)
data.frame(
  Adj.R2 = which.max(best$adjr2),
  CP = which.min(best$cp),
  BIC = which.min(best$bic),
  RSS=which.min(best$rss)
)

get_model_formula(id=4,object=bestsubset,outcome="Collection")

# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id,object,outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}




#backward stepwise
backward<- step(ols, direction = "backward")
summary(backward)$adjr2
coef(backward)


#predict values in test data
predict.regsubsets <- function(object,data,id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,data)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

predict.regsubsets()

#Ridge regression 
x <- model.matrix(Collection~., data = Train)[,-1]
x2 <- model.matrix(Collection~., data = Test)[,-1]
y <- Train$Collection
lambda =10^seq(10,-2,length=100)

ridge <- glmnet(x,y,lambda = lambda,alpha = 0)
lasso <- glmnet(x,y,lambda = lambda, alpha = 1)

cv_fit <- cv.glmnet(x,y,lambda = lambda,alpha= 0 )
cv_fit2 <- cv.glmnet(x,y,lambda = lambda,alpha= 1 )

plot(cv_fit)

opt_lambda <- cv_fit$lambda.min
opt_lambda.lass <- cv_fit2$lambda.min

predict.ridge <- predict(ridge, s=opt_lambda, newx = x2)
predict.lasso <- predict (lasso, s=opt_lambda.lass,newx = x2)


Tss <- sum((y-mean(y))^2)
Rss <- sum((y-mean(predict.ridge))^2)

rsq <- 1-(Rss/Tss)

tble1 <- data.frame(movie3$Collection,predict.ridge,predict.lasso,predictols)

