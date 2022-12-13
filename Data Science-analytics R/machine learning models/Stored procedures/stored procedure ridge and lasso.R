
function ridge(X,Y,newx){
library("glmnet")
x <- X
newx<-newx
y <- Y
lambda =10^seq(10,-2,length=100)

ridge <- glmnet(x,y,lambda = lambda,alpha = 0)

cv_fit <- cv.glmnet(x,y,lambda = lambda,alpha= 0 )

opt_lambda <- cv_fit$lambda.min

predict.ridge <- predict(ridge, s=opt_lambda, newx = newx)

Tss <- sum((y-mean(y))^2)
Rss <- sum((y-mean(predict.ridge))^2)

rsq ridge <- 1-(Rss/Tss)
}

function lasso(X,Y,newx){
  library("glmnet")
  x <- X
  newx<-newx
  y <- Y
  lambda =10^seq(10,-2,length=100)
  
  ridge <- glmnet(x,y,lambda = lambda,alpha = 0)
  
  cv_fit <- cv.glmnet(x,y,lambda = lambda,alpha= 0 )
  
  opt_lambda <- cv_fit$lambda.min
  
  predict.ridge <- predict(ridge, s=opt_lambda, newx = newx)
  
  Tss <- sum((y-mean(y))^2)
  Rss <- sum((y-mean(predict.ridge))^2)
  
  rsq ridge <- 1-(Rss/Tss)
  
}



