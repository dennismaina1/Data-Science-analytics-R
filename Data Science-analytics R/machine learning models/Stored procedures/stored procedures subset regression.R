
#get formula based on Id from adjr2 cp bic

# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
model_formula <- function(id,object,outcome){
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


#plot for bic, cp, rss, adjr2
data_plot <- function(object){
  
  rss<-summary(object)$rss
  rsq <- summary(object)$rsq
  bic <- summary(object)$bic
  cp <- summary(object)$cp
  adjrs <- summary(object)$adjr2
  # Set up a 2x2 grid so we can look at 4 plots at once
  par(mfrow = c(2.5,2.5))
  plot(rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  rss_min = which.min(rss) 
  abline(v=rss_min,col="red",lty=2)
  
  plot(rsq, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
  
  
  
  # We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
  # The which.max() function can be used to identify the location of the maximum point of a vector
  adj_r2_max = which.max(adjrs) 
  abline(v=adj_r2_max,col="red",lty=2)
  
  # We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
  plot(cp, xlab = "Number of Variables", ylab = "CP", type = "l")
  cp_min = which.min(cp) 
  abline(v=cp_min,col="red",lty=2)
  
  plot(bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  bic_min = which.min(bic) 
  abline(v=bic_min,col="red",lty=2)
  
}


#predict values using reg subsets using models with different number of input variables
predict.regsubsets <- function(object,data,id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,data)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

