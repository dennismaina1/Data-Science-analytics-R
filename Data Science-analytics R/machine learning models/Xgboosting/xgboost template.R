# xg bootsing

install.packages("xgboost")
install.packages('Rtools')
library(xgboost)

#convert dependent variable to boolean

trainy <- Train$Start_Tech_Oscar=="1"
testy <- Test$Start_Tech_Oscar=="1"

#convert independent variables to dummy data

trainx <- model.matrix(Start_Tech_Oscar~.-1,data=Train)
testx <- model.matrix(Start_Tech_Oscar~.-1,data = Test)

trainx <- trainx[,-12]
testx <- testx[,-12]

#dmatrix

xmatrix <- xgb.DMatrix(data = trainx, label=trainy)
xmatrixt <-xgb.DMatrix(data = testx, label=testy)



#model 
#eta learning rate 0-1, lower value slow learning and more fitting and vice versa
#nrounds iterations of the model on data: rule of thumb keep low value of eta and high value of nrounds
#numclass number of classes the model should produce
#max_depth control tree growth
#object : type of ml to be performed
# reg:linear - linear regression
# reg:logistic - logistic regression
# multi:softmax -multiple class classification

xgboost <- xgboost(data =xmatrix, nrounds = 50,eta=.4, objective="multi:softmax",num_class=2,max_depth=4 )

#predict fit 
predictxgboost  <-predict(xgboost,xmatrixt)

#confusion matrix
table(predictxgboost,Test$Start_Tech_Oscar)

