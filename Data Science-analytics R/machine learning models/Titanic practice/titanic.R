rm(df2)

titanic <- read.csv('F:/Dennis Personal Files/Learning/machine learning/datasets/titanic.csv',header=TRUE)

summary(titanic)

age <- titanic$age
who <- titanic$who

#preprocessing  the NA's in age

#get data to a new data frame
dataframe1 <- data.frame(age,who)

#get unique values in r
unique(dataframe1$who)

man <-subset(dataframe1,who=="man")
woman <-subset(dataframe1,who=="woman")
child <-subset(dataframe1,who=="child")

#replace NAs with range from -5,+5 of mean age
mean(man$age,na.rm = TRUE) #mean is 33
values <- seq(28,38,1)
man[is.na(man)] <- sample(values, sum(is.na(man)), replace = TRUE) #method 1


#replace NAs with range from -5,+5 of mean age
mean(woman$age,na.rm = TRUE) #mean is 32
values <- seq(27,37,1)
woman[is.na(woman)] <-  sample(sum(is.na(woman)), replace=TRUE) #method2

#concatinate the dataframes 
dataframe2 <- rbind(man,woman,child)
dataframe2$age <- lapply(dataframe2$age,as.integer)

#rearange dataframe
d <- dataframe2
d$index <- as.numeric(row.names(d))
dataframe2<- d[order(d$index),]

dataframe2 <- dataframe2[-3]
titanic$age <-dataframe2$age 
rm(dataframe1,dataframe2,d,df,man,woman,child)

#stats
#women in 3rd class
#women in 3rd class that survived
#men who boarded at Southhampton
#boarding by city
#average fare 1st 2nd 3rd

summary(titanic)

titanic <- titanic[-13]
df <- data.frame(titanic$pclass , titanic$who)

names(df)[1]<-"class"
names(df)[2]<-"who"

#women in 3rd class
count (subset.data.frame(df,class=="3" & who=="woman"))
#women in 3rd class that survived
count(subset.data.frame(titanic,who=="woman" & pclass=="1" & survived=="1"))
#men in southhampton
count(subset.data.frame(titanic,embark_town=="Southampton" & who=="man"))
#boarding by city
table(titanic$embark_town)
#mean fare
data <- titanic %>% group_by(pclass) %>% summarise(fare=mean(fare),town=unique(embark_town),.groups = 'drop')
#prediction of survival

which(is.na(titanic))

titan <- data.frame(titanic[,2:5],titanic[,11])
titan <-titan[,-3]
names(titan)[4]<-'who'

titan$age <- as.numeric(titan$age)

set.seed(0)
library(caTools)

split <- sample.split(titan,.8)
Train <- subset(titan, split==TRUE)
Test <- subset(titan,split== FALSE)



#logisitcal
logistic <- glm(survived~.,data = Train,family = "binomial")

pred <- predict(logistic,Test,type = "response")

class[pred>.9]<-"1"

table(class,Test$survived)

#lda

library(MASS)

lda <- lda(survived~.,data = Train)

pred2 <- predict(lda,Test)

class2 <- pred2$class

table(class2,Test$survived)


#knn
library(class)
library(dummies)
titan2 <- dummy.data.frame(titan)

set.seed(0)
library(caTools)

split <- sample.split(titan2,.8)
Train2 <- subset(titan2, split==TRUE)
Test2 <- subset(titan2,split== FALSE)


trainx <- Train2[-1]
testx<- Test2[-1]
trainy <- Train2$survived
testy <- Test2$survived

trainxs <-scale(trainx)
testxs <- scale(testx)

library(class)

knn1 <- knn(trainxs,testxs,trainy,k=15)
table(knn1,testy)


#decision tree
library("rpart")
library(rpart.plot)


desctree <- rpart(formula = survived~.,data = Train,control = rpart.control(maxdepth = 3),method = "class" )

pred3 <- predict(desctree,Test,type = "class")

table(pred3,Test$survived)

rpart.plot(desctree,digits = -3)

