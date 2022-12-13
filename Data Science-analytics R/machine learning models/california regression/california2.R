rm(practice1,Test_data,Train_data,ll,mse_test,mse_training,split,test_fit,train_fit,ul,lm_train)

california <- read.csv("F:/Dennis Personal Files/Learning/machine learning/california/housing.csv", header = TRUE)


#null value treatment
summary(california)

mean(california$total_bedrooms, na.rm = TRUE)

california$total_bedrooms[is.na(california$total_bedrooms)]<-mean(california$total_bedrooms, na.rm = TRUE)

#conditional data treatment

california2 <- dummy.data.frame(california)

barplot(table(california$ocean_proximity))

california2 <- california2[-12]


#outlier treatment
#population
plot(california2$median_house_value,california2$population)

ulCP <- 2.5* quantile(california2$population, .99)

california2$population[california2$population>ulCP]<- ulCP

#households
plot(california2$households, california2$median_house_value)

quantile(california2$households, .99)

(quantile(california2$households, .99))*1.5

abline(v=(quantile(california2$households, .99))*2, col="red")

ulCH <-(quantile(california2$households, .99))*2

california2$households[california2$households>ulCH]<- ulCH

#total rooms
plot(california2$total_rooms,california2$median_house_value)

quantile(california2$total_rooms, .99)

abline(v=quantile(california2$total_rooms, .99)*2, col = "red")

ulCR <- quantile(california2$total_rooms, .99)*2

california2$total_rooms[california2$total_rooms>ulCR]<- ulCR


#total bedrooms
plot(california2$total_bedrooms, california2$median_house_value)

quantile(california2$total_bedrooms, .99)

abline(v=quantile(california2$total_bedrooms, .99)*1.5, col="green")

ulCTB <- quantile(california2$total_bedrooms, .99)*1.5

california2$total_bedrooms[california2$total_bedrooms>ulCTB]<-ulCTB


round(cor(california2),3)

#remove correlated data
california2 <- california2[-5]


#linear regression

linear_model <- lm(median_house_value~., data = california2)
summary(linear_model)

#best subset regression
#best subset method

best_subest <- regsubsets(median_house_value~., data = california2, nvmax =11)
summary(best_subest)$adjr^2

which.max(summary(best_subest)$adjr^2)

coefficients(best_subest,9)

#foward step wise
foward_step <- step(linear_model, direction = "forward")

summary(foward_step)


#logistic regression

glm_model <- glm(median_house_value~., data = california2)

summary(glm_model)

lda <- lda(median_house_value~., california2)

predictions <- predict(lda, california2)
summary(predictions)$class






