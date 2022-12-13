rm(customer,product,a,b,c,d,x,y,z)

install.packages("readxl")
cardata <- read_excel("F:\\Dennis\\Learning\\machine learning\\data files\\Data Files\\Linear Regression Dataset\\Cardata.xlsx")

summary(cardata)

#disp, hp, wt have are all exhibiting logarithimic relationship to mpg. convert all to linear

cardata$disp <- log(cardata$disp)

cardata$HP <- log(cardata$HP)

cardata$wt <- log(cardata$wt)

pairs(~cyl+mpg, data = cardata)

#check for correlation between independent variables

round(cor(cardata),3)

#correlation between disp and wt, disp and cyl both 0.943, checking correlation with mpg, remove cyl & disp

cardata <- cardata[-1]

#run a multiple linear regression model
multiple_model <- lm(mpg~., data = cardata)

summary(multiple_model)



