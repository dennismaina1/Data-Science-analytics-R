
california <- read.csv("F:/Dennis Personal Files/Learning/machine learning/california/housing.csv", header = TRUE)

summary(california)

plot(california$median_house_value,california$median_income )
