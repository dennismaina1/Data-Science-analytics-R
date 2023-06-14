library('dplyr')
library('tidyr')
library('tidymodels')
library("cowplot")
library('plotly')
library('ggplot2')
library('data.table')
library('ggmap')
library('sf')
library('reshape2')
library('caret')
library('yardstick')
library("randomForest")

#dataset
wind <- read.csv("/home/Desktop/Data-Science-analytics-R/david/Assignment 3/wind-turbine.csv", header = TRUE)

#1.EXPLORATORY DATA ANALYSIS
#summary
summary(wind)
#structure
str(wind)
#finding NA
sum(is.na.data.frame(wind))
#distribution numeric data
numeric_cols <- sapply(wind,is.numeric)
numeric_dataset <- wind[,numeric_cols]
hist_plots <- list()
for (col in colnames(numeric_dataset)) {
  p <- ggplot(numeric_dataset, aes(x = .data[[col]])) +
    geom_histogram() +
    labs(title = paste("Histogram of", col),
         x = col,
         y = "Count") +
    theme_minimal()
  hist_plots[[col]] <- p
}
combined_plot <- do.call(cowplot::plot_grid, hist_plots)
print(combined_plot)


#distribution character dataset
char_cols <- sapply(wind,is.character)
char_dataset <- wind[,char_cols]
#distribution by province
province <- char_dataset%>%group_by(province_territory)%>%summarise(Count = n())
ggplot(aes(x=province_territory,y=Count), data=province)+
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#distribution by manufacturer
manufacturer <- char_dataset%>%group_by(manufacturer)%>%summarise(Count = n())
ggplot(aes(x=manufacturer,y=Count), data=manufacturer)+
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#distribution by commissioning date
commission <- char_dataset%>%group_by(commissioning_date)%>%summarise(Count = n())
ggplot(aes(x=commissioning_date,y=Count), data=commission)+
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#2.PREPROCESSING
#removing NA data
wind <- subset.data.frame(wind, select = -c(notes))
wind <- drop_na(wind)

#outlier detection and removal
boxplots <- list()
for (col in colnames(numeric_dataset)) {
  p <- ggplot(numeric_dataset, aes(x = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", col),
         x = col,
         y = "Count") +
    theme_minimal()
  boxplots[[col]] <- p
}
combined_plot <- do.call(cowplot::plot_grid, boxplots)
print(combined_plot)


#OUTLIERS DETECTED IN HUB DIAMETER AND HUB HEIGHT

#hub height outlier removal inter quartile method
# Calculate the quartiles and IQR
Q1 <- quantile(wind$hub_height_m, 0.25)
Q3 <- quantile(wind$hub_height_m, 0.75)
IQR <- Q3 - Q1
# Define the threshold values for outlier detection
lower_threshold <- Q1 - 1.5 * IQR
upper_threshold <- Q3 + 1.5 * IQR
# Identify outliers based on the thresholds
outliers <- wind$hub_height_m < lower_threshold | wind$hub_height_m > upper_threshold
# Remove outliers from the dataset
wind<- wind[!outliers, ]

#feature encoding
wind$province_territory <- as.numeric(as.factor(wind$province_territory))
wind$manufacturer <- as.numeric(as.factor(wind$manufacturer))
wind$commissioning_date <-as.numeric(as.factor(wind$commissioning_date))

#feature selection(removing irrelevant features)
dataset <- subset.data.frame(wind, select = -c(model,turbine_number_in_project,project_name,objectid,turbine_identifier))

#finding most relevant features through correlation analysis
corrlation <-round(cor(dataset, method = "pearson"),2)
corrlation2 <- melt(corrlation)
ggplot(data = corrlation2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))
# Find the correlation of each variable with the target variable
cor_with_target <- corrlation[, "total_project_capacity_mw"]

# Sort the correlation values in descending order
cor_with_target_sorted <- sort(cor_with_target, decreasing = TRUE)

# Print the correlation values with the target variable
print(cor_with_target_sorted)

#turbne rated capacity is not a useful feature cor with target = 0
dataset <- subset.data.frame(dataset, select = -c(turbine_rated_capacity_k_w))

#recipe setup
# Set up the recipe with the relevant formula
formula <- as.formula("total_project_capacity_mw ~ rotor_diameter_m + hub_height_m +
                      manufacturer+ commissioning_date + latitude + longitude")

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(dataset$total_project_capacity_mw, p = 0.8, list = FALSE)
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]
# Define the hyperparameters grid

hyperparameters <- expand.grid(.mtry = c(2, 4, 6))

# Perform hyperparameter tuning with cross-validation using train function
model <- train(
  formula,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = hyperparameters
)


# Retrieve the best hyperparameters
best_mtry <- model$bestTune$mtry

# Retrain the model using the best hyperparameters
best_model <- randomForest(
  formula,
  data = train_data,
  mtry = best_mtry,
)

# Make predictions on the test data using the random forest model
test_data$predictions <- predict(best_model, test_data)

# Calculate evaluation metrics
mae <- mean(abs(test_data$predictions - test_data$total_project_capacity_mw))
mse <- mean((test_data$predictions - test_data$total_project_capacity_mw)^2)
rmse <- sqrt(mse)
r_squared <- cor(test_data$predictions, test_data$total_project_capacity_mw)^2
adjusted_r_squared <- 1 - (1 - r_squared) * (nrow(test_data) - 1) / (nrow(test_data) - ncol(test_data) - 1)



# Visualize feature importance
importance <- importance(best_model)
var_importance <- data.frame(Variables = row.names(importance), Importance = round(importance, 2))
var_importance <- var_importance[order(var_importance$Importance, decreasing = TRUE), ]
# Reset the row names to NULL
row.names(var_importance) <- NULL

# Bar plot of feature importance
ggplot(var_importance, aes(x = Variables, y = IncNodePurity, fill = IncNodePurity)) +
  geom_bar(stat = "identity") +
  labs(title = "Feature Importance",
       x = "Variables",
       y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  coord_flip()
