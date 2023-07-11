# set seed for reproducibility
set.seed(111)

# load required packages
library(dplyr)
library(caret)
library(readxl)
library(corrplot)

#load dataset
data <- read_excel("C:/Users/Dvarim/Desktop/arrest_data_assignment1.xlsx", sheet = "final")

# convert education to factor
data$education <- factor(data$education)

# create correlation matrix of all variables in the model
vars <- c("arrested", "financial_aid", "age", "num_prior_cov", "first_week_of_emp", "emp_length_num_weeks")
cor_matrix <- cor(train_data[vars])

corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# split the data into training and validation sets
train_index <- createDataPartition(data$arrested, p = 0.6, list = FALSE)
train_data <- data[train_index, ]
validation_data <- data[-train_index, ]

# fit the logistic regression model
#model <- glm(arrested ~ financial_aid + age + race + workexp + marital_status + parole_realease + num_prior_cov + education + employment_status + first_week_of_emp, data = train_data, family = binomial)

model <- glm(arrested ~ age + employment_status, data = train_data, family = binomial)

# summarize the model
summary(model)


# make predictions on training and validation sets
train_predictions <- predict(model, type = "response", newdata = train_data)
validation_predictions <- predict(model, type = "response", newdata = validation_data)

# Set the levels of the predicted factor to be the same as the levels of the actual data
predictions1 <- ifelse(train_predictions > 0.5, 1, 0)
predictions2 <- ifelse(validation_predictions > 0.5, 1, 0)

# Change values to factors
un_predictions1 <-factor(unname(predictions1), levels = c(0,1))
un_predictions2 <-factor(unname(predictions2), levels = c(0,1))
train_data$arrested <- factor(train_data$arrested, levels = c(0,1))
validation_data$arrested <- factor(validation_data$arrested, levels = c(0,1))


# Create confusion matrix for training set
train_confusion_matrix <- confusionMatrix(un_predictions1, train_data$arrested)
validation_confusion_matrix <- confusionMatrix(un_predictions2, validation_data$arrested)

# print confusion matrices
print(train_confusion_matrix)
print(validation_confusion_matrix)
