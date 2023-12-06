###########################  Init code #########################################
rm(list = ls())
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

# read data

data <- read.csv("./Homework/Lab/Lab2/tecator.csv")

row_num <- nrow(data)
cols_num <- ncol(data)


# set data split ratio to 0.5, 0.5
ratio <- c(train = .5, test = .5)

# set random seed
set.seed(12345)

# split data to training and test dataset
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_set <- data[train_id, ]
dim(train_set)

test_id <- setdiff(1:row_num, train_id)
test_set <- data[test_id, ]
dim(test_set)

######################  Assignment 1.1 #########################################
# DONE

# Underline probabilistic model:

# We set the parameter of the model is theta, then we have
# Fat = theta_0 + theta_1 * Channel_1 + ... + theta_100 * Channel_100
# We can rewrite the equation as:
# Fat = theta * Channel

# select all columns with Channel in the name
selected_x_columns <- grep(paste0("^", "Channel"), names(train_set), value = TRUE)

# Filter the data with selected columns and Fat
data <- data[, c(selected_x_columns, "Fat")]

# filter out all the useless columns in train and test dataset
X_data_train <- train_set[, selected_x_columns]
Y_data_train <- train_set[names(train_set) == "Fat"]
X_data_test <- test_set[, selected_x_columns]
Y_data_test <- test_set[names(test_set) == "Fat"]

train_data_set <- cbind(X_data_train, Y_data_train)
test_data_set <- cbind(X_data_test, Y_data_test)

# fit linear model
lm_model <- lm(Fat ~ ., data = train_data_set)
summary(lm_model)

# predict the training data
predicted_train_fat <- predict(lm_model, train_data_set)

# calculate the mean square error of the training data
train_mse <- mean((predicted_train_fat - train_data_set$Fat)^2)
# this will generate 0.005709117
cat("train mse is: ", train_mse, "\n")

# calculate the mean square error of the test data
predicted_test_fat <- predict(lm_model, test_data_set)
test_mse <- mean((predicted_test_fat - test_data_set$Fat)^2)
# this will generate 722.4294
cat("test mse is: ", test_mse, "\n")

# According to the output, we can see that the model is not good enough.
# The train MSE is 0.005709117, and the test MSE is 722.4294.
# It is actually over-fitting. Since the test MSE is much larger than the
# training MSE.

######################  Assignment 1.2 DONE ####################################
# the cost function in lasso regression is:
#$$
#  MSE(y,y_pred) + \alpha * \sum_{i=1}^{n} |\theta_i|
#$$

######################  Assignment 1.3 #########################################
# DONE

# Fit the lasso regression model to the training data

# we will regularize the data before we fit the model
pre_proc_val <- preProcess(train_data_set, method = c("center", "scale"))
train_data_set <- predict(pre_proc_val, train_data_set)
test_data_set <- predict(pre_proc_val, test_data_set)

new_cols_num <- ncol(train_data_set)

X_data_train <- train_data_set[, 1:(new_cols_num - 1)]

Y_data_train <- train_data_set[, new_cols_num]

X_data_train <- as.matrix(X_data_train)

# Check here
dummies <- dummyVars(Fat ~ ., data = data)

train_dummies <- predict(dummies, newdata = train_data_set)

test_dummies <- predict(dummies, newdata = test_data_set)

print(dim(train_dummies)); print(dim(test_dummies))

x <- as.matrix(train_dummies)
y_train <- train_data_set$Fat

x_test <- as.matrix(test_dummies)
y_test <- test_data_set$Fat

# set range of lambda, we can know the range of lambda, then set it manually
grid <- 10^seq(2, -3, by = -.1)

# when alpha = 1, it is lasso regression
lasso_reg <- glmnet(x, y_train, alpha = 1, family = "gaussian", lambda = grid)

summary(lasso_reg)

plot(lasso_reg, xvar = "lambda", label = TRUE)

# choose lambda to select only 3 features
# According to the plot of lasso_reg, we choose top number to 3 which contains 
# 3 curve and choose the appropriate lambda value , which is around -3 (Check)

######################  Assignment 1.4 #########################################
# Almost DONE, need add comments

# the cost function in ridge regression is:
#$$
#  MSE(y,y_pred) + \alpha * \sum_{i=1}^{n} (\theta_i)^2
#$$

# when alpha = 1, it is ridge regression
ridge_reg <- glmnet(x, y_train, alpha = 0, lambda = grid)

summary(ridge_reg)

plot(ridge_reg, xvar = "lambda", label = TRUE)

######################  Assignment 1.5 #########################################

#TODO: Add something here.

lasso_reg_cv <- cv.glmnet(x, y_train, alpha = 1, lambda = grid, 
             standardize = TRUE)


# Best 

lambda_best <- lasso_reg_cv$lambda.min 

lambda_best
plot(lasso_reg_cv)
