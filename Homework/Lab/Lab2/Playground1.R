###########################  Init code #########################################
rm(list = ls())
library(plyr)
library(readr)

library(dplyr)

library(caret)
library(ggplot2)
library(repr)


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
selected_x_columns <- grep(paste0("^", "Channel"), names(train_set), value = TRUE)
X_data_train <- train_set[, selected_x_columns]
Y_data_train <- train_set[names(train_set) == "Fat"]
X_data_test <- test_set[, selected_x_columns]
Y_data_test <- test_set[names(test_set) == "Fat"]

train_data_set <- cbind(X_data_train, Y_data_train)
test_data_set <- cbind(X_data_test, Y_data_test)

# fit linear model
lm_model <- lm(Fat ~ ., data = train_data_set)
summary(lm_model)

predicted_train_fat <- predict(lm_model, train_data_set)
train_mse <- mean((predicted_train_fat - train_data_set$Fat)^2)

# this will generate 0.005709117
cat("train_mse: ", train_mse, "\n")

predicted_test_fat <- predict(lm_model, test_data_set)
test_mse <- mean((predicted_test_fat - test_data_set$Fat)^2)

# this will generate 722.4294
cat("test_mse: ", test_mse, "\n")

# TODO: try to calculate the underlying probabilistic model

# TODO: comment on the data and the model quality.

######################  Assignment 1.2 DONE #########################################
# the cost function in lasso regression is:
#$$ 
#  MSE(y,y_pred) + \alpha * \sum_{i=1}^{n} |\theta_i|
#$$
######################  Assignment 1.3 #########################################

# Fit the lasso regression model to the training data
#X_data_train <- as.matrix(X_data_train)

#cv_model <- cv.glmnet(x=X_data_train, y=Y_data_train, alpha = 1)

pre_proc_val <- preProcess(train_data_set, method = c("center", "scale"))
train_data_set <- predict(pre_proc_val, train_data_set)
test_data_set <- predict(pre_proc_val, test_data_set)

new_cols_num <- ncol(train_data_set)

X_data_train <- train_data_set[, 1:(new_cols_num - 1)]
Y_data_train <- train_data_set[, new_cols_num]

# set range of lambda, we can know the range of lambda, then set it manually
grid <- 10^seq(10, -2, length = 100)

# when alpha = 1, it is lasso regression
lasso_reg <- glmnet(X_data_train, Y_data_train, alpha = 1, lambda = grid)

summary(lasso_reg)

# according to the output, we know that lambda range from 0 to 1e10
plot(lasso_reg)


coef(lasso_reg)[,lasso_reg$lambda]

a <- coef(lasso_reg)

#dummies <- dummyVars(Fat ~ ., data = train_data_set)

#train_dummies <- predict(dummies, newdata = train_data_set)
#test_dummies <- predict(dummies, newdata = test_data_set)

#cv_model <- cv.glmnet(x=train_dummies, y=train_data_set$Fat, alpha = 1)


######################  Assignment 1.4 #########################################

# the cost function in ridge regression is:

#$$
#  MSE(y,y_pred) + \alpha * \sum_{i=1}^{n} (\theta_i)^2
#$$

# when alpha = 0, it is ridge regression
ridge_reg <- glmnet(X_data_train, Y_data_train, alpha = 0, lambda = grid)

summary(ridge_reg)

# according to the output, we know that lambda range from 0 to 1e10
plot(ridge_reg)

######################  Assignment 1.5 #########################################
