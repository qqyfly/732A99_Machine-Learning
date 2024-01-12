rm(list = ls())
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)
library(rpart)
library(rpart.plot)

# set random seed
set.seed(12345)


# read data
data <- read.csv("tecator.csv")
data <- data %>% select(Fat,Channel1:Channel100)

row_num <- nrow(data)
# set data split ratio to 0.5, 0.5
ratio <- c(train = .5, test = .5)

# split data to training and test dataset
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_data_set <- data[train_id, ]

test_id <- setdiff(1:row_num, train_id)
test_data_set <- data[test_id, ]

# fit linear model
lm_model <- lm(Fat ~ ., data = train_data_set)

predicted_fat_test <- predict(lm_model, test_data_set)
predicted_fat_train <- predict(lm_model, train_data_set)

# calc the mean value
test_mse <- mean((predicted_fat_test - test_data_set$Fat)^2) 
train_mse <- mean((predicted_fat_train - train_data_set$Fat)^2) 
cat("test_mse is:",test_mse,"\n")
cat("train_mse is:",train_mse,"\n")

summary(lm_model)

###############

# regularize the data before we fit the model
pre_proc_val <- preProcess(train_data_set, method = c("center", "scale"))
train_data_set <- predict(pre_proc_val, train_data_set)
test_data_set <- predict(pre_proc_val, test_data_set)

x_train <- as.matrix(train_data_set %>% select(-Fat))
y_train <- as.matrix(train_data_set %>% select(Fat))

# when alpha = 1, it is lasso regression
lasso_reg <- glmnet(x_train, y_train, alpha = 1,family = "gaussian")

summary(lasso_reg)

plot(lasso_reg, xvar="lambda", xlab='Log Lambda')


lambda_deg_freedom <- data.frame(lambda = lasso_reg$lambda,
                                 deg_freedom = lasso_reg$df)

lambda_deg_freedom <- lambda_deg_freedom %>% arrange(desc(deg_freedom))

lambda_deg_freedom_num <- lambda_deg_freedom[which(lambda_deg_freedom$deg_freedom == 4),] %>% 
                        head(1) %>% pull(lambda)



#########

ridge_reg <- glmnet(x_train, y_train, alpha = 0,family = "gaussian")

lambda_deg_freedom <- data.frame(lambda = ridge_reg$lambda,
                                 deg_freedom = ridge_reg$df)

lambda_deg_freedom <- lambda_deg_freedom %>% arrange(desc(deg_freedom))

lambda_deg_freedom_num <- lambda_deg_freedom[which(lambda_deg_freedom$deg_freedom == 4),] %>% 
                        head(1) %>% pull(lambda)

plot(ridge_reg,xvar="lambda", xlab='Log Lambda')
