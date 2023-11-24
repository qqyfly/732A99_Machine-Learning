###########################  Init code #########################################
rm(list = ls())
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

test_id <- setdiff(1:row_num, train_id)
test_set <- data[test_id, ]


######################  Assignment 1.1 #########################################
selected_x_columns <- grep(paste0("^", "Channel"), names(train_set), value = TRUE)
X_data_train <- train_set[, selected_x_columns]
# do not add intercept, since it will add automatically
#X_data_train <- cbind(1, X_data_train)
#names(X_data_train)[1]) <- "Intercept"

Y_data_train <- train_set[names(train_set) == "Fat"]

train_data_set <- cbind(X_data_train, Y_data_train)

# fit linear model
lm_model <- lm(Fat ~ ., data = train_data_set)
summary(lm_model)

predicted_fat1 <- predict(lm_model, train_data_set)
train_mse1 <- mean((predicted_fat1 - train_data_set$Fat)^2)

# predict the test set
X_data_test <- test_set[, selected_x_columns]

X_data_test <- cbind(1, X_data_test)
Y_data_test <- test_set[names(test_set) == "Fat"]

test_data_set <- cbind(X_data_test, Y_data_test)

predicted_fat <- predict(lm_model, test_data_set)

test_mse <- mean((predicted_fat - test_data_set$Fat)^2)
# This will output 722.4294

# TODO: try to calculate the underlying probabilistic model


# TODO: comment on the data and the model quality.

######################  Assignment 1.2 #########################################
# convert to matrix
X_data_train <- as.matrix(X_data_train)

cv_model <- cv.glmnet(x=X_data_train, y=Y_data_train, alpha = 1)

######################  Assignment 1.3 #########################################


######################  Assignment 1.4 #########################################


######################  Assignment 1.5 #########################################
