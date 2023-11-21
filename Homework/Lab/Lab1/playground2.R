##################################  Init Code ##################################
rm(list = ls())

###########################  Common Functions ##################################
# normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 2A DONE
###########################  Assignment 2.1 ####################################
# Load the data
data <- read.csv("./Homework/Lab/Lab1/parkinsons.csv")
row_num <- nrow(data)
cols_num <- ncol(data)

# Divide the data into training and test data (60/40)
# set data split ratio
ratio <- c(train = .6, test = .4)

# set random seed
set.seed(12345)

# split data
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_set <- data[train_id, ]
test_id <- setdiff(1:row_num, train_id)
test_set <- data[test_id, ]

# normalize data.
train_set <- as.data.frame(lapply(train_set, normalize))
test_set <- as.data.frame(lapply(test_set, normalize))

###########################  Assignment 2.2 ####################################
# Linear regression model

# apply linear regression
model <- lm(motor_UPDRS ~ ., data = train_set)

# predict the test value using the linear regression just created
test_pred <- predict(model, test_set)

# calculate test MSE
test_mse <- mean((test_pred - test_set$motor_UPDRS)^2)


###########################  Assignment 2.3 ####################################
# Loglikelihood function
loglikelihood <- function(theta, sigma, x) {
  return(sum(theta * log(sigma) + (1 - theta) * log(1 - sigma)))
}

ridge <- function(theta, sigma, lambda) {
}

ridgeopt <- function(x, y, lambda) {
}

df <- function() {
}

###########################  Assignment 2.4 ####################################
# Compute optimal $\theta$ parameter for $\lambda=1,\lambda=100,\lambda=1000$

# predict the motor_UPDRS values for training and test data and report the 
# training and test MSE values. 

# Which penalty parameter is most appropriate among the selected ones? 

# Compute and compare the degrees of freedom of these models and make 
# appropriate conclusions

# TODO: Finish all of them
