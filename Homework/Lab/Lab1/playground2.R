###########################  Init code For Assignment 2 ########################
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)

###########################  Common Functions ##################################
# normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

###########################  Assignment 2.1 ####################################
# Load the data
data <- read.csv("./Homework/Lab/Lab1/parkinsons.csv")
#data <- read.csv("./Homework/Lab/Lab1/optdigits.csv")


# remove useless data
data <- data[,-(1:4)]
data <- data[,-(2)]

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
preProcValues <- preProcess(train_set, method = c("scale"))
train_set <- predict(preProcValues,train_set)
test_set <- predict(preProcValues,test_set)

###########################  Assignment 2.2 ####################################
# Linear regression model

# apply linear regression
model <- lm(motor_UPDRS ~ ., data = train_set)

# predict the test value using the linear regression just created
test_pred <- predict(model, test_set)

# calculate test MSE
test_mse <- mean((test_pred - test_set$motor_UPDRS)^2)
model

###########################  Assignment 2.3 ####################################
# Loglikelihood function
loglikelihood <- function(theta, sigma) {
  n <- length(Y)
  log_likelihood_values <- -0.5 * (log(2 * pi * sigma^2) + ((Y - theta %*% X) / sigma)^2)
  return(total_log_likelihood <- sum(log_likelihood_values))
}

# ridge
ridge <- function(param) {
  param_length <- length(param)
  theta <- param[1:param_length-2]
  sigma <- param[param_length-1]
  lambda <- param[param_length] 
  loglikelihood_result <- loglikelihood(theta, sigma)
  ridge_penalty <- lambda * sum(theta^2)
  return(loglikelihood_result - ridge_penalty)
}

# ridgeopt
ridgeopt <- function(theta, sigma, lambda) {
  size_theta <- length(theta)
  param <- rep(0,size_theta+2)
  for(i in 1:size_theta){
    param[i] <- theta[i]
  }
  param[size_theta + 1] <- sigma
  param[size_theta + 2] <- lambda
  ridge_result <- optim(par = param, fn = ridge, method="BFGS")
  
}

# df
df <- function(X,lambda) {
  n <- nrow(X)
  # calculate hat_matrix
  hat_matrix <- X %*% solve(t(X) %*% X + lambda * diag(ncol(X)))
  # get degree of the hat_matrix
  df_ridge <- sum(diag(hat_matrix))
  return(df_ridge)
}

###########################  Assignment 2.4 ####################################

#sigma <- 0.01
#theta <- rep(1,21)
#X <- train_set[] 
#lambda <- 1
#ridgeopt(theta,sigma,lambda)

#lambda <- 100
#ridgeopt(theta,sigma,lambda)

#lambda <- 1000
#ridgeopt(theta,sigma,lambda)