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
loglikelihood <- function(x,y, theta, sigma) {
  n <- length(x)
  log_likelihood_value <- -0.5 * (n * log(2 * pi * sigma^2)) -
                          sum((t(theta) * x - y)^2) / (2 * sigma^2)
  return(log_likelihood_value)
}

# ridge
ridge <- function(par,x,y,lambda) {

  param_length <- length(par)

  theta <- par[1:param_length - 1]
  sigma <- par[param_length]

  loglikelihood_result <- loglikelihood(x,y,theta, sigma)
  ridge_penalty <- lambda * sum(theta^2)
  return(loglikelihood_result - ridge_penalty)
}

# ridgeopt
ridgeopt <- function(initial_values, x,y,lambda ) {
  # Use optim function
  
  result <- optim(par = initial_values, ridge, method = "BFGS",
                  x = x, y = y,lambda = lambda)
  return(result)
}

# df
df <- function(x, y, theta, sigma) {
  hat_y <- t(theta) %*% x
  return(sum(cov(hat_y, y)) / sigma^2)
}

###########################  Assignment 2.4 ####################################

x <- train_set[,-1]
y <- train_set$motor_UPDRS


#lambda <- 1
theta <- rep(1, ncol(x))
sigma <- 1
initial_values <- c(theta, sigma)
result1 <- ridgeopt(initial_values=initial_values, x = x, y=y,lambda = 1 )

#lambda <- 100
theta <- rep(1, ncol(x))
sigma <- 1
initial_values <- c(theta, sigma)
result100 <- ridgeopt(initial_values=initial_values, x = x, y=y,lambda = 100 )

#lambda <- 1000
theta <- rep(1, ncol(x))
sigma <- 1
initial_values <- c(theta, sigma)
result1000 <- ridgeopt(initial_values=initial_values, x = x, y=y,lambda = 1000 )

cat("lambda = 1, theta = ", result1$par[1:ncol(x)], "sigma = ", result1$par[ncol(x) + 1], "\n")
cat("lambda = 100, theta = ", result100$par[1:ncol(x)], "sigma = ", result100$par[ncol(x) + 1], "\n")
cat("lambda = 1000, theta = ", result1000$par[1:ncol(x)], "sigma = ", result1000$par[ncol(x) + 1], "\n")



