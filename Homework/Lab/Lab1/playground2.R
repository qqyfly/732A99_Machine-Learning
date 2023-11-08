# normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

loglikelihood <- function(theta, sigma) {
  return(sum(theta * log(sigma) + (1 - theta) * log(1 - sigma)))
}

ridge <- function(theta, sigma, lambda) {
}

ridgeopt <- function(x, y, lambda) {
}

df <- function() {
}

data <- read.csv("./Homework/Lab/Lab1/parkinsons.csv")

# set data split ratio
ratio <- c(train = .6, test = .4)

row_num <- nrow(data)
cols_num <- ncol(data)

# set random seed
set.seed(12345)

# split data
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_set <- data[train_id, ]

test_id <- setdiff(1:row_num, train_id)
test_set <- data[test_id, ]

# normalize data
train_set <- as.data.frame(lapply(train_set, normalize))
test_set <- as.data.frame(lapply(test_set, normalize))

#apply linear regression
model <- lm(motor_UPDRS ~ ., data = train_set)

# test MSE
test_pred <- predict(model, test_set)
test_mse <- mean((test_pred - test_set$motor_UPDRS)^2)
print(test_mse)