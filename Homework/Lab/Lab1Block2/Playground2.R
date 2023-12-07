###########################  Init code #########################################
rm(list = ls())
library(randomForest)

###############  Assignment 1 Ensemble methods #################################

compare_method1 <- function(x1, x2) {
  return(x1 < x2)
}

compare_method2 <- function(x1, x2) {
  return(x1 < 0.5)
}

compare_method3 <- function(x1, x2) {
  return((x1 > 0.5 & x2 > 0.5) | (x1 < 0.5 & x2 < 0.5))
}

ensemble_method <- function(record_number, tree_number, node_size, compare_method) {
  # init data set (copy from pdf file)
  set.seed(1234)

  x1 <- runif(record_number)
  x2 <- runif(record_number)
  trdata <- cbind(x1, x2)
  y <- as.numeric(compare_method(x1, x2))
  trlabels <- as.factor(y)
  #plot(x1, x2, col = (y + 1))

  # training the model using random forest
  rf_model <- randomForest(trdata, trlabels, ntree = tree_number,
                          nodesize = node_size, keep.forest = TRUE)

  rf_pred <- predict(rf_model, newdata = trdata, class = "classification")

  # calculate the misclassification rate
  misclassification_rate <- sum(as.numeric(rf_pred != trlabels)) /
  length(trlabels)

  return (misclassification_rate)
}

################################################################################
# call the function
record_numbers <- c(100, 1000)
tree_numbers <- c(1, 10, 100)

node_size <- 25
# data record number = 100
result1 <- ensemble_method(record_numbers[1], tree_numbers[1], node_size, 
                          compare_method1)
result2 <- ensemble_method(record_numbers[1], tree_numbers[2], node_size, 
                          compare_method1)
result3 <- ensemble_method(record_numbers[1], tree_numbers[3], node_size, 
                          compare_method1)

cat("The misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", result1, "\n")
cat("The misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", result2, "\n")
cat("The misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", result3, "\n")
################################################################################

misclassification_rates_1 <- rep(0, 1000)
misclassification_rates_10 <- rep(0, 1000)
misclassification_rates_100 <- rep(0, 1000)

# data record number = 100
for (i in 1:1000) {

  # data record number = 100
  misclassification_rates_1[i] <- ensemble_method(record_numbers[1], 
                                                 tree_numbers[1], node_size, 
                                                 compare_method1)
  misclassification_rates_10[i] <- ensemble_method(record_numbers[1], 
                                                  tree_numbers[2], node_size, 
                                                  compare_method1)
  misclassification_rates_100[i] <- ensemble_method(record_numbers[1], 
                                                   tree_numbers[3], node_size, 
                                                   compare_method1)
}

cat("The mean of misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_1), "\n")

cat("The mean of misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_10), "\n")

cat("The mean of misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_100), "\n")

cat("The var of misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_1), "\n")

cat("The var of misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_10), "\n")

cat("The var of misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_100), "\n")
################################################################################

misclassification_rates_1 <- rep(0, 1000)
misclassification_rates_10 <- rep(0, 1000)
misclassification_rates_100 <- rep(0, 1000)

# data record number = 100
for (i in 1:1000) {

  # data record number = 100
  misclassification_rates_1[i] <- ensemble_method(record_numbers[1], 
                                                 tree_numbers[1], node_size, 
                                                 compare_method2)
  misclassification_rates_10[i] <- ensemble_method(record_numbers[1], 
                                                  tree_numbers[2], node_size, 
                                                  compare_method2)
  misclassification_rates_100[i] <- ensemble_method(record_numbers[1], 
                                                   tree_numbers[3], node_size, 
                                                   compare_method2)
}

cat("The mean of misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_1), "\n")

cat("The mean of misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_10), "\n")

cat("The mean of misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_100), "\n")

cat("The var of misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_1), "\n")

cat("The var of misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_10), "\n")

cat("The var of misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_100), "\n")

################################################################################

misclassification_rates_1 <- rep(0, 1000)
misclassification_rates_10 <- rep(0, 1000)
misclassification_rates_100 <- rep(0, 1000)

node_size <- 12
# data record number = 100
for (i in 1:1000) {

  # data record number = 100
  misclassification_rates_1[i] <- ensemble_method(record_numbers[1], 
                                                 tree_numbers[1], node_size, 
                                                 compare_method3)
  misclassification_rates_10[i] <- ensemble_method(record_numbers[1], 
                                                  tree_numbers[2], node_size, 
                                                  compare_metho3)
  misclassification_rates_100[i] <- ensemble_method(record_numbers[1], 
                                                   tree_numbers[3], node_size, 
                                                   compare_method3)
}

cat("The mean of misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_1), "\n")

cat("The mean of misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_10), "\n")

cat("The mean of misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    mean(misclassification_rates_100), "\n")

cat("The var of misclassification rate for 1 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_1), "\n")

cat("The var of misclassification rate for 10 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_10), "\n")

cat("The var of misclassification rate for 100 tree with record_number=",
    record_numbers[1], " and node_size=", node_size, " is: ", 
    var(misclassification_rates_100), "\n")

################################################################################





###############  Assignment 2 MIXTURE MODELS ###################################

###########################  Init code #########################################
rm(list = ls())

# init data set (copy from pdf file)
set.seed(1234567890)

# max number of EM iterations
max_it <- 5

# min change in log lik between two consecutive iterations
min_change <- 0.1

# number of training points
n <- 1000

# number of dimensions
D <- 10

# training data
x <- matrix(nrow = n, ncol = D)

# true mixing coefficients
true_pi <- vector(length = 3)

# true conditional distributions
true_mu <- matrix(nrow = 3, ncol = D)

true_pi <- c(1 / 3, 1 / 3, 1 / 3)

true_mu[1, ] <- c(0.5, 0.6, 0.4, 0.7, 0.3, 0.8, 0.2, 0.9, 0.1, 1)
true_mu[2, ] <- c(0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8, 0.1, 0.9, 0)
true_mu[3, ] <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

plot(true_mu[1, ], type = "o", col = "blue", ylim = c(0, 1))
points(true_mu[2, ], type = "o", col = "red")
points(true_mu[3, ], type = "o", col = "green")

# Producing the training data
for(i in 1:n) {
  m <- sample(1:3, 1, prob = true_pi)
  for(d in 1:D) {
    x[i, d] <- rbinom(1, 1, true_mu[m, d])
  }
}

M = 3 # number of clusters
w <- matrix(nrow = n, ncol = M) # weights
pi <- vector(length = M) # mixing coefficients
mu <- matrix(nrow = M, ncol = D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations

# Random initialization of the parameters
pi <- runif(M, 0.49, 0.51)
pi <- pi / sum(pi)
for (m in 1:M) {
  mu[m, ] <- runif(D, 0.49, 0.51)
}

pi
mu

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  #Sys.sleep(2)

  # E-step: Computation of the weights
  for (i in 1:n) {
    for (m in 1:M) {
      w[i, m] <- pi[m] * prod(mu[m,]^x[i,] * (1 - mu[m,])^(1 - x[i,]))
    }
    w[i,] <- w[i,] / sum(w[i,])
  }


  #Log likelihood computation.
  llik[it] <- sum(log(apply(w, 1, function(row) sum(row * pi))))

  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()

  # Stop if the lok likelihood has not changed significantly
  if (it > 1 && abs(llik[it] - llik[it - 1]) < min_change) {
    cat("Converged.\n")
    break
  }

  #M-step: ML parameter estimation from the data and weights
  for (m in 1:M) {
    pi[m] <- sum(w[, m]) / n
    mu[m, ] <- colSums(x * w[, m]) / sum(w[, m])
  }
}

pi
mu
plot(llik[1:it], type = "o")
