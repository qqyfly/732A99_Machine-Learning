###########################  Init code #########################################
rm(list = ls())
library(randomForest)
library(ggplot2)

###############  Assignment 1 Ensemble methods #################################

compare_method1 <- function(x1, x2) {
    return(x1 < x2)
}

compare_method2 <- function(x1, x2) {
    return(x1 < 0.5)
}

compare_method3 <- function(x1, x2) {
    return(x1 > 0.5 & x2 > 0.5)
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

    cat("The misclassification rate for current tree is: ",
      misclassification_rate, "\n")

    # TODO: change the method of MSE if necessary
    mean_error_rate <- mean(rf_model$err.rate[, 1])
    cat("The mean error rate for current tree is: ", mean_error_rate, "\n")
}

# call the function
record_numbers <- c(100, 1000)
tree_numbers <- c(1, 10, 100)

node_size <- 25
# data record number = 100
ensemble_method(record_numbers[1], tree_numbers[1], node_size, compare_method1)
ensemble_method(record_numbers[1], tree_numbers[2], node_size, compare_method1)
ensemble_method(record_numbers[1], tree_numbers[3], node_size, compare_method1)

# data record number = 1000
ensemble_method(record_numbers[2], tree_numbers[1], node_size, compare_method2)
ensemble_method(record_numbers[2], tree_numbers[2], node_size, compare_method2)
ensemble_method(record_numbers[2], tree_numbers[3], node_size, compare_method2)

node_size <- 12
ensemble_method(record_numbers[2], tree_numbers[1], node_size, compare_method3)
ensemble_method(record_numbers[2], tree_numbers[2], node_size, compare_method3)
ensemble_method(record_numbers[2], tree_numbers[3], node_size, compare_method3)

# comment on the results
# 

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

true_pi <- c(1/3, 1/3, 1/3)

true_mu[1,] = c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,] = c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,] = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)

plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")

# Producing the training data
for(i in 1:n) {
    m <- sample(1:3,1,prob=true_pi)
    for(d in 1:D) {
        x[i,d] <- rbinom(1,1,true_mu[m,d])
    }
}

M=3 # number of clusters
w <- matrix(nrow=n, ncol=M) # weights
pi <- vector(length = M) # mixing coefficients
mu <- matrix(nrow=M, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations

# Random initialization of the parameters
pi <- runif(M,0.49,0.51)
pi <- pi / sum(pi)
for(m in 1:M) {
    mu[m,] <- runif(D,0.49,0.51)
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
    # TODO:Your code here

    #Log likelihood computation.
    #TODO: Your code here

    cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
    flush.console()

    # Stop if the lok likelihood has not changed significantly
    # TODO:Your code here

    #M-step: ML parameter estimation from the data and weights
    # TODO:Your code here
}

pi
mu
plot(llik[1:it], type="o")