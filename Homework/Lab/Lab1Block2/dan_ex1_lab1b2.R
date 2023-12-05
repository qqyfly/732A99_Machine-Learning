### Ex 1

library(randomForest)

### test set

set.seed(12354)

x1 <- runif(1000)
x2 <- runif(1000)
tedata <- cbind(x1,x2)
y <- as.numeric(x1 < x2)
telabels <- as.factor(y)
plot(x1,x2,col=(y+1))

### misclassification error function

misC <- function(x,y){
  return(sum(as.numeric(x != y)))
}

### 1k test sets

t1.misC.errors <- c()
t10.misC.errors <- c()
t100.misC.errors <- c()

for (i in 1:1E3){
  x1 <- runif(100)
  x2 <- runif(100)
  trdata <- cbind(x1,x2)
  y <- as.numeric(x1 < x2)
  trlabels <- as.factor(y)
  
  t1.rf <- randomForest(trdata, trlabels, ntree=1, nodesize=25, keep.forest=TRUE)
  t10.rf <- randomForest(trdata, trlabels, ntree=10, nodesize=25, keep.forest=TRUE)
  t100.rf <- randomForest(trdata, trlabels, ntree=100, nodesize=25, keep.forest=TRUE)
  
  t1.pred <- predict(t1.rf, newdata=tedata, class="classification")
  t10.pred <- predict(t10.rf, newdata=tedata, class="classification")
  t100.pred <- predict(t100.rf, newdata=tedata, class="classification")
  
  t1.misC.errors[i] <- misC(t1.pred, telabels)
  t10.misC.errors[i] <- misC(t10.pred, telabels)
  t100.misC.errors[i] <- misC(t100.pred, telabels)
  
}

mean(t1.misC.errors)
var(t1.misC.errors)

mean(t10.misC.errors)
var(t10.misC.errors)

mean(t100.misC.errors)
var(t100.misC.errors)

### ex 2

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

M <- 3 # number of clusters
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
  #Sys.sleep(.5)
  
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


