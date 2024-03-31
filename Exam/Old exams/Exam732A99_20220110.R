###########################################
# Semi-supervised learning of GMMs - 732A99
###########################################

#install.packages("mvtnorm")
library(mvtnorm)

set.seed(1234567890)

min_change <- 0.001 # min parameter change between two consecutive iterations
N=300 # number of training points
D=2 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data

# Producing the training data
mu1<-c(0,0)
Sigma1 <- matrix(c(5,3,3,5),D,D)
dat1<-rmvnorm(n = 100, mu1, Sigma1)
mu2<-c(5,7)
Sigma2 <- matrix(c(5,-3,-3,5),D,D)
dat2<-rmvnorm(n = 100, mu2, Sigma2)
mu3<-c(8,3)
Sigma3 <- matrix(c(3,2,2,3),D,D)
dat3<-rmvnorm(n = 100, mu3, Sigma3)
plot(dat1,xlim=c(-10,15),ylim=c(-10,15))
points(dat2,col="red")
points(dat3,col="blue")
x[1:100,]<-dat1
x[101:200,]<-dat2
x[201:300,]<-dat3
plot(x,xlim=c(-10,15),ylim=c(-10,15))

K=3 # number of classes
w <- matrix(nrow=N, ncol=K) # fractional class assignments
pi <- vector(length=K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # class conditional means
Sigma <- array(dim=c(D,D,K)) # class conditional covariances
aux <- vector(length=K)

# ML parameter estimation from labeled data
w[,] <- 0
w[1:10,1] <- 1
w[101:110,2] <- 1
w[201:210,3] <- 1
for(k in 1:K) {
  pi[k] <- sum(w[,k]) / sum(w[,])
  for(d in 1:D) {
    mu[k, d] <- sum(x[, d] * w[, k]) / sum(w[,k])
  }
  for(d in 1:D) {
    for(d2 in 1:D) {
      Sigma[d,d2,k]<-sum((x[, d]-mu[k,d]) * (x[, d2]-mu[k,d2]) * w[, k]) / sum(w[,k])
    }
  }
}

pi
mu
Sigma

it <- 0
repeat {
  # Fractional class assignment
  for(n in 1:N) {
    for(k in 1:K) {
      w[n,k] <- pi[k]*dmvnorm(x[n,],mu[k,],Sigma[,,k])
    }
    
    w[n,] <- w[n,]/sum(w[n,])
  }
  
  w[1:10,] <- 0
  w[1:10,1] <- 1
  w[101:110,] <- 0
  w[101:110,2] <- 1
  w[201:210,] <- 0
  w[201:210,3] <- 1
  
  # ML parameter estimation from all data
  oldpi <- pi
  oldmu <- mu
  oldSigma <- Sigma
  for(k in 1:K) {
    pi[k] <- sum(w[,k]) / N
    for(d in 1:D) {
      mu[k, d] <- sum(x[, d] * w[, k]) / sum(w[,k])
    }
    for(d in 1:D) {
      for(d2 in 1:D) {
        Sigma[d,d2,k]<-sum((x[, d]-mu[k,d]) * (x[, d2]-mu[k,d2]) * w[, k]) / sum(w[,k])
      }
    }
  }
  
  # Convergence
  it <- it+1
  conv <- max(max(abs(oldpi-pi)),max(abs(oldmu-mu)),max(abs(oldSigma-Sigma)))
  print(it)
  print(conv)
  if(conv<min_change || it>50){
    break
  }
}

pi
mu
Sigma

# Students should mention in their analysis of the results that the parameter
# values in lines 120-122 are closer to the true ones than the parameter values
# in lines 57-59. This can be seen by comparing conv in the first and last iterations.
# This shows that the unlabeled points contain valuable 
# information that help to estimate the true parameter values more accurately.

#########################
# Kernel methods - 732A99
#########################

set.seed(1234567890)

N_class1 <- 1500
N_class2 <- 1000

data_class1 <- NULL
for(i in 1:N_class1){
  a <- rbinom(n = 1, size = 1, prob = 0.3)
  b <- rnorm(n = 1, mean = 15, sd = 3) * a + (1-a) * rnorm(n = 1, mean = 4, sd = 2)
  data_class1 <- c(data_class1,b)
}

data_class2 <- NULL
for(i in 1:N_class2){
  a <- rbinom(n = 1, size = 1, prob = 0.4)
  b <- rnorm(n = 1, mean = 10, sd = 5) * a + (1-a) * rnorm(n = 1, mean = 15, sd = 2)
  data_class2 <- c(data_class2,b)
}

conditional_class1 <- function(t, h){
  d <- 0
  for(i in 1:N_class1)
    d <- d+dnorm((t-data_class1[i])/h)
  
  return (d/N_class1)
}

# The h value is chosen by visually verifying that the resulting distribution
# fits the data or empirical distribution, i.e. the data histogram.

h <- 1
hist(data_class1,xlim=c(-5,25),probability = TRUE)
xfit <- seq(-5,25, 0.1)
yfit <- conditional_class1(xfit, h)
lines(xfit, yfit, lwd=2)

conditional_class2 <- function(t, h){
  d <- 0
  for(i in 1:N_class2)
    d <- d+dnorm((t-data_class2[i])/h)
  
  return (d/N_class2)
}

h <- 1
hist(data_class2,xlim=c(-5,25),probability = TRUE)
xfit <- seq(-5,25, 0.1)
yfit <- conditional_class2(xfit, h)
lines(xfit, yfit, lwd=2)

# To produce posterior class probabilities, we simply use Bayes theorem.
# Students do not need to implement the solutoin. They just have to mention
# Bayes theorem in their answer to the question.

prob_class1 <- function(t, h){
  prob_class1 <- conditional_class1(t,h)*N_class1/(N_class1 + N_class2)
  prob_class2 <-conditional_class2(t,h)*N_class2/(N_class1 + N_class2)
  
  return (prob_class1/(prob_class1 + prob_class2))
}

h <- 1
xfit <- seq(-5,25, 0.1)
yfit <- prob_class1(xfit, h)
plot(xfit, yfit, lwd=2)
abline(h=0.5)