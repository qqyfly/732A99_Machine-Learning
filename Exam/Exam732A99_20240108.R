###########################################
# 732A99 January 2024
###########################################

# (10 p) The points are divided as indicated below.

#install.packages("mvtnorm")
library(mvtnorm)

set.seed(123)

N=300 # number of training points
M=3000 # number of test points
D=2 # number of dimensions
tr <- matrix(nrow=N, ncol=D+1) # training data
te <- matrix(nrow=M, ncol=D+1) # test data
B <- 10 # number of bootstrap samples
pte <- matrix(nrow=M,ncol=B) # class predictions for the test data from the individual predictors

# producing the training data
mu1<-c(0,0)
Sigma1 <- matrix(c(5,1,1,5),D,D)
dat1<-rmvnorm(n = 1100, mu1, Sigma1)
mu2<-c(4,6)
Sigma2 <- matrix(c(5,-1,-1,5),D,D)
dat2<-rmvnorm(n = 1100, mu2, Sigma2)
mu3<-c(7,2)
Sigma3 <- matrix(c(3,2,2,3),D,D)
dat3<-rmvnorm(n = 1100, mu3, Sigma3)
plot(dat1,xlim=c(-10,15),ylim=c(-10,15))
points(dat2,col="red")
points(dat3,col="blue")
tr[1:100,]<-c(dat1[1:100,],rep(1,100))
tr[101:200,]<-c(dat2[1:100,],rep(2,100))
tr[201:300,]<-c(dat3[1:100,],rep(3,100))
te[1:1000,]<-c(dat1[101:1100,],rep(1,1000))
te[1001:2000,]<-c(dat2[101:1100,],rep(2,1000))
te[2001:3000,]<-c(dat3[101:1100,],rep(3,1000))

K=3 # number of classes
pi <- vector(length=K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # class conditional means
Sigma <- array(dim=c(D,D,K)) # class conditional covariances
p <- matrix(nrow=M, ncol=K) # posterior class probabilities

e <- 0
for(b in 1:B){
  # bootstrap sample
  btr <- tr[sample(1:N,N,replace = TRUE),]
  
  # ML parameter estimation, i.e. sample mean and covariance matrix (2 p)
  for(k in 1:K) {
    pi[k] <- sum(btr[,3]==k) / N
    for(d in 1:D) {
      mu[k, d] <- sum(btr[, d] * (btr[, 3]==k)) / sum(btr[,3]==k)
    }
    for(d in 1:D) {
      for(d2 in 1:D) {
        Sigma[d,d2,k]<-sum((btr[, d]-mu[k,d]) * (btr[, d2]-mu[k,d2]) * (btr[, 3]==k)) / sum(btr[,3]==k)
      }
    }
  }
  
  # individual prediction via Bayes' theorem (3 p)
  for(m in 1:M) {
    for(k in 1:K) {
      p[m,k] <- pi[k]*dmvnorm(te[m,1:2],mu[k,],Sigma[,,k])
    }
    
    p[m,] <- p[m,]/sum(p[m,])

    pte[m,b] <- which.max(p[m,])
  }
}

# returns the mode/majority of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ensemble prediction via majority voting (3 p)

e <- 0
for(m in 1:M) {
  e <- e + (getmode(pte[m,])!=te[m,3])
}

e/M

# (1 p) Bagging may not work well here because a Gaussian mixture model is a model with relatively low variance and large bias. These
# features may be inherited by the corresponding discriminative model, which make it unsuitable for bagging.

# (1 p) We could decorrelate the ensemble's base models by randomly selecting the dimensions or features to include in the generative model.
