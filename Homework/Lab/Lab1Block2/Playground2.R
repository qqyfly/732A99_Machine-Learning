mixel_model_fun <- function(M){
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

  par(mfrow=c(3,4),mar = c(1, 1, 1, 1))

  iterLog <- vector(length = max_it)

  for(it in 1:max_it) {
    plot(mu[1,], type="o", col="blue", ylim=c(0,1))
    points(mu[2,], type="o", col="red")
    if (M > 2){
        points(mu[3,], type="o", col="green")
    }
    if (M > 3){
        points(mu[4,], type="o", col="pink")
    }
    Sys.sleep(2)

    # E-step: Computation of the weights
    for (i in 1:n) {
        pxi <- 0
        for (m in 1:M) {
          bern <- 1
          for(d in 1:D) {
            bern <- bern * (mu[m,d]^x[i,d]) * (1-mu[m,d])^(1-x[i,d])          
          }
          pxi <- pxi + pi[m] * bern
        }

        for (m in 1:M) {
          bern <- 1
          for(d in 1:D) {
            bern <- bern * (mu[m,d]^x[i,d]) * (1-mu[m,d])^(1-x[i,d])          
          }
          w[i, m] <- pi[m] * bern / pxi
        }
        w[i,] <- w[i,] / sum(w[i,])
    }

    #Log likelihood computation.
    llik[it] <- 0
    for (i in 1:n) {
      pxi <- 0
      for (m in 1:M) {
        bern <- 1
        for(d in 1:D) {
          bern <- bern * (mu[m,d]^x[i,d]) * (1-mu[m,d])^(1-x[i,d])          
        }
        pxi <- pxi + pi[m] * bern
      }
      llik[it] <- llik[it] + log(pxi)
    }
    
    iterLog[it] <- paste("iteration is ", it, 
                        "log likelihood ", llik[it])

    flush.console()

    # Stop if the lok likelihood has not changed significantly
    if (it > 1 && abs(llik[it] - llik[it - 1]) < min_change) {
        break
    }

    pi <- apply(w,2,mean)
    mu <- t(w) %*% x / colSums(w)
  }

  par(mfrow=c(1,1),mar = c(1, 1, 1, 1))
  plot(llik[1:it], type = "o")
  print(iterLog[1:it])

  print("Pi value are")
  print(pi)
  print("Mu value are")
  print(mu)
}


# init data set (copy from pdf file)
set.seed(1234567890)
# max number of EM iterations
max_it <- 100
# min change in log likelihood between two consecutive iterations
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
M=2
mixel_model_fun(M=2)