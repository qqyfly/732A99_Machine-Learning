set.seed(1234)

x1 <- runif(10000,0,10)
x2 <- runif(10000,-2,2)
y <- ifelse(x2 > sin(x1),1,-1)
plot(x1,x2,col = y + 2)
allData <- cbind(x1,x2,y)
D <- 2

stumpLearn <- function(w){
  
o1 <- order(dat[,1])
foo1 <- cumsum(w[o1] * dat[o1,3])
m1 <- max(abs(foo1))
i1 <- which.max(abs(foo1))

o2 <- order(dat[,2])
foo2 <- cumsum(w[o2] * dat[o2,3])
m2 <- max(abs(foo2))
i2 <- which.max(abs(foo2))

if(m1>m2){
  i <- 1
  t <- dat[o1[i1],1]
  l <- ifelse(foo1[i1]>0,1,-1)
}
else{
  i <- 2
  t <- dat[o2[i2],2]
  l <- ifelse(foo2[i2]>0,1,-1)
}

return(list(i=i, t=t, l=l))
}

stumpPredict <- function(x, pars){
  
foo<-ifelse(x[,pars$i]<pars$t,pars$l,-(pars$l))

return(foo)
}

AdaBoostLearn <- function(){
  
  n <- nrow(dat)
  w <- rep(1/n, times = n)
  alpha <- rep(0,times = B)
  allPars <- rep(list(list()),B)
  for(b in 1:B){
    allPars[[b]] <- stumpLearn(w)
    yhat <- stumpPredict(dat[,1:D],allPars[[b]])
    E <- sum(w * (dat[,D+1] != yhat))
    alpha[b] <- 0.5 * log((1-E)/E)
    w <- w * exp(- alpha[b] * dat[,D+1] * yhat)
    w <- w / sum(w)
  }
  
  return(list(allPars=allPars, allAlpha=alpha))
}

AdaBoostPredict <- function(x, allPars, allAlpha){
  
  foo <- rep(0,times=nrow(x))
  for(b in 1:B){
    foo <- foo + allAlpha[b] * stumpPredict(x, allPars[[b]])
  }
  foo <- ifelse(foo>0,1,-1)
  
  return(foo)
}

res <- NULL
for(B in 1:50){
  res2 <- NULL
  for(i in 1:10){
    foo <- sample(1:10000,500)
    dat <- allData[foo,]
    tes <- allData[-foo,]
    foo <- AdaBoostLearn()
    res2 <- c(res2,mean(AdaBoostPredict(tes[,1:D],foo$allPars,foo$allAlpha) != tes[,D+1]))
  }
  res <- c(res,mean(res2))
}
plot(res, type = "l")
