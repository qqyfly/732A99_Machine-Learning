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
