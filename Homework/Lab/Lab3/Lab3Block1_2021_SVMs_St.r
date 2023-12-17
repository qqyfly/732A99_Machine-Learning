# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

cat("minimal error =", min(err_va), " when C =", which.min(err_va) * by,"\n")

c_plot <- seq(by, 5, by)
plot(x = c_plot, y = err_va, type = "p",xlab = "C", ylab = "error", main = "err_va vs C")

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)


filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)

cat("accuracy of filter0 is", (1-err0)*100,"%\n")
cat("accuracy of filter1 is", (1-err1)*100,"%\n")
cat("accuracy of filter2 is", (1-err2)*100,"%\n")
cat("accuracy of filter3 is", (1-err3)*100,"%\n")


# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?

accuracy of filter0 is 93.25 %
accuracy of filter1 is 91.51061 %
accuracy of filter2 is 91.7603 %
accuracy of filter3 is 97.87765 %

| Filter | Accuracy | Accept / Reject | 
|:-:|:-:|:-:|
| filter0 | 93.25 % | Accept | 
It follow the best practice and get the highest accuracy rate. it train on training data,it predict on validation data

| Filter | Accuracy | Accept / Reject | 
|:-:|:-:|:-:|
| filter1 | 91.51061 % | Reject |  |
It follow the best practice but its accuracy rate is lower than filter 0.it train on training data,it predict on test  data

| Filter | Accuracy | Accept / Reject | 
|:-:|:-:|:-:|
| filter2 | 91.7603 % | Reject | 
It does not follow the best practice but still practiciable, it train on training+validation data, predict on test data, also its accuracy rate is lower than filter 0.

| Filter | Accuracy | Accept / Reject | 
|:-:|:-:|:-:|
| filter3 | 97.87765 % | Reject |
Training on all data,and using the seen data to predict. which is not accepetable

Accoring to the results, we should return filter0 to the user.

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
#filter0 and filter1 are same.

#so estimate of the generalization error of these 2 filters are same.that means if we apply test data on filter 1, this value will be same as filter1 error rate 
#It can be prove using the following code.

filter0_pred <- predict(filter0, te, type = "response")
filter0_gerr <- mean(filter0_pred != te[, 58])
if(filter0_generr == err1){
  cat("the result is same\n")
}else{
    cat("the result is different\n")
  } 

# the result is TRUE

# 3. Implementation of SVM predictions.

sv <- alphaindex(filter3)[[1]]
co<-coef(filter3)[[1]]
inte<- - b(filter3)
rbf <- rbfdot(sigma = 0.05) # RBF kernel with sigma = 0.05
k<-NULL
# We produce predictions for just the first 10 points in the dataset.
for(i in 1:10){ 
  k2<-NULL
  for(j in 1:length(sv)){
    k2 <- k2 + co[j] * rbf(as.numeric(spam[sv[j], -58]),
as.numeric(spam[i, -58]))
  }
  k<-c(k,  k2 + inte)
}
k
pred_filter3 <- predict(filter3,spam[1:10,-58], type = "decision")
pred_filter3
