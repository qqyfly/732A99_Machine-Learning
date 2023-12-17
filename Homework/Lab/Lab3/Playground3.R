
###########################  Init code For Assignment 3 ########################
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(neuralnet)
set.seed(1234567890)

### 3.1 

###########################  3.1 ################################################
rand_var <- runif(500, 0, 10)
df <- data.frame(rand_var, Sin=sin(rand_var))
train <- df[1:25,] # Training
test <- df[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)
nn <- neuralnet(data = train, formula = Sin ~ rand_var, hidden = c(10))

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train,  col = "black", cex=1, main="Default")
points(test, col = "blue", cex=1)
points(test[,1], predict(nn,test), col="red", cex=1)
legend(x = 6.5, y = -0.6, legend = c("Training data", "Test data",
"Pred"), col = c("black", "blue", "red"), cex = 0.7,pch = 1)

### 3.2 


###########################  3.2.1 Linear #######################################

#Linear
h1 <- function(x) {
  x
} 

nn1 <- neuralnet(data = train, formula = Sin ~ rand_var, hidden = c(10), act.fct = h1)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train,  col = "black", cex=1, main="Linear")
points(test, col = "blue", cex=1)
points(test[,1], predict(nn1,test), col="red", cex=1)
legend(x = 6.5, y = -0.6, legend = c("Training data", "Test data",
"Pred"), col = c("black", "blue", "red"), cex = 0.7,pch = 1)


###########################  3.2.2 Relu #######################################

#ReLU
h2 <- function(x) {
  # max(0,x) not working
  ifelse(x>=0,x,0)
}


nn2 <- neuralnet(formula = Sin ~ rand_var, data = train, hidden = 10,
startweights = winit, act.fct = h2)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train,  col = "black", cex=1,main="Relu")
points(test, col = "blue", cex=1)
points(test[,1], predict(nn2,test), col="red", cex=1)
legend(x = 6.5, y = -0.6, legend = c("Training data", "Test data",
"Pred"), col = c("black", "blue", "red"), cex = 0.7,pch = 1)



###########################  3.2.3 Softplus #######################################

#Softplus
h3 <- function(x) {
  log(1+exp(x))
}

nn3 <- neuralnet(formula = Sin ~ rand_var, data = train, hidden = 10,
startweights = winit, act.fct = h3)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train,  col = "black", cex=1,main="Softplus")
points(test, col = "blue", cex=1)
points(test[,1], predict(nn3,test), col="red", cex=1)
legend(x = 6.5, y = -0.6, legend = c("Training data", "Test data",
"Pred"), col = c("black", "blue", "red"), cex = 0.7,pch = 1)



### 3.3


###########################  3.3  #######################################
# Sample 500 points
set.seed(1234567890)
rand_var <- runif(500, min = 0, max = 50) 
df <- data.frame(rand_var, Sin = sin(rand_var))
plot(train, cex = 2, xlim = c(0, 50), ylim = c(-6, 2))
points(df, col = "blue", cex = 1)
points(df[, 1], predict(nn, df), col = "red", cex = 1)
plot(nn)


###########################  3.4  #######################################
weight_1 <- nn$weights[[1]][[1]][2,]
bias_1 <- nn$weights[[1]][[1]][1,]
sigmoid_val <- sigmoid(weight_1 * 100 + bias_1)

bias_2 <- nn$weights[[1]][[2]][1,]
weight_2 <- nn$weights[[1]][[2]][2:11,]
cat("The value will converge to:", weight_2 %*% sigmoid_val + bias_2, "\n")

###########################  3.5  #######################################

# Sample 500 points
Var <- runif(500, min = 0, max = 10) 
df <- data.frame(Sin = sin(Var), Var)
nn4 <- neuralnet(formula = Var ~ Sin, data = df, hidden = 10, startweights = winit, threshold = 0.1)
pred_4 <- predict(nn4, df)

plot(df, cex = 2, col = "blue")
points(df[, 2], pred_4, col = "red", cex = 1)
legend(x = -1, y = 8.5, legend = c("Training data", "NN Pred"),
col = c("blue", "red"), cex = 1, pch = 1)

