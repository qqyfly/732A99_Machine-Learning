### EX 3

## 3.1

library(neuralnet)

set.seed(1234567890)

Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test


# Random initialization of the weights in the interval [-1, 1]
#winit <- # Your code here
nn <- neuralnet(data = tr, formula = Sin ~ Var, hidden = c(10))

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=1)
points(te, col = "blue", cex=1)
points(te[,1], predict(nn,te), col="red", cex=1)

# ANSWER: We observe in the plot that the predictions from the neural net (red 
# dots ) seem to be generally good predictions for the sine function.

## 3.2


