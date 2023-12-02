### Ex 3

rm(list=ls(all.names = T))


library(ggplot2)
library(caret)

set.seed(12345)
data <- read.csv("communities.csv")

# 1) 

n <- nrow(data)
features <- data[, -101]

s_features <- scale(features)

S <- (t(s_features) %*% s_features)/n  # sample covariance matrix

Eig <- eigen(S) 

# eigen in descending order
s_indx <- order(Eig$values, decreasing = TRUE)
s_eig <- Eig$values[s_indx]

# cumulative explained variance
cum_var <- cumsum(s_eig) / sum(s_eig)

q_95 <- which(cum_var >= 0.95)[1]  # q for 95% var

first_two_components <- Eig$vectors[, 1:2]  # first two PC

# proportion of variation explained by each of the first two components
PC1_var <- s_eig[1] / sum(s_eig)
PC2_var <- s_eig[2] / sum(s_eig)

cat("Number of components needed for 95% variance:", q_95, "
Proportion of variation explained by the first component:", PC1_var, "
Proportion of variation explained by the second component:", PC2_var, "\n")

# 2)

PCA <- princomp(features)

L <- PCA$loadings
plot(L[,1], main="Traceplot", xlim=c(0,20))

highest5 <- order(L[,1], decreasing=TRUE)[1:5]

# We observe from the plot how the variable "state", the first feature of PC1, 
# explains most of the data. The other variables carry significantly less 
# explanation. The other 4 features that contribute the most are features 93 
# (PctBornSameState), 61 (PctSpeakEnglOnly), 5 (racePctWhite) and 76 
# (PersPerRentOccHous).

plot(L[,1], L[,2], main="PCA Scores", xlim=c(0,0.005))

# plot to be completed

# 3)

# train and test data
id <- sample(1:n, floor(n*0.5))
trn <- data[id,]
tst <- data[-id,]

# scale training and test data
scaler <- preProcess(trn)
trainS <- predict(scaler,trn)
testS <- predict(scaler,tst)

# linear regression model and test data predictions
linmod <- lm(trainS$ViolentCrimesPerPop ~ ., trainS)
test_pred <- predict(linmod, testS[,-101])

# training and test data MSE
train_MSE <- mean((trainS$ViolentCrimesPerPop - linmod$fitted.values)^2)
test_MSE <- mean((testS$ViolentCrimesPerPop - test_pred)^2)

cat("Train mean squared error:", train_MSE, "\nTest mean squared error:", test_MSE)

### answer

# 4)


















