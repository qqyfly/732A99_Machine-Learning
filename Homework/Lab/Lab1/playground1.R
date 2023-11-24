###########################  Init code #########################################
rm(list = ls())
library(kknn)

######################  Assignment 1.1 #########################################
# The following code will input the data and divide the data into training, 
# validation and test sets.

# read data

data <- read.csv("./Homework/Lab/Lab1/optdigits.csv")

#data <- read.csv("optdigits.csv")
row_num <- nrow(data)
cols_num <- ncol(data)

# set the last column name as "label_value" since we need to create a formula later
names(data)[cols_num] <- "label_value"

# set data split ratio to 0.5, 0.25 and 0.25
ratio <- c(train = .5, test = .25, validate = .25)

# data pre-processing
# columns 1-64 are number based and do not need to be normalized, last column
# is integer represent number from 0-9 which don't need to process again

# set random seed
set.seed(12345)

# split data to training, test and validation set
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_set <- data[train_id, ]

set.seed(12345)
test_val_id <- setdiff(1:row_num, train_id)
test_id <- sample(test_val_id, floor(row_num * ratio[2]))
test_set <- data[test_id, ]

valid_id <- setdiff(test_val_id, test_id)
valid_set <- data[valid_id, ]

######################  Assignment 1.2 #########################################

# The following code contain a function call to kknn with k=30 and
# kernel = ”rectangular”. We calculate the confusion matrices and the
# misclassification errors. The code is attached in the appendix.

######################  Assignment 1.2 #########################################

# The confusion_matrices is as follows:

# |   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
# |:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
# | 0 |97 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
# | 1 | 2 |80 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |
# | 2 | 0 |20 |91 | 0 | 0 | 0 | 1 | 0 | 0 | 1 |
# | 3 | 0 | 1 | 9 |57 | 3 | 0 | 0 | 0 | 3 | 1 |
# | 4 | 0 | 0 | 4 |18 |82 | 2 | 0 | 0 | 3 | 0 |
# | 5 | 0 | 1 | 5 |13 |14 |70 | 1 | 0 | 5 | 2 |
# | 6 | 0 | 1 | 2 | 3 | 3 |16 |80 | 1 |12 | 3 |
# | 7 | 0 | 1 | 1 | 0 | 5 | 4 | 0 |95 |24 |19 |
# | 8 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 1 |37 |45 |
# | 9 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |14 |

# As what the data show in the confusion matrix, we can find that the accuracy
# of number 0,1,9 are very high. For number 2,3,4,5,6,7, the accuracy still
# acceptable since most of the predictions are right. But for number 8, we found
# almost half of the prediction are wrong.

# Since most of the prediction accuracy rate still acceptable, we got a total
# accuracy rate of 0.7361257, and total error rate is 0.2638743.

# calculate error rate for k = 30
k_value <- 30
fit_kknn <- train::kknn(label_value ~ .,
                       train_set,
                       test_set,
                       k = k_value,
                       kernel = "rectangular")

predict_data <- predict(fit_kknn,newdata=test_set)

# generate confusion matrix
confusion_matrices <- table(round(predict_data), test_set$label_value)

# calculate accuracy
accuracy <- sum(diag(confusion_matrices)) / sum(confusion_matrices)

# calculate error rate
error_rate <- 1 - accuracy

######################  Assignment 1.3 #########################################

# Find all the 8 images
eight_images <- train_set[train_set$label_value == 8, ]

# get the number of 8 images in training data, we got 205 images
eight_images_number <- nrow(eight_images)

image_number  <- 18
eight_images_matrix <- matrix(c(as.numeric(eight_images[image_number, 1:64])), ncol = 8)
heatmap(x = eight_images_matrix, Rowv = NA, Colv = NA)

image_number  <- 50
eight_images_matrix <- matrix(c(as.numeric(eight_images[image_number, 1:64])), ncol = 8)
heatmap(x = eight_images_matrix, Rowv = NA, Colv = NA)

image_number  <- 1
eight_images_matrix <- matrix(c(as.numeric(eight_images[image_number, 1:64])), ncol = 8)
heatmap(x = eight_images_matrix, Rowv = NA, Colv = NA)

image_number  <- 2
eight_images_matrix <- matrix(c(as.numeric(eight_images[image_number, 1:64])), ncol = 8)
heatmap(x = eight_images_matrix, Rowv = NA, Colv = NA)

image_number  <- 55
eight_images_matrix <- matrix(c(as.numeric(eight_images[image_number, 1:64])), ncol = 8)
heatmap(x = eight_images_matrix, Rowv = NA, Colv = NA)

######################  Assignment 1.4 #########################################
# calculate error rate for different k values

error_rates_train <- rep(0, 30)
error_rates_validate <- rep(0, 30)

for(k_value in 1:30){
  # apply kknn function 
  train_kknn <- train.kknn(label_value ~ .,
                       data = train_set,
                       kmax = k_value,
                       kernel = "rectangular")

 predict_data_train <- predict(train_kknn,newdata=train_set)
  predict_data_validation <- predict(train_kknn,newdata=valid_set)

  # generate confusion matrix
  confusion_matrices_train <- table(round(predict_data_train), train_set$label_value)
  confusion_matrices_validation <- table(round(predict_data_validation), valid_set$label_value)

  # calculate accuracy
  accuracy_train <- sum(diag(confusion_matrices_train)) / sum(confusion_matrices_train)
  accuracy_validation <- sum(diag(confusion_matrices_validation)) / sum(confusion_matrices_validation)

  # calculate error rate
  error_rates_train[k_value] <- 1 - accuracy_train
  error_rates_validate[k_value] <- 1 - accuracy_validation
}

# plot the error rate graph
k <- 1:30

error_rate_data <- data.frame(k, error_rates_train,error_rates_validate)
ggplot2::ggplot() + 
  ggplot2::geom_line(error_rate_data,mapping=ggplot2::aes(x=k,y=error_rates_train),colour="blue") + 
  ggplot2::geom_line(error_rate_data,mapping=ggplot2::aes(x=k,y=error_rates_validate),colour="red") + 
  ggplot2::labs(x = "K",y="error rate")

######################  TODO:Assignment 1.5 ####################################
# calculate error rate for different k values with an extra constant 
# and calculate the error for the validation data as cross-entropy 

error_rates <- rep(30, 0)

for(k_value in 1:30){
  # apply kknn function
  fit_kknn <- train.kknn(label_value ~ .,
                         train_set,
                         test_set,
                         k = k_value,
                         distance = 1,
                         kernel = "rectangular")

  predict_data <- predict(fit_kknn,newdata = test_set)

  # generate confusion matrix
  confusion_matrices <- table(round(fit), test_set$label_value)

  # calculate accuracy
  accuracy <- sum(diag(confusion_matrices)) / sum(confusion_matrices)

  # calculate error rate
  error_rates[k_value] <- 1 - accuracy
}

# plot the error rate graph
x <- 1:30
y <- error_rates
error_rate_data <- data.frame(x, y)
ggplot2::ggplot(error_rate_data, ggplot2::aes(x, y)) + ggplot2::geom_line() + ggplot2::labs(x = "K",y="error rate")











