###########################  Init code #########################################
# load the library
library(kknn)

######################  Assignment 1.1 #########################################
# read data
data <- read.csv("./Homework/Lab/Lab1/optdigits.csv")
row_num <- nrow(data)
cols_num <- ncol(data)

# set the last column as the label value
names(data)[cols_num] <- "label_value"

# set data split ratio
ratio <- c(train = .5, validate = .25, test = .25)

# check data
# we use table(data[,65]), and we know we have 10 classes in the training data

# set random seed
set.seed(12345)

# split data
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_set <- data[train_id, ]

set.seed(12345)
test_val_id <- setdiff(1:row_num, train_id)
valid_id <- sample(test_val_id, floor(row_num * ratio[2]))
validate_set <- data[valid_id, ]

test_id <- setdiff(test_val_id, valid_id)
test_set <- data[test_id, ]


######################  Assignment 1.2 #########################################

# since the training data is a image, we do not apply normalization here.

# calculate error rate for k = 30
k_value <- 30
fit_kknn <- kknn(label_value ~ .,
                    train_set,
                    test_set,
                    k = k_value,
                    distance = 1,
                    kernel = "rectangular")

fit <- fitted(fit_kknn)

# generate confusion matrix
confusion_matrices <- table(round(fit), test_set$label_value)

# calculate accuracy
accuracy <- sum(diag(confusion_matrices)) / sum(confusion_matrices)

# calculate error rate
error_rate <- 1 - accuracy

# TODO: Need some comment here about the quality of predictions for different 
# digits and on the overall prediction quality

######################  Assignment 1.3 #########################################
# Question description:
# Find any 2 cases of digit “8” in the training data which were easiest to 
# classify and 3 cases that were hardest to classify (i.e. having highest and 
# lowest probabilities of the correct class).

# Find all the 8 images
eight_images <- train_set[train_set$label_value == 8,]

# draw a test image using heatmap
# we need to convert the image to a matrix of numeric values

# convert the image to a matrix of numeric values
eight_images_number <- nrow(eight_images)

for(i in 1:eight_images_number){
    eight_images_matrix <- matrix(c(as.numeric(eight_images[i,1:64])),ncol=8)
    heatmap(x=eight_images_matrix, Rowv = NA, Colv = NA)
}

# TODO:
# It is hard to identify the digit 8 from the heatmap, but we can see that the
# digit 8 is not very clear in the image X,X,X,X,X


######################  Assignment 1.4 #########################################
# calculate error rate for different k values

# error_rates <- rep(30, 0)

# for(k_value in 1:30){ 
#     # apply kknn function
#     fit_kknn <- kknn(label_value ~ .,
#                      train_set,
#                      test_set,
#                      k = k_value,
#                      distance = 1,
#                      kernel = "rectangular")

#     fit <- fitted(fit_kknn)

#     # generate confusion matrix
#     confusion_matrices <- table(round(fit), test_set$label_value)

#     # calculate accuracy
#     accuracy <- sum(diag(confusion_matrices)) / sum(confusion_matrices)

#     # calculate error rate
#     error_rates[k_value] <- 1 - accuracy
# }

# plot the error rate graph

# x <- 1:30
# y <- error_rates
# error_rate_data <- data.frame(x, y)
# ggplot2::ggplot(error_rate_data, ggplot2::aes(x, y)) + ggplot2::geom_line()

# TODO: Need comment on how the model complexity change when K increases and 
# how does it affect the training and validation

######################  TODO:Assignment 1.5 ####################################
# calculate error rate for different k values with an extra constant 
# and calculate the error for the validation data as cross-entropy 

# cross_entropy formula: 
# $ D = - \sum_{i=1}^{K}{\hat{p_{k}} * log(\hat{p_{k}})} $
# and a formula
# https://towardsdatascience.com/from-confusion-matrix-to-weighted-cross-entropy-9d2999c06845
# labels * -log(sigmoid(logits)) + (1 - labels) * -log(1 - sigmoid(logits))

