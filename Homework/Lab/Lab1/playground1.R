library(kknn)

data <- read.csv("./Homework/Lab/Lab1/optdigits.csv")

# set data split ratio
ratio <- c(train = .5, validate = .25, test = .25)

row_num <- nrow(data)
cols_num <- ncol(data)

# set the last column as the label value
names(data)[cols_num] <- "label_value"

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

# since the training data is a image, we do not apply normalization

##########################################################

###########################################################


error_rates <- rep(30, 0)

for(k_value in 1:30){
    # apply kknn function
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
    error_rates[k_value] <- 1 - accuracy
}

x <- 1:30
y <- error_rates
error_rate_data <- data.frame(x, y)
ggplot2::ggplot(error_rate_data, ggplot2::aes(x, y)) + ggplot2::geom_line()
