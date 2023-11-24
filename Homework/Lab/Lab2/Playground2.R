###########################  Init code #########################################
rm(list = ls())

######################  Assignment 2.1 #########################################
# read data
data <- read.csv("./Homework/Lab/Lab2/bank-full.csv")

row_num <- nrow(data)
cols_num <- ncol(data)

# set data split ratio to 0.5, 0.3, 0.3
ratio <- c(train = .5, validate = 0.3, test = .3)

# set random seed
set.seed(12345)

# split data to training,validate and test dataset
train_id <- sample(1:row_num, floor(row_num * ratio[1]))
train_set <- data[train_id, ]

# set random seed
set.seed(12345)

validation_test_id <- setdiff(1:row_num, train_id)
validation_id <- sample(validation_test_id, floor(row_num * ratio[2]))
validation_set <- data[validation_id, ]

test_id <- setdiff(validation_test_id, validation_id)
test_set <- data[test_id, ]

######################  Assignment 2.2 #########################################
