library(kknn)

data <- read.csv("~/Data/Owncloud/01.Pierre/Statistics_and_ML/732A99_Machine Learning/machine_learning.git/Homework/Lab/Lab1/optdigits.csv")

percentage_training <- 0.5
percentage_validation <- 0.25
percentage_test <- 0.25

n <- dim(data)[1]
col_num <- dim(data)[2]
set.seed(42)

training_data_id <- sample(1:n, floor(n * percentage_training), replace = FALSE)
train_data <- data[training_data_id]

test_validate_id <- c(1:n)[-training_data_id]

test_data_id <- sample(test_validate_id, floor(n * percentage_test),
                       replace = FALSE)
validate_data_id <- test_validate_id[-test_data_id]

data_train <- data[training_data_id]
data_test <- data[test_data_id]
data_validate <- data[validate_data_id]


#kknn(formula = formula(data_train), train = data_train, test = data_test,
#    k=30, kernel = "triangular", distance = 2, scale = TRUE)
