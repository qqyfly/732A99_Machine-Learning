#############################  Init Code #######################################



###########################  Assignment 3.1 ####################################

data <- read.csv("./Homework/Lab/Lab1/pima-indians-diabetes.csv")

age <- data[, 8]
plasma <- data[, 2]

names(data)[9] <- "diabetes"
names(data)[2] <- "plasma"
names(data)[8] <- "age"

x <- age
y <- plasma
df <- data.frame(x, y)

ggplot2::ggplot(df, ggplot2::aes(age, plasma)) + ggplot2::geom_point(ggplot2::aes(colour =  data[, 9]))+
ggplot2::labs(x = "age",y="Plasma glucose")


model <- glm(diabetes ~ plasma + age, family = gaussian, data = data)

summary(model)