In this assignment, we will try to predict the temp of Linköping station on date 2000-12-26.

###########################  Init code For Assignment 1 ########################
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(geosphere)
set.seed(1234567890)

############################ init data ######################################
stations <- read.csv("./Homework/Lab/Lab3/stations.csv", fileEncoding = "latin1")
temps <- read.csv("./Homework/Lab/Lab3/temps50k.csv")
st <- merge(stations, temps, by = "station_number")

# define date of interest
date_interest <- as.Date("2000-12-26")

# filter out data which is later than data of interest 
st$date <- as.Date(st$date)
old_sf <- st #for later use
st <- st[-which(difftime(date_interest, st$date) <= 0),]

# define station of interest to Linköping
station_row <- which(stations$station_name == "Linköping") 
station <- stations$station_name[station_row] 

# split the data to train and test (70/30)
n <- dim(st)[1]
id <- sample(1:n, floor(n * 0.7))
train <- st[id, ]
test <- st[-id, ]

######################### Kernel Code  #####################################
# Gaussian kernel of geo distance
geo_distance <- function(data, location_interest, h_dist) {
  location <- data.frame(longitude = data$longitude,
                         latitude  = data$latitude)
  distances <- distHaversine(location_interest,location)
  kernel_result <- exp(-(distances)^2 / (2 * h_dist^2))
  return(c(distances, kernel_result))
}

# Gaussian kernel of day distance
day_distance <- function(data, day_interest, h_day) {
  distances <- as.numeric(difftime(day_interest,data, 
                          units = "days"))
  kernel_result <- exp(-(distances)^2 / (2 * h_day^2))
  return(c(distances, kernel_result))
}

# Gaussian kernel of hour distance
hour_distance <- function(data, hour_interest, h_hour) {
  
  diff <- sapply(1:length(hour_interest), function(x){
              abs(as.numeric(difftime(strptime(hour_interest[x], "%H:%M:%S"), 
                                      strptime(data$time, "%H:%M:%S")), units="hours"))
  })
  
  distances <- ifelse(diff <= 12, diff, 24-diff)
  kernel_result <- exp(-(distances)^2 / (2 * h_day^2))
  return(kernel_result)
}

################################ 1.1 Distance Kernel ###########################

# These three values are up to the students
h_distance <- 100000

# set coordinates to predict(Linköping)
a <- stations$longitude[station_row]
b <- stations$latitude[station_row]

N <- nrow(train)
i <- 1:N
k_distance <- sapply(i, function(i){geo_distance(train[i, ], c(a, b), h_distance)})
plot(k_distance[1,], k_distance[2,], main = "Distance Gaussian Kernel", xlab = "Distance", ylab = "K")

################################ 1.2 Day Kernel ###############################
h_day <- 20
k_day <- sapply(i, function(i) day_distance(as.Date(train$date[i]), date_interest, h_day))
plot(k_day[1,], k_day[2,], main = "Day Gaussian Kernel", xlab = "Days", ylab = "K")

h_hour <- 7

# Get the time format we want
times_interest <- c(paste0("0",seq(4,8,by=2),":00:00"), paste0(seq(10,24,by=2),":00:00"))

k_hour <- sapply(i, function(i) hour_distance(train[i,], times_interest, h_hour))

predicted_temp <- sapply(1:length(times_interest), function(x) 
        sum((k_distance[2,] + k_day[2,] + k_hour[x,]) * train$air_temperature)/ sum(k_distance[2,], k_day[2,], k_hour[x,]))

plot(seq(4,24,by=2),predicted_temp, type="l", main = "Temp in Linkoping", xlab = "Time", ylab = "Temp")
################################ 1.5 Multi Kernel ###############################
