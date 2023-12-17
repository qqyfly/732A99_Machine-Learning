library(geosphere)

######################### Kernel Code Area #####################################
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
  distances <- as.numeric(difftime(strptime(hour_interest, "%H:%M:%S"),
                              strptime(data$time, "%H:%M:%S"),
                              units = "hours"))
  kernel_result <- exp(-(distances)^2 / (2 * h_day^2))
  return(c(distances, kernel_result))
}

################################ 1 #################################################

#stations <- read.csv("stations.csv", fileEncoding = "latin1")
stations <- read.csv("./Homework/Lab/Lab3/stations.csv",
                    fileEncoding = "latin1")

#temps <- read.csv("temps50k.csv")
temps <- read.csv("./Homework/Lab/Lab3/temps50k.csv")

st <- merge(stations, temps, by = "station_number")



date_interest <- as.POSIXlt("2016-12-26")

# Filter out the data before the date of interest
if (any(as.POSIXlt(st$date) >= date_interest)) { 
  st <- st[-which(as.POSIXlt(st$date) >= date_interest), ]
}

# split the data to train and test (70/30)
n <- dim(st)[1]
id <- sample(1:n, floor(n * 0.7))
train <- st[id, ]
test <- st[-id, ]

################################ 1.1 Distance Kernel ###########################
# Station Name to Linköping 
#station_row <- which(stations$station_name == "Linköping") 
station_row <- which(stations$station_name == "Åreskutan Aut") 

station <- stations$station_name[station_row] 

# These three values are up to the students
h_distance <- 250000

# set coordinates to predict
a <- stations$longitude[station_row]
b <- stations$latitude[station_row]

N <- nrow(train)
i <- 1:N
k_distance <- sapply(i, function(i){geo_distance(train[i, ], c(a, b), h_distance)})
plot(k_distance[1,], k_distance[2,], main = "Distance Gaussian Kernel", xlab = "Distance", ylab = "K")

################################ 1.2 Day Kernel ###############################
h_day <- 6000
k_day <- sapply(i, function(i) day_distance(as.POSIXlt(train$date[i]), date_interest, h_day))
plot(k_day[1,], k_day[2,], main = "Day Gaussian Kernel", xlab = "Days", ylab = "K")
################################ 1.3 Hour Kernel ##############################
times_interest <- "18:00:00"

# Need to filter here
h_hour <- 7
k_hour <- sapply(i, function(i) hour_distance(train[i,], times_interest, h_hour))
plot(k_hour[1,], k_hour[2,], main = "Hour Gaussian Kernel", xlab = "Days", ylab = "K")
#plot(k_hour[1:11,], k_hour[12:22,], main = "Hour Kernel", xlab = "Diff Hour", ylab = "K")

################################ 1.4 Sum Kernel ##############################
# TODO: Change the code since it's a copy of the code

# setting up the addition kernel which is the *sum* of the 3 kernels distance, day & hour
k_sum <- matrix(NA, nrow = 11, ncol = N)
temp_pred_sum <-matrix(NA, nrow = 11, ncol = 1)
k <- 1
for (x in 12:22) {
  # sum the kernels
  k_sum[k,] <- (k_distance[2,] + k_day[2,] + k_hour[x,])
  temp_pred_sum[k] <- (k_sum[k,]%*%train$air_temperature)/sum(k_sum[k,])
  k <- k + 1
}

N <- nrow(test)
i <- 1:N
k_distance <- sapply(i, function(i) k_distance_f(train[i,], a, b, h_dist))
k_day <- sapply(i, function(i) k_day_f(as.POSIXlt(test$date[i]), date, h_day))
k_hour <- sapply(i, function(i) k_hour_f(test[i,], times, h_hour))

k_sum <- matrix(NA, nrow = 11, ncol = N)
temp_pred_sum <-matrix(NA, nrow = 11, ncol = 1)
k <- 1
for (x in 12:22) {
  # sum the kernels
  k_sum[k,] <- (k_distance[2,] + k_day[2,] + k_hour[x,])
  temp_pred_sum[k] <- (k_sum[k,]%*%test$air_temperature)/sum(k_sum[k,])
  k <- k + 1
}
k_mult <- matrix(NA, nrow = 11, ncol = N)
temp_pred_mult <- matrix(NA, nrow = 11, ncol = 1)
k <- 1
for (x in 12:22) {
  k_mult[k,] <- k_distance[2,] * k_day[2,] * k_hour[x,]
  temp_pred_mult[k] <- (k_mult[k,]%*%test$air_temperature)/sum(k_mult[k,])
  k <- k + 1
}
# combined plot for addition and multiplied kernel
plot(strptime(times, format = "%H:%M:%S"), temp_pred_mult, ylab = "Predicted Temp", xlab = "Time", 
     main = paste("Test Data: ", station, date), ylim = c(min(temp_pred_mult, temp_pred_sum), 
                                                  1+max(temp_pred_mult, temp_pred_sum)), pch = 3)
points(strptime(times, format = "%H:%M:%S"), temp_pred_sum, col = "red")
legend(x = "topleft", legend = c("Addition", "Multiplied"), col = c("red", "black"), pch = c(1,3))