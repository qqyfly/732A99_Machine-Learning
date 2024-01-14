rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(geosphere)
set.seed(1234567890)

stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")

date_interest <- as.Date("1980-1-1")

# Point to predict(Linköping University Valla Campus Geo Location)
a <- 58.3986
b <- 15.5780

# other parameters
# distance unit is KM
h_distance <- 200
h_date <- 10
h_time <- 2
# It will generate a sequence of time from 4:00:00 to 24:00:00 with a step of 2 hours
times_interest <- c(paste0("0",seq(4,8,by=2),":00:00"), paste0(seq(10,24,by=2),":00:00"))

kernel_prediction <- function(h_distance, h_date, h_time, a, b, date, times){

  distance_vector <- c()
  distance_date_vector <- c()
  distance_time_vector <- c()

  kernel_distance_vector <- c()
  kernel_date_distance_vector <- c()
  kernel_time_distance_vector <- c()

  pred_temp_sum  <- c()
  pred_temp_mul <- c()
  
  for(i in 1:length(times)){
  
    # Filter out time that is later than data of interest 
    filtered_st <- st[(st$date == date & st$time <= times[i]) | st$date < date,]
    
    # calaulate the distance between the point of interest and the weather station on filtered data
    # Convert to KM
    distances <- distHaversine(c(a, b), filtered_st[,c('latitude','longitude')]) / 1000
    distance_vector <- c(distance_vector,distances)
    kernel_distance <- exp(-(distances)^2 / (2 * h_distance^2))
    kernel_data_distance_vector <- c(kernel_data_distance_vector,kernel_distance)

    # calaulate the distance between the date of interest and the date on filtered data
    date_diff <- abs(as.numeric(difftime(date, filtered_st$data, units = "days")))
    distance_date_vector = c(distance_date_vector,date_diff)
    kernel_date_distance <- exp(-(date_diff)^2 / (2 * h_date^2))
    kernel_data_distance_vector <- c(kernel_data_distance_vector,kernel_date_distance)  

    # calculate the distance between the time of interest and the time on filtered data
    time_diff <- abs(as.numeric(difftime(
                                  strptime(filtered_st$time, "%H:%M:%S"), 
                                  strptime(times[i], "%H:%M:%S")), 
                                  units="hours"))
    distance_time_vector = c(distance_time_vector,time_diff)
    kernel_time_distance <- exp(-(time_diff)^2 / (2 * h_time^2))
    kernel_time_distance_vector <- c(kernel_time_distance_vector,kernel_time_distance)

    # calculate the sum of 3 kernels
    kernel_sum <- kernel_distance + kernel_date_distance + kernel_time_distance

    # calculate the mul of 3 kernels
    kernel_mul <- kernel_distance * kernel_date_distance * kernel_time_distance

    # predict the temperature
    predicted_temp_sum <- sum((kernel_sum * filtered_st$air_temperature) / sum(kernel_sum))
    predicted_temp_mul <- sum((kernel_mul * filtered_st$air_temperature) / sum(kernel_mul))

    pred_temp_sum <- c(pred_temp_sum,predicted_temp_sum)
    pred_temp_mul <- c(pred_temp_mul,predicted_temp_mul)
  }
  return(list(pred_temp_sum,pred_temp_mul,
              distance_vector,kernel_distance_vector,
              distance_date_vector,kernel_date_distance_vector,
              distance_time_vector,kernel_time_distance_vector))
}

kernel_result <- kernel_prediction(h_distance, h_date, h_time, a, b, date_interest, times_interest)
