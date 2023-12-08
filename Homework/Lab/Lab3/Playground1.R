set.seed(1234567890)
library(geosphere)

######################### Kernel Code Area #####################################
# return the day distance between 2 days
day_distance(day, day_of_interest){
  return (as.Date(day_of_interest) - as.Date(day))
}

# return the geo distance between 2 stations
# df: data frame of st
# station1_number: the number of the first station
# station2_number: the number of the second station
geo_distance(df, station1_number, station2_number ){
  return distHaversine(df$stations[station1_number,4:5],
                       df$stations[station2_number,4:5])
}

# return the hour distance between 2 different hours
temp_distance(hour1,hour2){
  data <- as.difftime(c(hour1,hour2),units="hours")
  return (as.numeric(data[2]-data[1])
}

################################################################################

#stations <- read.csv("stations.csv", fileEncoding = "latin1")
stations <- read.csv("./Homework/Lab/Lab3/stations.csv",
                    fileEncoding = "latin1")

#temps <- read.csv("temps50k.csv")
temps <- read.csv("./Homework/Lab/Lab3/temps50k.csv")

st <- merge(stations, temps, by = "station_number")

# These three values are up to the students
h_distance <- 
h_date <-
h_time <-

# The point to predict (up to the students)
a <- 58.4274 
b <- 14.826

# The date to predict (up to the students)
date <- "2013-11-04"

times <- c("04:00:00", "06:00:00", ..., "24:00:00")

temp <- vector(length=length(times)

# TODO: Students’ code here


plot(temp, type="o")