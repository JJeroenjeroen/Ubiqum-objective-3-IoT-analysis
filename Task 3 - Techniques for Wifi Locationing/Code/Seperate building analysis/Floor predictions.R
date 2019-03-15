setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 0")
source(file = "Building 0 analysis.R")
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 1")
source(file = "Building 1 analysis.R")
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 2")
source(file = "Building 2 analysis.R")

#read results into R
predictions0 <- readRDS("2019-03-15 BUilding 0 total")
predictions1 <- readRDS("2019-03-15 BUilding 1 total")
predictions2 <- readRDS("2019-03-15 BUilding 2 total")


#combine predictions
all_y_values <- bind_rows(predictions0, predictions1, predictions2)


all_y_values$knn_predict_FLOOR <- as.factor(all_y_values$knn_predict_FLOOR)
all_y_values$FLOOR <- as.factor(all_y_values$FLOOR)

#Confusion matrices of the floors and buildings
confusionMatrix(data = all_y_values$knn_predict_FLOOR, all_y_values$FLOOR)

postResample(all_y_values$knn_predict_LATITUDE, all_y_values$LATITUDE)
postResample(all_y_values$knn_predict_LONGITUDE, all_y_values$LONGITUDE)

ggplot(all_y_values_b1) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = knn_predict_FLOOR)) +
  facet_wrap("FLOOR")


all_y_values_b1 <- all_y_values %>% filter(FLOOR == 0)

ggplot(all_y_values_b1) +
  geom_density(aes(x = knn_predict_LATITUDE - LATITUDE), fill = "blue", alpha = 0.3) +
  geom_density(aes(x = knn_predict_LONGITUDE - LONGITUDE), fill = "red", alpha = 0.3)


all_y_values$errors_lat <- (abs(all_y_values$knn_predict_LATITUDE - all_y_values$LATITUDE))
all_y_values$errors_long <- (abs(all_y_values$knn_predict_LONGITUDE - all_y_values$LONGITUDE))

all_y_values_lat_error <- all_y_values %>% filter(errors_lat > 15)
all_y_values_long_error <- all_y_values %>% filter(errors_long > 15)

ggplot(all_y_values_lat_error) +
  geom_point(aes(x = knn_predict_LONGITUDE, y = knn_predict_LATITUDE, colour = FLOOR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE))


ggplot(all_y_values_long_error) +
  geom_point(aes(x = knn_predict_LONGITUDE, y = knn_predict_LATITUDE, colour = FLOOR))
