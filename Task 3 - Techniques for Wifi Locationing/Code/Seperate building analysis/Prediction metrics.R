#####################################################
# Date:      19-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file runs all the models and provides the performance metrics 
#For more information, visit http://archive.ics.uci.edu/ml/dataB2s/UJIIndoorLoc
#########################################################################################

#Run scripts that train and test a model on predicting floors  
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 0")
source(file = "Building 0 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 1")
source(file = "Building 1 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 2")
source(file = "Building 2 analysis.R")

#Run scripts that train and test a model on predicting longitude and latitude
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/Building 0")
source(file = "Building 0 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/Building 1")
source(file = "Building 1 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/Building 2")
source(file = "Building 2 analysis.R")


#read results into R
Building0_floor <- readRDS("2019-03-18 BUilding 0 total")
Building1_floor <- readRDS("2019-03-18 BUilding 1 total")
Building2_floor <- readRDS("2019-03-18 BUilding 2 total")
Building0_location <- readRDS("2019-03-19 BUilding 0 location")
Building1_location <- readRDS("2019-03-19 BUilding 1 location")
Building2_location <- readRDS("2019-03-19 BUilding 2 location")


#combine predictions 
all_y_values <- bind_rows(Building0_floor, Building1_floor, Building2_floor)
all_y_values_location <- bind_rows(Building0_location, Building1_location, Building2_location)

all_y_values$LATITUDE <- all_y_values_location$LATITUDE
all_y_values$LONGITUDE <- all_y_values_location$LONGITUDE

all_y_values$knn_predict_LATITUDE <- all_y_values_location$knn_predict_LATITUDE
all_y_values$knn_predict_LONGITUDE <- all_y_values_location$knn_predict_LONGITUDE

all_y_values$knn_predict_FLOOR <- as.factor(all_y_values$knn_predict_FLOOR)
all_y_values$FLOOR <- as.factor(all_y_values$FLOOR)

#Confusion matrices of the floors and buildings
confusionMatrix(data = all_y_values$knn_predict_FLOOR, all_y_values$FLOOR)

postResample(all_y_values$knn_predict_LATITUDE, all_y_values$LATITUDE)
postResample(all_y_values$knn_predict_LONGITUDE, all_y_values$LONGITUDE)

ggplot(all_y_values) +
  geom_point(aes(x =knn_predict_LONGITUDE , y = knn_predict_LATITUDE))
