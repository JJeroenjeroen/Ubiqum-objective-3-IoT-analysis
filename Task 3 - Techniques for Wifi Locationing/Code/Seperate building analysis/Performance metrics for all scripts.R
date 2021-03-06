#####################################################
# Date:      24-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   final                                  #    
#####################################################


#This file runs all the models and provides the performance metrics 
#For more information, visit http://archive.ics.uci.edu/ml/dataB2s/UJIIndoorLoc
#########################################################################################

#Run scripts that train and test a model on predicting floors  
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 0")
source(file = "3) Building 0 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 1")
source(file = "3) Building 1 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 2")
source(file = "3) Building 2 analysis.R")

#Run scripts that train and test a model on predicting longitude and latitude
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/Building 0")
source(file = "3) Building 0 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/Building 1")
source(file = "3) Building 1 analysis.R")

setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/Building 2")
source(file = "3) Building 2 analysis.R")


#change working directory to map where all results are stalled
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results/total")

#read results floor predictions into R
Building0_floor <- readRDS(paste(Sys.Date(), "BUilding 0 total"))
Building1_floor <- readRDS(paste(Sys.Date(), "BUilding 1 total"))
Building2_floor <- readRDS(paste(Sys.Date(), "BUilding 2 total"))

#read results latitude and longitude predictions into R
Building0_location <- readRDS(paste(Sys.Date(), "BUilding 0 location"))
Building1_location <- readRDS(paste(Sys.Date(), "BUilding 1 location"))
Building2_location <- readRDS(paste(Sys.Date(), "BUilding 2 location"))

#combine all seperate floor predictions to 1 dataframe 
all_y_values <- bind_rows(Building0_floor, 
                          Building1_floor, 
                          Building2_floor)

#combine all seperate location predictions to 1 dataframe 
all_y_values_location <- bind_rows(Building0_location, 
                                   Building1_location, 
                                   Building2_location)

#add the Latitude, Longitude and floor predictions 1 dataframe 
all_y_values$LATITUDE <- all_y_values_location$LATITUDE
all_y_values$LONGITUDE <- all_y_values_location$LONGITUDE
all_y_values$knn_predict_LATITUDE <- all_y_values_location$knn_predict_LATITUDE
all_y_values$knn_predict_LONGITUDE <- all_y_values_location$knn_predict_LONGITUDE


#change floor to a factor
all_y_values$knn_predict_FLOOR <- as.factor(all_y_values$knn_predict_FLOOR)
all_y_values$FLOOR <- as.factor(all_y_values$FLOOR)

#Confusion matrices of the floors and buildings
confusionMatrix(data = all_y_values$knn_predict_FLOOR, all_y_values$FLOOR)

#performance metrics of Latitude and Longitude regressions
postResample(all_y_values$knn_predict_LATITUDE, all_y_values$LATITUDE)
postResample(all_y_values$knn_predict_LONGITUDE, all_y_values$LONGITUDE)


#generate Longitude and Latitude errors
all_y_values$Longerror <- (all_y_values$knn_predict_LONGITUDE -
                             all_y_values$LONGITUDE)

all_y_values$Laterror <- (all_y_values$knn_predict_LATITUDE - 
                            all_y_values$LATITUDE) 

#provide the mean circular distance
all_y_values$Longerror <- all_y_values$Longerror^2
all_y_values$Laterror <- all_y_values$Laterror^2 
all_y_values$dist <- all_y_values$Laterror + all_y_values$Longerror 
mean(sqrt(all_y_values$dist))

#generate lon/lat combination error 
all_y_values$dist <- (sqrt(all_y_values$dist))


ggplot(all_y_values) +
  geom_histogram(aes(x = dist),
                 bins = 90,
                 colour = "black",
                 fill = "lightblue")


ggplot(all_y_values) +
  geom_histogram(aes(x = sqrt(Longerror)),
                 bins = 90,
                 colour = "black",
                 fill = "lightblue")


ggplot(all_y_values) +
  geom_histogram(aes(x = sqrt(Laterror)),
                 bins = 90,
                 colour = "black",
                 fill = "lightblue")
