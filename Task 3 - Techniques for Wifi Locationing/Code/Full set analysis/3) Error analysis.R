#####################################################
# Date:      06-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will give the results and plot the predictions to see where the errors occur  
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################
#set working directory to get the resultss
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results")

#read results into R

predictions <- readRDS("2019-03-21 predicted values3")


#add all actual y values in 1 dataframe with the results
predictions <- data.frame(predictions)
y_df_test <- data.frame(y_list_test)
predictions$ID <- seq.int(nrow(predictions))
y_df_test$ID <- seq.int(nrow(y_df_test))
all_y_values <- left_join(y_df_test, predictions, by = "ID")
remove(y_list_test, y_df_test, predictions)

#Confusion matrix of the buildings
confusionMatrix(data = all_y_values$knn_predict_BUILDING, all_y_values$BUILDING)

#metrics for longitude and latitude
postResample(all_y_values$knn_predict_LATITUDE, all_y_values$LATITUDE)
postResample(all_y_values$knn_predict_LONGITUDE, all_y_values$LONGITUDE)


#Genereate the model's errors for building
all_y_values$errorBUILD <- as.numeric(all_y_values$BUILDINGID) - as.numeric(all_y_values$knn_predict_BUILDINGID)

#plot Correct building vs wrong building
ggplot(all_y_values) +
  geom_point((aes(x = LONGITUDE, y = LATITUDE, colour = as.factor(errorBUILD))))

#Genereate the model's errors for Longitude & Latitude
all_y_values$longerr <- all_y_values$knn_predict_LATITUDE - all_y_values$LATITUDE
all_y_values$laterr <- all_y_values$knn_predict_LONGITUDE - all_y_values$LONGITUDE

#histogram of longitude & latitude errors
ggplot(all_y_values) +
  geom_histogram(aes(x = longerr), colour = "black", fill = "lightblue")

ggplot(all_y_values) +
  geom_histogram(aes(x = laterr), colour = "black", fill = "lightblue")
