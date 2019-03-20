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

predictions <- readRDS("2019-03-20 predicted values3")


#add all actual y values in 1 dataframe with the results
predictions <- data.frame(predictions)
y_df_test <- data.frame(y_list_test)
predictions$ID <- seq.int(nrow(predictions))
y_df_test$ID <- seq.int(nrow(y_df_test))
all_y_values <- left_join(y_df_test, predictions, by = "ID")
remove(y_list_test, y_df_test, predictions)

#Confusion matrices of the floors and buildings
confusionMatrix(data = all_y_values$knn_predict_FLOOR, all_y_values$FLOOR)
confusionMatrix(data = all_y_values$knn_predict_BUILDING, all_y_values$BUILDING)

confusionMatrix(data = all_y_values$rf_predict_FLOOR, all_y_values$FLOOR)
confusionMatrix(data = all_y_values$rf_predict_BUILDING, all_y_values$BUILDING)


confusionMatrix(data = all_y_values$svmPoly_predict_FLOOR, all_y_values$FLOOR)
confusionMatrix(data = all_y_values$svmPoly_predict_BUILDING, all_y_values$BUILDING)


all_y_values$errorBUILD <- as.numeric(all_y_values$BUILDINGID) - as.numeric(all_y_values$knn_predict_BUILDINGID)

all_y_values$errorBUILD

#plot results
ggplot(all_y_values) +
  
  geom_point((aes(x = LONGITUDE, y = LATITUDE, colour = errorBUILD)))


all_y_values$longerr <- all_y_values$knn_predict_LATITUDE - all_y_values$LATITUDE
all_y_values$laterr <- all_y_values$knn_predict_LONGITUDE - all_y_values$LONGITUDE

all_y_values_err <- all_y_values %>% filter(abs(longerr) > 15) 

#plot results
ggplot(all_y_values_err) +
  
  geom_point((aes(x = LONGITUDE, y = LATITUDE)))

ggplot(all_y_values)

