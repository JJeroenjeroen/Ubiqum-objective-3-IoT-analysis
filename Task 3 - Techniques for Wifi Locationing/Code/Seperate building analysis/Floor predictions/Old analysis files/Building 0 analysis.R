#####################################################
# Date:      06-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will give the results and plot the predictions to see where the errors occur  
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis")
source(file = "Building 0 caret.R")

#set working directory to get the resultss
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results")

#read results into R
predictions <- readRDS("2019-03-15 Building 0")


#add all actual y values in 1 dataframe with the results
predictions <- data.frame(predictions)
y_df_test <- data.frame(y_list_test)
predictions$ID <- seq.int(nrow(predictions))
y_df_test$ID <- seq.int(nrow(y_df_test))
all_y_values <- left_join(y_df_test, predictions, by = "ID")

#store all y values:
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results/total")
saveRDS(all_y_values,
        file = paste(Sys.Date(), "BUilding 0", "total"))

#Confusion matrices of the floors and buildings
confusionMatrix(data = all_y_values$knn_predict_FLOOR, all_y_values$FLOOR)


#plot results
ggplot(all_y_values) +
  
  geom_point(aes(x = LONGITUDE, y = LATITUDE, color = knn_predict_FLOOR)) +
  facet_wrap("FLOOR")



#plot results
ggplot(Building_0_train) +
  
  geom_point((aes(x = LONGITUDE, y = LATITUDE))) +
  geom_point((aes(x = knn_outcome_LONGITUDE, y = knn_outcome_LATITUDE))) +
    facet_wrap("FLOOR")




all_y_values_f1 <- all_y_values %>% filter(FLOOR == 2)
plot_ly(data = all_y_values_f1, x = ~LONGITUDE, y = ~LATITUDE, color = ~knn_predict_FLOOR)
plot_ly(data = Building_0_train, x = ~LONGITUDE, y = ~LATITUDE)

inspect1 <- Building_0_train %>% filter(FLOOR == 0 & LATITUDE < 4864960 & LATITUDE > 4864948 & LONGITUDE > -7645 & LONGITUDE < -7625)


inspect1 <- inspect1[-which(apply(inspect1, 2, var) == 0)] 
ggplot(inspect1) +
  
  geom_point((aes(x = LONGITUDE, y = LATITUDE)))

summ_inspect_1 <- inspect1 %>% group_by(FLOOR) %>% summarise_all(mean)

