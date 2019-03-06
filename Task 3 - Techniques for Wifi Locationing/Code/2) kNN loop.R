#####################################################
# Date:      06-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will create the train various models and save the results in a csv file  
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################


#set cross validation parameters
control_method <-"repeatedcv"
control_folds <- 10
control_repeats <- 1


fitControl <- trainControl(method = control_method,
                           number = control_folds,
                           repeats = control_repeats)




#Choose parameters for training and testing the models
############################################################################

#which algorithms should be used?
algorithms <- c("knn", "gbm", "rf")

#for which dependent variables models should be trained?
y_names <- c("BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE")


############################################################################


#this for-loop will specify which algorithm the caret package will use
for (method in algorithms){
  
  #set training parameters
  train_method = method
  
  #the following for-loop will loop all datasets x & y's and make a trained  model for them
  for (i in 1:length(y_names)){
  set.seed(124)
    #create fitted values
    assign(paste(method, "_fit_",
                 y_names[i],
                 sep = ""),
           train(x = x_list_train[[y_names[i]]],
                 y = y_list_train[[y_names[i]]],
                 method = train_method,
                 trControl = fitControl))
  
#create a throwaway fit that can be used in this loop
    throwaway_fit <- train(x = x_list_train[[y_names[i]]],
                           y = y_list_train[[y_names[i]]],
                           method = train_method,
                           trControl = fitControl)
  
#create predictions
    assign(paste(method, 
                 "_predict_",
                 y_names[i], 
                 sep = ""),
           predict(throwaway_fit,
                   newdata = x_list_test[[y_names[i]]]))

  
#create a throwaway predict that can be used in this loop and removed afterwards 
    throwaway_predict <- predict(throwaway_fit,
                                 newdata = x_list_test[[y_names[i]]])
  
  
#provide statistics of applied model on the testing set
    assign(paste(method,
                 "_outcome_",
                 y_names[i],
                 sep = ""),
           postResample(throwaway_predict
                        , y_list_test[[y_names[i]]]))
    
    
    remove(throwaway_fit)
    remove(throwaway_predict)

    
    }
}





  
#show values in confusion matrix
confusionMatrix(data = knn_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID)
confusionMatrix(data = knn_predict_FLOOR, y_test_FLOOR$FLOOR)
knn_outcome_LATITUDE
knn_outcome_LONGITUDE

confusionMatrix(data = rf_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID)
confusionMatrix(data = rf_predict_FLOOR, y_test_FLOOR$FLOOR)
rf_outcome_LATITUDE
rf_outcome_LONGITUDE

confusionMatrix(data = gbm_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID)
confusionMatrix(data = gbm_predict_FLOOR, y_test_FLOOR$FLOOR)
gbm_outcome_LATITUDE
gbm_outcome_LONGITUDE


results_list <-  list(knn_con_building = confusionMatrix(data = knn_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID),
                      knn_con_floor = confusionMatrix(data = knn_predict_FLOOR, y_test_FLOOR$FLOOR),
                      knn_latitude = knn_outcome_LATITUDE,
                      knn_latitude = knn_outcome_LONGITUDE,
                      
                      gbm_con_building = confusionMatrix(data = gbm_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID),
                      gbm_con_floor = confusionMatrix(data = gbm_predict_FLOOR, y_test_FLOOR$FLOOR),
                      gbm_latitude = gbm_outcome_LATITUDE,
                      gbm_latitude = gbm_outcome_LONGITUDE,
                      
                      rf_con_building = confusionMatrix(data = rf_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID),
                      rf_con_floor = confusionMatrix(data = rf_predict_FLOOR, y_test_FLOOR$FLOOR),
                      rf_latitude = rf_outcome_LATITUDE,
                      rf_latitude = rf_outcome_LONGITUDE)

                      
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
saveRDS(results_list, file = paste(Sys.Date(), "Results of loop"))

        