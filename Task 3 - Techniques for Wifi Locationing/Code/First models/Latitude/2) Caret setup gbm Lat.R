#####################################################
# Date:      05-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   1.0                                    #    
#####################################################


#This file will make a pipeline for a caret setup for the UJIIndoorLoc dataset 
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################


#set cross validation parameters
control_method <-"repeatedcv"
control_folds <- 10
control_repeats <- 1


fitControl <- trainControl(method = control_method,
                           number = control_folds,
                           repeats = control_repeats)


#set training parameters
train_method = ""
train_metric <- "RMSE"
train_tuneLength = 1


#train gbm model 
set.seed(123)
gbm_Fit1 <- train(x = wifi_train_xvalues, 
                  y = wifi_train_yvalues$LATITUDE,
                  method = train_method,
                  metric = train_metric,
                  trControl = fitControl)




#provide statistics of training data
print(gbm_Fit1)
plot(gbm_Fit1)

#predict brand outcomes on the testing data
Prediction_gbm_lat <- predict(gbm_Fit1, newdata = wifi_test_xvalues)
postResample(Prediction_gbm_lat, wifi_test_yvalues$LATITUDE)


#add predicted values to testvalues
wifi_test_yvalues$pred_lat <- Prediction_gbm_lat
