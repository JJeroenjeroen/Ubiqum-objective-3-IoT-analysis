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
train_method = "gbm"
train_metric <- "RMSE"
train_tuneLength = 1


#train gmb model 
set.seed(123)
gmb_Fit1 <- train(x = wifi_train_xvalues, 
                y = wifi_train_yvalues$LONGITUDE,
                method = train_method,
                metric = train_metric,
                trControl = fitControl)




#provide statistics of training data
print(gmb_Fit1)
plot(gmb_Fit1)

#predict brand outcomes on the testing data
Prediction_gmb_lon <- predict(gmb_Fit1, newdata = wifi_test_xvalues)
postResample(Prediction_gmb_lon, wifi_test_yvalues$LONGITUDE)


#add predicted values to testvalues
wifi_test_yvalues$pred_lon <- Prediction_gmb_lon
