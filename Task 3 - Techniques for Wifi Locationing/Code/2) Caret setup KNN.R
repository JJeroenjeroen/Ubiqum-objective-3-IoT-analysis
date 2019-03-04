#####################################################
# Date:      04-03-2019                             #
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
train_method = "knn"
train_metric <- "Accuracy"
train_tuneLength = 1


#train knn model 
set.seed(123)
knn_Fit1 <- train(x = wifi_train_xvalues, 
                y = wifi_train_yvalues$BUILDINGID,
                method = train_method,
                metric = train_metric,
                trControl = fitControl)




#predict brand outcomes on the testing data
Prediction_knn <- predict(knn_Fit1, newdata = wifi_test_xvalues)
postResample(Prediction_knn, wifi_test_yvalues$BUILDINGID)

#See the most important predictors
varImp(knn_Fit1)

#show values in confusion matrix
confusionMatrix(data = Prediction_knn, wifi_test_yvalues$BUILDINGID)

