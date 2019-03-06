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


#the following for-loop will loop all datasets x & y's and make a trained knn model for them
for (i in 1:length(y_names)){
  set.seed(124)
  
#create fitted values  
  assign(paste("knn_fit_", 
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
  assign(paste("knn_predict_", 
               y_names[i], 
               sep = ""),  
         predict(throwaway_fit,
                 newdata = x_list_test[[y_names[i]]]))

  
#create a throwaway predict that can be used in this loop and removed afterwards 
  throwaway_predict <- predict(throwaway_fit,
                               newdata = x_list_test[[y_names[i]]])
  
  
#provide statistics of applied model on the testing set
  assign(paste("knn_outcome_", 
               y_names[i], 
               sep = ""),
         postResample(throwaway_predict
                      , y_list_test[[y_names[i]]]))
  
  
  remove(throwaway_fit)
  remove(throwaway_predict)
}


#show values in confusion matrix
confusionMatrix(data = knn_predict_BUILDINGID, y_test_BUILDINGID$BUILDINGID)
confusionMatrix(data = knn_predict_FLOOR, y_test_FLOOR$FLOOR)
print(knn_outcome_LATITUDE)
print(knn_outcome_LONGITUDE)