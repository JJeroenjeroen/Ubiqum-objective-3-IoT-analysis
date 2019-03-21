
#set coss validation parameters
control_method <-"repeatedcv"
control_folds <- 10
control_repeats <- 1
control_search <- "grid"
fitControl <- trainControl(method = control_method,
                           number = control_folds,
                           repeats = control_repeats,
                           search = control_search)

#Normalize data and set training parameters
train_preProc <- c("center", "scale")
train_method = "knn"
train_metric <- "Accuracy"
train_tuneGrid <- expand.grid(.k=c(2))


Latitude <- train_new_yvals["LATITUDE"]
Longitude <- train_new_yvals["LONGITUDE"] 

#train Random Forest Regression model 
set.seed(123)
KNNFit1 <- train(x = train_new_waps,
                 y = Latitude[[1]],
                 method = train_method,
                 trControl = fitControl)

print(KNNFit1)
plot(KNNFit1)


#predict brand outcomes on the testing data
prediction_kNN <- predict(KNNFit1, newdata = Test_waps)
str(Brand_prediction_kNN)

#Provide model accuracy and Kappa
postResample(prediction_kNN, Test_yvals$LATITUDE)

#See the most important variables
varImp(KNNFit1)

#show values in confusion matrix
confusionMatrix(data = Brand_prediction_kNN, Task_2_kNN_Test_set$brand)
