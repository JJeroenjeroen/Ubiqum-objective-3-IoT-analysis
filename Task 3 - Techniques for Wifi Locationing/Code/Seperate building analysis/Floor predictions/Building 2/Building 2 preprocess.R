#####################################################
# Date:      14-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will do the preprocsessing for Building 0 
#For more information, visit http://archive.ics.uci.edu/ml/dataB2s/UJIIndoorLoc
#########################################################################################
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/")
source(file = "Setup.R")

#remove files building 0 & 1
remove(Building_0_test, Building_0_train, Building_1_test, Building_1_train)

#split the trainingB2 in 2 so the independent variables (WAPS) can be adjusted 
train_B2_yvars <- Building_2_train[c((ncol(Building_2_train)-8):ncol(Building_2_train))]
train_B2_wapcolumns <- Building_2_train[-c((ncol(Building_2_train)-8):ncol(Building_2_train))]


#split the testB2 in 2 so the independent variables (WAPS) can be adjusted 
test_B2_yvars <- Building_2_test[c((ncol(Building_2_test)-8):ncol(Building_2_test))]
test_B2_wapcolumns <- Building_2_test[-c((ncol(Building_2_test)-8):ncol(Building_2_test))]



#change weaks signals to no signal
train_B2_wapcolumns[train_B2_wapcolumns <= -90] <- 100


#change weaks signals to no signal
train_B2_wapcolumns[train_B2_wapcolumns == 100] <- -100
test_B2_wapcolumns[test_B2_wapcolumns == 100] <- -100

#change too strong signals to no signal
train_B2_wapcolumns[train_B2_wapcolumns >= -30] <- train_B2_wapcolumns[train_B2_wapcolumns >= -30] -30



#make values positive
train_B2_wapcolumns <- 100 + train_B2_wapcolumns
test_B2_wapcolumns <- 100 + test_B2_wapcolumns


#remove rows without variance in both test and training B2 
train_B2_yvars <- train_B2_yvars[-which(apply(train_B2_wapcolumns, 1, var) == 0), ]
train_B2_wapcolumns <- train_B2_wapcolumns[-which(apply(train_B2_wapcolumns, 1, var) == 0), ]



#combine dataframe again and remove the seperate parts
Building_2_train <- bind_cols(train_B2_wapcolumns, train_B2_yvars)
Building_2_test <- bind_cols(test_B2_wapcolumns, test_B2_yvars)



#create empty lists that will be used in the loop
x_list_train <- list()
y_list_train <- list()
x_list_test <- list()
y_list_test <- list()


#Here the values for the data partitioning can be entered
########################################################################

#how big should the data partition be?
no_rows_partition <- 3000

#which values should be added as a dependent variable?
y_names <- c("FLOOR", "LATITUDE", "LONGITUDE")



#change class of building and floor to factor
Building_2_train$BUILDINGID <- as.factor(Building_2_train$BUILDINGID)
Building_2_train$FLOOR <- as.factor(Building_2_train$FLOOR)

Building_2_test$BUILDINGID <- as.factor(Building_2_test$BUILDINGID)
Building_2_test$FLOOR <- as.factor(Building_2_test$FLOOR)


#change unix time variable to actual datetime
Building_2_train$DateTime <- anytime(Building_2_train$TIMESTAMP)
Building_2_test$DateTime <- anytime(Building_2_test$TIMESTAMP)


#for loop that creates smaller data frames for each data dependent variable
for (i in 1:length(y_names)){
  set.seed(124)
  
  #create the data partition of the full trainingset  
  train_id <- createDataPartition(y = Building_2_train[,c(y_names[i])], 
                                  p = no_rows_partition/nrow(Building_2_train), 
                                  list = FALSE)
  
  #use the partition to generate the training set that will be used  
  training <- Building_2_train[train_id,]
  
  #remove columns without variance in both test and training set   
  testing <- Building_2_test[-which(apply(training, 2, var) == 0)]
  training <- training[-which(apply(training, 2, var) == 0)]
  
  
  
  #seperate x and y values for each training dataframe 
  #this makes the df for the independent variables (x)
  assign("throwaway_x_train",
         training[ , grepl( "WAP" , names(training))])
  
  
  
  #seperate x and y values for each test dataframe  
  #this makes the df for the dependent variable (x)
  assign("throwaway_x_test",
         testing[ , grepl( "WAP" , names(testing))])
  
  
  
  #normalize rows in the train & test dataframe 
  throwaway_x_train <- as.data.frame(scale(t(throwaway_x_train)))
  throwaway_x_test <- as.data.frame(scale(t(throwaway_x_test)))       
  throwaway_x_train <- as.data.frame(t(throwaway_x_train))
  throwaway_x_test <- as.data.frame(t(throwaway_x_test))
  
  
  
  #assign training and testing x & y frames seperately in a list  
  x_list_train[[y_names[i]]] <- throwaway_x_train
  y_list_train[y_names[i]] <- training[y_names[i]]
  
  x_list_test[[y_names[i]]] <- throwaway_x_test
  y_list_test[y_names[i]] <- testing[y_names[i]]
  
  remove(throwaway_x_train)
  remove(throwaway_x_test)
  remove(training)
  remove(testing)
}
