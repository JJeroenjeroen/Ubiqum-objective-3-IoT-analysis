#####################################################
# Date:      06-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will contain the data setup for the UJIIndoorLoc dataset 
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################

#setup libraries 
rm(list = ls())
set.seed(124)

library(ggplot2) 
library(readr) 
library(anytime)
library(caret)
library(dplyr)
library(rlist)
library(kernlab)

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")


#Preprocessing
#change class of building and floor to factor
wifi_train$BUILDINGID <- as.factor(wifi_train$BUILDINGID)
wifi_train$FLOOR <- as.factor(wifi_train$FLOOR)

wifi_test$BUILDINGID <- as.factor(wifi_test$BUILDINGID)
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)


#change unix time variable to actual datetime
wifi_train$DateTime <- anytime(wifi_train$TIMESTAMP)
wifi_test$DateTime <- anytime(wifi_test$TIMESTAMP)


#split the dataframe in 2 so the independent variables (WAPS) can be adjusted 
train_set_yvars <- wifi_train[c((ncol(wifi_train)-9):ncol(wifi_train))]
train_set_wapcolumns <- wifi_train[-c((ncol(wifi_train)-9):ncol(wifi_train))]

#remove rows without variance in both test and training set (76 rows)
train_set_yvars <- train_set_yvars[-which(apply(train_set_wapcolumns, 1, var) == 0), ]
train_set_wapcolumns <- train_set_wapcolumns[-which(apply(train_set_wapcolumns, 1, var) == 0), ]


wifi_train <- bind_cols(train_set_wapcolumns, train_set_yvars)

x_list_train <- list()
y_list_train <- list()
x_list_test <- list()
y_list_test <- list()


#Here the values for the data partitioning can be entered
########################################################################

#how big should the data partition be?
no_rows_partition <- 5000

#which values should be added as a dependent variable?
y_names <- c("BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE")


#for loop that creates smaller data frames for each data dependent variable
for (i in 1:length(y_names)){
  set.seed(124)
  
  #create the data partition of the full trainingset  
  train_id <- createDataPartition(y = wifi_train[,c(y_names[i])], 
                                  p = no_rows_partition/nrow(wifi_train), 
                                  list = FALSE)
  
  #use the partition to generate the training set that will be used  
  training <- wifi_train[train_id,]
  
  #remove columns without variance in both test and training set   
  testing <- wifi_test[-which(apply(training, 2, var) == 0)]
  training <- training[-which(apply(training, 2, var) == 0)]
  
  
  #store the training & test set under a seperate name  
  assign(paste("train_set_", y_names[i], sep = ""), training)
  assign(paste("test_set_", y_names[i], sep = ""), testing)
  
  #save the training and testsets as an rds file  
  saveRDS(training, file = paste("train_set_", y_names[i]))
  saveRDS(testing, file = paste("test_set_", y_names[i]))
  
  
  
  #seperate x and y values for each training dataframe 
  #this makes the df for the independent variables (x)
  assign("throwaway_x_train",
         training[ , grepl( "WAP" , names(training))])
  

  
  #seperate x and y values for each test dataframe  
  #this makes the df for the dependent variable (x)
  assign("throwaway_x_test",
         testing[ , grepl( "WAP" , names(testing))])
  
  #take exponent of train and test set
  throwaway_x_train <- exp(throwaway_x_train)
  throwaway_x_test <- exp(throwaway_x_test)
  
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
