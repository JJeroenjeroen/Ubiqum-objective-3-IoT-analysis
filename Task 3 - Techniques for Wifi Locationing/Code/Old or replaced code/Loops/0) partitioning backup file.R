#####################################################
# Date:      04-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   1.0                                    #    
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
library(stringr)

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")





#change class of building and floor to factor
wifi_train$BUILDINGID <- as.factor(wifi_train$BUILDINGID)
wifi_train$FLOOR <- as.factor(wifi_train$FLOOR)
wifi_train$RELATIVEPOSITION <- as.factor(wifi_train$RELATIVEPOSITION)
wifi_train$PHONEID <- as.factor(wifi_train$PHONEID)
wifi_train$USERID <- as.factor(wifi_train$USERID)

wifi_test$BUILDINGID <- as.factor(wifi_test$BUILDINGID)
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)
wifi_test$PHONEID <- as.factor(wifi_test$PHONEID)


#change unix time variable to actual datetime
wifi_train$DateTime <- anytime(wifi_train$TIMESTAMP)
wifi_test$DateTime <- anytime(wifi_test$TIMESTAMP)


#create a vector of names for y values data partitioning
y_names <- c("BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE")




#for loop that creates smaller data frames for each data dependent variable
for (i in 1:length(y_names)){
  set.seed(124)
  
  #create the data partition  
  train_id <- createDataPartition(y = wifi_train[,c(y_names[i])], 
                                  p = 500/nrow(wifi_train), 
                                  list = FALSE)
  
  #make the training set build out of the full dataset  
  training <- wifi_train[train_id,]
  
  #remove columns without variance  
  training <- training[-which(apply(training, 2, var) == 0)]
  
  
  #store train set under a seperate name  
  assign(paste("train_set_", y_names[i], sep = ""), training)
  #save the training set as an rds file  
  saveRDS(training, file = paste("train_set_", y_names[i]))
  
  
  #seperate x and y values for each dataframe  
  #this makes the df for the independent variables (x)
  assign(paste("x_train_",
               y_names[i],
               sep = ""),
         training[ , grepl( "WAP" , names(training))])
  
  #this makes the df for the independent variables (y)  
  assign(paste("y_train_",
               y_names[i],
               sep = ""),
         training[y_names[i]])
  
}




#create lists with all x and y dataframes
x_list  <- list(BUILDINGID = x_train_BUILDINGID, 
                FLOOR =  x_train_FLOOR, 
                LATITUDE =  x_train_LATITUDE, 
                LONGITUDE = x_train_LONGITUDE)

y_list <- c(y_train_BUILDINGID, 
            y_train_FLOOR, 
            y_train_LATITUDE, 
            y_train_LONGITUDE)

