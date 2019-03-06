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
library(ggplot2) 
library(readr) 
library(anytime)
library(caret)
library(dplyr)
library(ggthemes)


#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")


#subset for smaller data frame
wifi_partition <- createDataPartition(y = wifi_train$LONGITUDE, 
                                      times = 1, 
                                      p = (500/19937), 
                                      list = FALSE)


wifi_train <- wifi_train[ wifi_partition, ]

#add an ID for each row
wifi_train$ID <- seq.int(nrow(wifi_train))
wifi_test$ID <- seq.int(nrow(wifi_test))


#change class of building and floor to factor
wifi_train$BUILDINGID <- as.factor(wifi_train$BUILDINGID)
wifi_train$FLOOR <- as.factor(wifi_train$FLOOR)
wifi_train$RELATIVEPOSITION <- as.factor(wifi_train$RELATIVEPOSITION)
wifi_train$PHONEID <- as.factor(wifi_train$PHONEID)
wifi_train$USERID <- as.factor(wifi_train$USERID)

wifi_test$BUILDINGID <- as.factor(wifi_test$BUILDINGID)
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)



#change unix time variable to actual datetime
wifi_train$DateTime <- anytime(wifi_train$TIMESTAMP)
wifi_test$DateTime <- anytime(wifi_test$TIMESTAMP)


#distinguish x & y values
wifi_train_xvalues <- wifi_train[,1:520]
wifi_train_yvalues <- wifi_train[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

wifi_test_xvalues <- wifi_test[,1:520]
wifi_test_yvalues <- wifi_test[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]



#make the wifi signals more logical by making no connectiona 0, and perfect connection a 1
wifi_train_xvalues <- wifi_train_xvalues + 105
wifi_train_xvalues[wifi_train_xvalues > 150] <- 0
wifi_train_xvalues <- (wifi_train_xvalues/105)

wifi_test_xvalues <- wifi_test_xvalues + 105
wifi_test_xvalues[wifi_test_xvalues > 150] <- 0
wifi_test_xvalues <- (wifi_test_xvalues/105)

