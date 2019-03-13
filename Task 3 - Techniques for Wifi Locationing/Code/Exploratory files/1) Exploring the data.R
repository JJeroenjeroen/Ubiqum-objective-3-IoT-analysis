#####################################################
# Date:      04-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   1.0                                    #    
#####################################################


#This file will contain the data exploration for the UJIIndoorLoc dataset 
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
library(tidyr)

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")

wifi_train$train <- "train"
wifi_test$train <- "test"

wifi_train <- bind_rows(wifi_train, wifi_test)


ggplot(wifi_train) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = train)) + 
  facet_wrap("FLOOR")

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


#remove columns with zero variance
wifi_test <- wifi_test[-which(apply(wifi_train, 2, var) == 0)]
wifi_train <- wifi_train[-which(apply(wifi_train, 2, var) == 0)]


#make a long dataset for exploratory analysis
train_set_long <- wifi_train[ , c((ncol(wifi_train)-10):(ncol(wifi_train)), 1:(ncol(wifi_train)-11))]
train_set_long <- gather(train_set_long, WPA, dnB, 12:ncol(train_set_long))
train_set_long_connected <- train_set_long %>% filter(train_set_long$dnB < 1)

#plot the distribution of WPA values
ggplot(train_set_long_connected) +
  geom_density(aes(x = dnB)) + 
  facet_wrap(~BUILDINGID, scale="free")
  

ggplot(train_set_long_connected) +
  geom_density(aes(x = dnB)) +
  facet_wrap(~FLOOR, scale="free")




