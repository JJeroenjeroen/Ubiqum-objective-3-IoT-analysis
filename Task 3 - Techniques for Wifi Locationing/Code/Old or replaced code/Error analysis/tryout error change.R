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
library(rlist)
library(kernlab)
library(plotly)
library(tidyr)
library(dplyr)

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")


#Preprocessing
#change unix time variable to actual datetime
wifi_train$DateTime <- anytime(wifi_train$TIMESTAMP)
wifi_test$DateTime <- anytime(wifi_test$TIMESTAMP)



#split the dataframe in 2 so the independent variables (WAPS) can be adjusted 
train_set_yvars <- wifi_train[c((ncol(wifi_train)-9):ncol(wifi_train))]
train_set_wapcolumns <- wifi_train[-c((ncol(wifi_train)-9):ncol(wifi_train))]

#do the same with the test set
test_set_yvars <- wifi_test[c((ncol(wifi_test)-9):ncol(wifi_test))]
test_set_wapcolumns <- wifi_test[-c((ncol(wifi_test)-9):ncol(wifi_test))]

#remove rows without variance in both test and training set (76 rows)
train_set_yvars <- train_set_yvars[-which(apply(train_set_wapcolumns, 1, var) == 0), ]
train_set_wapcolumns <- train_set_wapcolumns[-which(apply(train_set_wapcolumns, 1, var) == 0), ]

#bind columns back together
wifi_train <- bind_cols(train_set_wapcolumns, train_set_yvars)
wifi_test <- bind_cols(test_set_wapcolumns, test_set_yvars)

#make a long dataset for exploratory analysis
train_set_long <- wifi_train[ , c((ncol(wifi_train)-10):(ncol(wifi_train)), 1:(ncol(wifi_train)-11))]
train_set_long <- gather(train_set_long, WPA, dnB, 12:ncol(train_set_long))
train_set_long %>% filter(LATITUDE < 8964800) %>% filter(WPA != 100)




#change class of building and floor to factor
wifi_train$BUILDINGID <- as.factor(wifi_train$BUILDINGID)
wifi_train$FLOOR <- as.factor(wifi_train$FLOOR)

wifi_test$BUILDINGID <- as.factor(wifi_test$BUILDINGID)
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)





#Here the values for the data partitioning can be entered
########################################################################

#how big should the data partition be?
no_rows_partition <- 1500

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
  assign(paste("x_train_",
               y_names[i],
               sep = ""),
         training[ , grepl( "WAP" , names(training))])
  
  
  #this makes the df for the dependent variable (y)  
  assign(paste("y_train_",
               y_names[i],
               sep = ""),
         training[y_names[i]])
  
  
  
  #seperate x and y values for each test dataframe  
  #this makes the df for the dependent variable (x)
  assign(paste("x_test_",
               y_names[i],
               sep = ""),
         testing[ , grepl( "WAP" , names(testing))])
  
  #this makes the df for the dependent test variable (y)  
  assign(paste("y_test_",
               y_names[i],
               sep = ""),
         testing[y_names[i]])
  
  remove(training)
  remove(testing)
}
#since log10 scale, scale all WAP values for train and set
x_train_BUILDINGID <- 10^(x_train_BUILDINGID)
x_train_FLOOR <- 10^(x_train_FLOOR)
x_train_LATITUDE <- 10^(x_train_LATITUDE)
x_train_LONGITUDE <- 10^(x_train_LONGITUDE)

#take 10^onent of all WAP values test set
x_test_BUILDINGID <- 10^(x_test_BUILDINGID)
x_test_FLOOR <- 10^(x_test_FLOOR)
x_test_LATITUDE <- 10^(x_test_LATITUDE)
x_test_LONGITUDE <- 10^(x_test_LONGITUDE)




#normalize rows in the train dataframe 
x_train_BUILDINGID <- as.data.frame(scale(t(x_train_BUILDINGID)))
x_train_FLOOR <- as.data.frame(scale(t(x_train_FLOOR)))
x_train_LATITUDE <- as.data.frame(scale(t(x_train_LATITUDE)))
x_train_LONGITUDE <- as.data.frame(scale(t(x_train_LONGITUDE)))

x_train_BUILDINGID <- as.data.frame(t(x_train_BUILDINGID))
x_train_FLOOR <- as.data.frame(t(x_train_FLOOR))
x_train_LATITUDE <- as.data.frame(t(x_train_LATITUDE))
x_train_LONGITUDE <- as.data.frame(t(x_train_LONGITUDE))

#normalize the rows of the test dataframe
x_test_BUILDINGID <- as.data.frame(scale(t(x_test_BUILDINGID)))
x_test_FLOOR <- as.data.frame(scale(t(x_test_FLOOR)))
x_test_LATITUDE <- as.data.frame(scale(t(x_test_LATITUDE)))
x_test_LONGITUDE <- as.data.frame(scale(t(x_test_LONGITUDE)))

x_test_BUILDINGID <- as.data.frame(t(x_test_BUILDINGID))
x_test_FLOOR <- as.data.frame(t(x_test_FLOOR))
x_test_LATITUDE <- as.data.frame(t(x_test_LATITUDE))
x_test_LONGITUDE <- as.data.frame(t(x_test_LONGITUDE))




#create lists with all x and y dataframes for training and testsets
x_list_train  <- list(BUILDINGID = x_train_BUILDINGID,
                      FLOOR =  x_train_FLOOR,
                      LATITUDE =  x_train_LATITUDE,
                      LONGITUDE = x_train_LONGITUDE)

y_list_train <- c(y_train_BUILDINGID,
                  y_train_FLOOR, 
                  y_train_LATITUDE,
                  y_train_LONGITUDE)


x_list_test  <- list(BUILDINGID = x_test_BUILDINGID,
                     FLOOR =  x_test_FLOOR,
                     LATITUDE =  x_test_LATITUDE,
                     LONGITUDE = x_test_LONGITUDE)

y_list_test <- c(y_test_BUILDINGID,
                 y_test_FLOOR, 
                 y_test_LATITUDE,
                 y_test_LONGITUDE)

