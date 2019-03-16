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
library(tidyr)
library(rlist)
library(kernlab)

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")


wifi_train$longlatuser <- paste(wifi_train$LONGITUDE, wifi_train$LATITUDE, wifi_train$USERID, wifi_train$FLOOR)
wifi_train = wifi_train[order(wifi_train[,'longlatuser'],-wifi_train[,'TIMESTAMP']),]
wifi_train = wifi_train[!duplicated(wifi_train$longlatuser),]

wifi_train$longlatuser <- NULL

#Preprocessing
#split the trainingset in 2 so the independent variables (WAPS) can be adjusted 
train_set_yvars <- wifi_train[c((ncol(wifi_train)-8):ncol(wifi_train))]
train_set_wapcolumns <- wifi_train[-c((ncol(wifi_train)-8):ncol(wifi_train))]


#split the testset in 2 so the independent variables (WAPS) can be adjusted 
test_set_yvars <- wifi_test[c((ncol(wifi_test)-8):ncol(wifi_test))]
test_set_wapcolumns <- wifi_test[-c((ncol(wifi_test)-8):ncol(wifi_test))]

#create dataframes for the BUILDINGID & floor specifically
building_floor_train <- train_set_yvars %>% select(BUILDINGID, FLOOR)
building_floor_test <- test_set_yvars %>% select(BUILDINGID, FLOOR)
Long_lat_train <- train_set_yvars %>% select(LONGITUDE, LATITUDE)
Long_lat_test <- test_set_yvars %>% select(LONGITUDE, LATITUDE)

#add BUILDINGID & floor for later preprocessing
train_set_wapcolumns <- bind_cols(train_set_wapcolumns, building_floor_train)
test_set_wapcolumns <- bind_cols(test_set_wapcolumns, building_floor_test)

#add longitude and latitude to set
train_set_wapcolumns <- bind_cols(train_set_wapcolumns, Long_lat_train)
test_set_wapcolumns <- bind_cols(test_set_wapcolumns, Long_lat_test)


#add ID to train and test sets
train_set_yvars$ID <- seq.int(nrow(train_set_yvars)) + 101
train_set_wapcolumns$ID <- seq.int(nrow(train_set_wapcolumns)) + 101
test_set_yvars$ID <- seq.int(nrow(test_set_yvars)) + 101
test_set_wapcolumns$ID <- seq.int(nrow(test_set_wapcolumns)) + 101


#split sets depending on BUILDINGID
train_set_wapcolumns_0 <- train_set_wapcolumns %>% filter(BUILDINGID == 0) 
train_set_wapcolumns_1 <- train_set_wapcolumns %>% filter(BUILDINGID == 1)
train_set_wapcolumns_2 <- train_set_wapcolumns %>% filter(BUILDINGID == 2)

test_set_wapcolumns_0 <- test_set_wapcolumns %>% filter(BUILDINGID == 0) 
test_set_wapcolumns_1 <- test_set_wapcolumns %>% filter(BUILDINGID == 1)
test_set_wapcolumns_2 <- test_set_wapcolumns %>% filter(BUILDINGID == 2)


#change weaks signals to no signal
train_set_wapcolumns_0[train_set_wapcolumns_0 <= -90] <- 100
train_set_wapcolumns_2[train_set_wapcolumns_2 <= -90] <- 100



#create specific dataframe for floors of building 1
B1_floor1_train <- train_set_wapcolumns_1 %>% filter(FLOOR == 1 &
                                                       LONGITUDE > -7530 &
                                                       LONGITUDE < -7450 &
                                                       LATITUDE > 4864835 &
                                                       LATITUDE < 4864905)


B1_floor0_train <- train_set_wapcolumns_1 %>% filter(FLOOR == 0 &
                                                       LONGITUDE > -7530 &
                                                       LONGITUDE < -7450 &
                                                       LATITUDE > 4864835 &
                                                       LATITUDE < 4864905)

B1_floor_rest_train <- train_set_wapcolumns_1 %>% filter(!((FLOOR == 1 &
                                                              LONGITUDE > -7530 &
                                                              LONGITUDE < -7450 &
                                                              LATITUDE > 4864835 &
                                                              LATITUDE < 4864905) |
                                                             FLOOR == 0 &
                                                             LONGITUDE > -7530 &
                                                             LONGITUDE < -7450 &
                                                             LATITUDE > 4864835 &
                                                             LATITUDE < 4864905))


#change WAP values of specific part of floor 1
B1_floor1_train[B1_floor1_train < -75] <- 100

#change WAP values of floor 0
B1_floor0_train[B1_floor0_train < -90] <- 100

#change values of rest
B1_floor_rest_train[B1_floor_rest_train < -90] <- 100


train_set_wapcolumns_1 <- bind_rows(B1_floor1_train, B1_floor_rest_train, B1_floor0_train)


#bind rows back together
train_set_wapcolumns <- bind_rows(train_set_wapcolumns_0, train_set_wapcolumns_1, train_set_wapcolumns_2)
test_set_wapcolumns <- bind_rows(test_set_wapcolumns_0, test_set_wapcolumns_1, test_set_wapcolumns_2)

#remove BUILDINGID 
train_set_wapcolumns$BUILDINGID <-  NULL
test_set_wapcolumns$BUILDINGID <-  NULL

train_set_wapcolumns$FLOOR <- NULL
test_set_wapcolumns$FLOOR <- NULL


train_set_wapcolumns$LATITUDE <-  NULL
test_set_wapcolumns$LATITUDE <-  NULL


train_set_wapcolumns$LONGITUDE <-  NULL
test_set_wapcolumns$LONGITUDE <-  NULL


#combine dataframe again and remove ID so later on rowvariance can be calculated more easily
wifi_train <- left_join(train_set_wapcolumns, train_set_yvars, by = "ID")
wifi_test <- left_join(test_set_wapcolumns, test_set_yvars, by = "ID")

#remove ID´s
wifi_train$ID <- NULL
wifi_test$ID <- NULL


#split the trainingset in 2 so the independent variables (WAPS) can be adjusted 
train_set_yvars <- wifi_train[c((ncol(wifi_train)-8):ncol(wifi_train))]
train_set_wapcolumns <- wifi_train[-c((ncol(wifi_train)-8):ncol(wifi_train))]


#split the testset in 2 so the independent variables (WAPS) can be adjusted 
test_set_yvars <- wifi_test[c((ncol(wifi_test)-8):ncol(wifi_test))]
test_set_wapcolumns <- wifi_test[-c((ncol(wifi_test)-8):ncol(wifi_test))]




#change weaks signals to no signal

train_set_wapcolumns[train_set_wapcolumns == 100] <- -100
test_set_wapcolumns[test_set_wapcolumns == 100] <- -100


train_set_wapcolumns[train_set_wapcolumns >= -30] <- train_set_wapcolumns[train_set_wapcolumns >= -30] -30


#remove rows without variance in both test and training set 
train_set_yvars <- train_set_yvars[-which(apply(train_set_wapcolumns, 1, var) == 0), ]
train_set_wapcolumns <- train_set_wapcolumns[-which(apply(train_set_wapcolumns, 1, var) == 0), ]


train_set_wapcolumns <- 100 + train_set_wapcolumns
test_set_wapcolumns <- 100 + test_set_wapcolumns

#combine dataframe again and remove the seperate parts
wifi_train <- bind_cols(train_set_wapcolumns, train_set_yvars)
wifi_test <- bind_cols(test_set_wapcolumns, test_set_yvars)


#remove the dataframes that wont be used anymore
remove(train_set_yvars, train_set_wapcolumns)

#create empty lists that will be used in the loop
x_list_train <- list()
y_list_train <- list()
x_list_test <- list()
y_list_test <- list()


#Here the values for the data partitioning can be entered
########################################################################

#how big should the data partition be?
no_rows_partition <- 1000

#which values should be added as a dependent variable?
y_names <- c("BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE")



#change class of building and floor to factor
wifi_train$BUILDINGID <- as.factor(wifi_train$BUILDINGID)
wifi_train$FLOOR <- as.factor(wifi_train$FLOOR)

wifi_test$BUILDINGID <- as.factor(wifi_test$BUILDINGID)
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)


#change unix time variable to actual datetime
wifi_train$DateTime <- anytime(wifi_train$TIMESTAMP)
wifi_test$DateTime <- anytime(wifi_test$TIMESTAMP)


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
