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
set.seed(127)

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
#split the dataframe in 2 so the independent variables (WAPS) can be adjusted 
#change unix time variable to actual datetime
wifi_train$DateTime <- anytime(wifi_train$TIMESTAMP)
wifi_test$DateTime <- anytime(wifi_test$TIMESTAMP)


train_set_yvars <- wifi_train[c((ncol(wifi_train)-9):ncol(wifi_train))]
train_set_wapcolumns <- wifi_train[-c((ncol(wifi_train)-9):ncol(wifi_train))]

#remove rows without variance in both test and training set (76 rows)
train_set_yvars <- train_set_yvars[-which(apply(train_set_wapcolumns, 1, var) == 0), ]
train_set_wapcolumns <- train_set_wapcolumns[-which(apply(train_set_wapcolumns, 1, var) == 0), ]
train_set_wapcolumns[train_set_wapcolumns == 100] <- -105

#filter rows with WAP values that are higher than /29  
high_value_WAps <-train_set_wapcolumns[which(apply(train_set_wapcolumns, 1, max) >= -29), ]
yvar_error_rows <- train_set_yvars[which(apply(train_set_wapcolumns, 1, max) >= -29), ]
error_rows <- bind_cols(high_value_WAps, yvar_error_rows)

#plot these rows where these high values occur 
ggplot(error_rows) +
  geom_point(aes(x = LATITUDE, y = LONGITUDE, colour = USERID)) +
  facet_wrap("FLOOR")

#make a specific df for values obtained by user 6
USIER6 <- wifi_train %>% filter(USERID == 6)

#plot the values obtained by user 6
ggplot(USIER6) +
  geom_point(aes(x = LATITUDE, y = LONGITUDE, colour = USERID)) +
  facet_wrap("FLOOR")

#make a long dataframe for user 6
USIER6[USIER6 == 100] <- -105
USIER6_long <- USIER6[ , c((ncol(USIER6)-10):(ncol(USIER6)), 1:(ncol(USIER6)-11))]
USIER6_long <- gather(USIER6_long, WPA, dnB, 12:ncol(USIER6_long))
USIER6_long <- USIER6_long %>% filter(USIER6_long$dnB > -100)


ggplot(USIER6_long) +
  geom_density(aes(x = dnB)) +
  facet_wrap(~FLOOR, scale="free")

