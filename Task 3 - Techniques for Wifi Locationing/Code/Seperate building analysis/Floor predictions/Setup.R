#####################################################
# Date:      14-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will contain the data setup Seperated per building for the UJIIndoorLoc dataset 
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
library(plotly)
library(doParallel)

#add extra core to calculate
registerDoParallel(1)
getDoParWorkers()

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")

#make seperate traininsets per building
Building_0_train <- wifi_train %>% filter(BUILDINGID == 0)
Building_1_train <- wifi_train %>% filter(BUILDINGID == 1)
Building_2_train <- wifi_train %>% filter(BUILDINGID == 2)

Building_0_test <- wifi_test %>% filter(BUILDINGID == 0)
Building_1_test <- wifi_test %>% filter(BUILDINGID == 1)
Building_2_test <- wifi_test %>% filter(BUILDINGID == 2)

