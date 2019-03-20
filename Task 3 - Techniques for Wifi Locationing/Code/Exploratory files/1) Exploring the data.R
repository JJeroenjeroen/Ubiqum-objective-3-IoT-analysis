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
library(bbplot)

#import data
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles")
wifi_train <- read.csv("trainingData.csv", header=TRUE, row.names=NULL, sep = ",")
wifi_test <- read.csv("validationData.csv", header=TRUE, row.names=NULL, sep = ",")

wifi_train$train <- "train"
wifi_test$train <- "test"

wifi_train <- bind_rows(wifi_train, wifi_test)


ggplot(wifi_train) +
  geom_hline(yintercept = 4864900, colour = "white") +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = BUILDINGID)) +
  bbc_style() +
  theme(legend.position = "none", 
        plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 20),
        axis.text.x = element_text(hjust = 1, angle = 0, size = 17),
        axis.text.y = element_text(hjust = 1, angle = 90, size = 17)) +
  labs(title="Latitude and Longitude for each observation",
       subtitle = "An example of how positions in the buildings look like as datapoints") +
  scale_x_continuous(breaks = c(-7500),
                     labels = c("Longitude")) +
  scale_y_continuous(breaks = c(4864900),
                     labels = c("Latitude"))




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



#make a long dataset for exploratory analysis
train_set_long <- wifi_train[c((ncol(wifi_train)-11):(ncol(wifi_train)), 1:(ncol(wifi_train)-12))]
train_set_long <- gather(train_set_long, WPA, dnB, 12:ncol(train_set_long))
train_set_long_connected <- train_set_long %>% filter(train_set_long$dnB < 0)


train_set_long_connected <- train_set_long_connected %>% select(WPA, dnB)
#plot the distribution of WPA values
ggplot(train_set_long_connected) +
  geom_density(aes(x = dnB), size = 0.8, colour = "black") + 
  geom_hline(yintercept = 0.045, colour = "white") +
  bbc_style() +
  theme(legend.position = "right",
        plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 20),
        axis.text.x = element_text(hjust = 1, angle = 25, size = 17)) +
     labs(title="dBm distibution in the dataset",
       subtitle = "All three buildings combined. Observations with no signal were removed")+
  scale_x_continuous(breaks = c(-100, -75, -50, -25, 0),
                     labels = c("-100", "-75", "-50", "-25", "dBm")) +
  scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.045),
                     labels = c("0", "0.01", "0.02", "0.03", "0.04", "Probability"))



#plot the distribution of WPA values
ggplot(train_set_long_connected) +
  geom_point(aes(x = LATITUDE, y = LONGITUDE), size = 0.8, colour = "black") + 
  bbc_style() +
  theme(legend.position = "right",
        plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 20),
        axis.text.x = element_text(hjust = 1, angle = 25, size = 17)) +
  labs(title="Geometrical positions of the data")


table(train_set_long_connected$dnB)
library(ggplot2)
library(bbplot)
