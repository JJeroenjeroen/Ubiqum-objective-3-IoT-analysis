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

WAPS <- wifi_train[1:520]
Yvalues <- wifi_train[-c(1:520)]

#WAPS[WAPS < -90] <- 100
WAPS[WAPS == 100] <- -105
WAPS[WAPS > -30] <- WAPS[WAPS > -30] -30

#remove empty rows
Yvalues <- Yvalues[-which(apply(WAPS, 1, var)== 0), ]
WAPS <- WAPS[-which(apply(WAPS, 1, var)==0), ]


#bind frame back together
wifi_train <- bind_cols(WAPS, Yvalues)

#group by usedID and position
wifi_train$longlatuser <- paste(wifi_train$LONGITUDE, wifi_train$LATITUDE, wifi_train$FLOOR)
wifi_train = wifi_train[order(wifi_train[,'longlatuser'],-wifi_train[,'TIMESTAMP']),]

#group by for the positions
grouped_set_mean <- wifi_train %>% group_by(longlatuser) %>% summarize_all(mean)
grouped_set_std <- wifi_train %>% group_by(longlatuser) %>% summarize_all(var)

grouped_set_mean$longlatuser = NULL
grouped_set_std$longlatuser = NULL

#waps mean
grouped_set_mean_waps <- grouped_set_mean[1:520]
grouped_set_mean_yvals <- grouped_set_mean[-c(1:520)]
grouped_set_std_waps <- grouped_set_std[1:520]

#take the qrt of the variation
grouped_set_std_waps <- sqrt(grouped_set_std_waps)

#give WAPS an upper and lower bound
group_mean_upper <- (grouped_set_mean_waps +  grouped_set_std_waps)
group_mean_lower <- (grouped_set_mean_waps -  grouped_set_std_waps)

#add y-values to upper and lower bound
group_mean_upper <- bind_cols(group_mean_upper, grouped_set_mean_yvals)
group_mean_lower <- bind_cols(group_mean_lower, grouped_set_mean_yvals)

#add the set together
wifi_train_new <- bind_rows(group_mean_upper, grouped_set_mean, grouped_set_mean, grouped_set_mean, grouped_set_mean, grouped_set_mean, grouped_set_mean, grouped_set_mean, group_mean_lower)

#remove useless sets
remove(group_mean_lower, 
       group_mean_upper, 
       grouped_set_mean, 
       grouped_set_mean_yvals, 
       wifi_train, 
       Yvalues, 
       grouped_set_mean_waps, 
       grouped_set_std, 
       WAPS, grouped_set_std_waps)

#preprocess
train_new_waps <- wifi_train_new[1:520]
train_new_yvals <- wifi_train_new[-c(1:520)]

train_new_waps <- exp(train_new_waps)
train_new_waps[train_new_waps <= 2.506567e-45] <- 0

#normalize data
train_new_waps <- as.data.frame(scale(t(train_new_waps)))
train_new_waps <- as.data.frame(t(train_new_waps))

#bind columns back together
wifi_train_new <- bind_cols(train_new_waps, train_new_yvals)


#test preprocess
Test_waps <- wifi_test[1:520]
Test_yvals <- wifi_test[-c(1:520)]

#turn 100's into no signal
Test_waps[Test_waps == 100] <- -105

#take exponent of the data
Test_waps <- exp(Test_waps)
Test_waps[Test_waps <= 2.506567e-45] <- 0

#normalize data
Test_waps <- as.data.frame(scale(t(Test_waps)))
Test_waps <- as.data.frame(t(Test_waps))

#bind testset back together
wifi_test <- bind_cols(Test_waps, Test_yvals)

