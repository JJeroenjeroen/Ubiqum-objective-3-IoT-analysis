#####################################################
# Date:      14-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will do the preprocsessing for Building 0 
#For more information, visit http://archive.ics.uci.edu/ml/dataB0s/UJIIndoorLoc
#########################################################################################
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions")
source(file = "Setup.R")

#split the trainingB0 in 2 so the independent variables (WAPS) can be adjusted 
train_B0_yvars <- Building_0_train[c((ncol(Building_0_train)-8):ncol(Building_0_train))]
train_B0_wapcolumns <- Building_0_train[-c((ncol(Building_0_train)-8):ncol(Building_0_train))]


#split the testB0 in 2 so the independent variables (WAPS) can be adjusted 
test_B0_yvars <- Building_0_test[c((ncol(Building_0_test)-8):ncol(Building_0_test))]
test_B0_wapcolumns <- Building_0_test[-c((ncol(Building_0_test)-8):ncol(Building_0_test))]



#change weaks signals to no signal
train_B0_wapcolumns[train_B0_wapcolumns <= -90] <- 100


train_B0_wapcolumns[train_B0_wapcolumns >= -30] <- 100


#combine dataframe again and remove the seperate parts
Building_0_train <- bind_cols(train_B0_wapcolumns, train_B0_yvars)
Building_0_test <- bind_cols(test_B0_wapcolumns, test_B0_yvars)


#split the trainingB1 in 2 so the independent variables (WAPS) can be adjusted 
train_B1_yvars <- Building_1_train[c((ncol(Building_1_train)-8):ncol(Building_1_train))]
train_B1_wapcolumns <- Building_1_train[-c((ncol(Building_1_train)-8):ncol(Building_1_train))]


#split the testB1 in 2 so the independent variables (WAPS) can be adjusted 
test_B1_yvars <- Building_1_test[c((ncol(Building_1_test)-8):ncol(Building_1_test))]
test_B1_wapcolumns <- Building_1_test[-c((ncol(Building_1_test)-8):ncol(Building_1_test))]


#create dataframes for the BUILDINGID & floor specifically
building_floor_train <- train_B1_yvars %>% select(BUILDINGID, FLOOR)
building_floor_test <- test_B1_yvars %>% select(BUILDINGID, FLOOR)
Long_lat_train <- train_B1_yvars %>% select(LONGITUDE, LATITUDE)
Long_lat_test <- test_B1_yvars %>% select(LONGITUDE, LATITUDE)

#add BUILDINGID & floor for later preprocessing
train_B1_wapcolumns <- bind_cols(train_B1_wapcolumns, building_floor_train)
test_B1_wapcolumns <- bind_cols(test_B1_wapcolumns, building_floor_test)

#add longitude and latitude to set
train_B1_wapcolumns <- bind_cols(train_B1_wapcolumns, Long_lat_train)
test_B1_wapcolumns <- bind_cols(test_B1_wapcolumns, Long_lat_test)

#remove dataframes with lat&lot & building and floor again
remove(building_floor_train, building_floor_test, Long_lat_train, Long_lat_test)

#add ID to train and test sets
train_B1_yvars$ID <- seq.int(nrow(train_B1_yvars)) + 101
train_B1_wapcolumns$ID <- seq.int(nrow(train_B1_wapcolumns)) + 101
test_B1_yvars$ID <- seq.int(nrow(test_B1_yvars)) + 101
test_B1_wapcolumns$ID <- seq.int(nrow(test_B1_wapcolumns)) + 101


#create specific dataframe for floors of building 1
B1_floor1_train <- train_B1_wapcolumns %>% filter(FLOOR == 1 &
                                                    LONGITUDE > -7530 &
                                                    LONGITUDE < -7450 &
                                                    LATITUDE > 4864835 &
                                                    LATITUDE < 4864905)


B1_floor0_train <- train_B1_wapcolumns %>% filter(FLOOR == 0 &
                                                    LATITUDE < 4864905 & 
                                                    LATITUDE > 4864875 &
                                                    LONGITUDE > -7510 & 
                                                    LONGITUDE < -7470)

B1_floor_rest_train <- train_B1_wapcolumns %>% filter(!((FLOOR == 1 &
                                                           LONGITUDE > -7530 &
                                                           LONGITUDE < -7450 &
                                                           LATITUDE > 4864835 &
                                                           LATITUDE < 4864905) |
                                                          FLOOR == 0 & 
                                                          LATITUDE < 4864905 & 
                                                          LATITUDE > 4864875 &
                                                          LONGITUDE > -7510 & 
                                                          LONGITUDE < -7470))


B1_floor1_test <- test_B1_wapcolumns %>% filter(FLOOR == 1 &
                                                  LONGITUDE > -7530 &
                                                  LONGITUDE < -7450 &
                                                  LATITUDE > 4864835 &
                                                  LATITUDE < 4864905)

B1_floor0_test <- test_B1_wapcolumns %>% filter(FLOOR == 0 & 
                                                  LATITUDE < 4864905 & 
                                                  LATITUDE > 4864875 &
                                                  LONGITUDE > -7510 & 
                                                  LONGITUDE < -7470)

B1_floor_rest_test <- test_B1_wapcolumns %>% filter(!((FLOOR == 1 &
                                                         LONGITUDE > -7530 &
                                                         LONGITUDE < -7450 &
                                                         LATITUDE > 4864835 &
                                                         LATITUDE < 4864905) |
                                                        FLOOR == 0 & 
                                                        LATITUDE < 4864905 & 
                                                        LATITUDE > 4864875 &
                                                        LONGITUDE > -7510 & 
                                                        LONGITUDE < -7470))




#change WAP values of specific part of floor 1
B1_floor1_train[B1_floor1_train < -72] <- 100

B1_floor1_train[B1_floor1_train < -70] <- 100

#change values of rest
B1_floor_rest_train[B1_floor_rest_train < -99] <- 100



#bind rows back together
train_B1_wapcolumns <- bind_rows(B1_floor1_train, B1_floor_rest_train, B1_floor0_train)
test_B1_wapcolumns <- bind_rows(B1_floor1_test, B1_floor_rest_test, B1_floor0_test)

#remove the dataframes for the specific floors
remove(B1_floor1_train, B1_floor_rest_train, B1_floor0_train, B1_floor1_test, B1_floor_rest_test, B1_floor0_test)

#remove BUILDINGID, FLOOR, LAT & LONG
train_B1_wapcolumns$BUILDINGID <-  NULL
test_B1_wapcolumns$BUILDINGID <-  NULL

train_B1_wapcolumns$FLOOR <- NULL
test_B1_wapcolumns$FLOOR <- NULL


train_B1_wapcolumns$LATITUDE <-  NULL
test_B1_wapcolumns$LATITUDE <-  NULL


train_B1_wapcolumns$LONGITUDE <-  NULL
test_B1_wapcolumns$LONGITUDE <-  NULL


#combine dataframe again and remove ID so later on rowvariance can be calculated more easily
Building_1_train <- left_join(train_B1_wapcolumns, train_B1_yvars, by = "ID")
Building_1_test <- left_join(test_B1_wapcolumns, test_B1_yvars, by = "ID")



#add extra information for floor 1 in the trainset
B1_F0_part <- Building_1_train %>% filter(FLOOR == 0 & 
                                            LATITUDE < 4864920 & 
                                            LATITUDE > 4864840 &
                                            LONGITUDE > -7525 & 
                                            LONGITUDE < -7475)



B1_F1_part <- Building_1_train %>% filter(FLOOR == 1 & 
                                            LATITUDE < 4864920 & 
                                            LATITUDE > 4864860 &
                                            LONGITUDE > -7450 & 
                                            LONGITUDE < -7400)



#remove ID´s
Building_1_train$ID <- NULL
Building_1_test$ID <- NULL

#split the trainingB1 in 2 again so the independent variables (WAPS) can be adjusted 
train_B1_yvars <- Building_1_train[c((ncol(Building_1_train)-8):ncol(Building_1_train))]
train_B1_wapcolumns <- Building_1_train[-c((ncol(Building_1_train)-8):ncol(Building_1_train))]

#split the testB1 in 2 so the independent variables (WAPS) can be adjusted 
test_B1_yvars <- Building_1_test[c((ncol(Building_1_test)-8):ncol(Building_1_test))]
test_B1_wapcolumns <- Building_1_test[-c((ncol(Building_1_test)-8):ncol(Building_1_test))]


#change too strong signals to no signal
train_B1_wapcolumns[train_B1_wapcolumns >= -30] <- 100


#combine dataframe again and remove the seperate parts
Building_1_train <- bind_cols(train_B1_wapcolumns, train_B1_yvars)
Building_1_test <- bind_cols(test_B1_wapcolumns, test_B1_yvars)

#split the trainingB2 in 2 so the independent variables (WAPS) can be adjusted 
train_B2_yvars <- Building_2_train[c((ncol(Building_2_train)-8):ncol(Building_2_train))]
train_B2_wapcolumns <- Building_2_train[-c((ncol(Building_2_train)-8):ncol(Building_2_train))]


#split the testB2 in 2 so the independent variables (WAPS) can be adjusted 
test_B2_yvars <- Building_2_test[c((ncol(Building_2_test)-8):ncol(Building_2_test))]
test_B2_wapcolumns <- Building_2_test[-c((ncol(Building_2_test)-8):ncol(Building_2_test))]



#change weaks signals to no signal
train_B2_wapcolumns[train_B2_wapcolumns <= -90] <- 100


#change too strong signals to no signal
train_B2_wapcolumns[train_B2_wapcolumns >= -30] <- train_B2_wapcolumns[train_B2_wapcolumns >= -30] -30



#combine dataframe again and remove the seperate parts
Building_2_train <- bind_cols(train_B2_wapcolumns, train_B2_yvars)
Building_2_test <- bind_cols(test_B2_wapcolumns, test_B2_yvars)


wifi_train <- bind_rows(Building_0_train, Building_1_train, Building_2_train)
wifi_test <- bind_rows(Building_0_test, Building_1_test, Building_2_test)

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
y_names <- c("LATITUDE", "LONGITUDE")



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
  testing <- wifi_test
  
  #seperate x and y values for each training dataframe 
  #this makes the df for the independent variables (x)
  assign("throwaway_x_train",
         training[ , grepl( "WAP" , names(training))])
  
  
  
  #seperate x and y values for each test dataframe  
  #this makes the df for the dependent variable (x)
  assign("throwaway_x_test",
         testing[ , grepl( "WAP" , names(testing))])
  
  
  
  throwaway_x_train <- as.data.frame(exp(throwaway_x_train))
  throwaway_x_test <- as.data.frame(exp(throwaway_x_test))       
  
  #normalize rows in the train & test dataframe 
  throwaway_x_train <- as.data.frame(scale(t(throwaway_x_train)))
  throwaway_x_test <- as.data.frame(scale(t(throwaway_x_test)))       
  throwaway_x_train <- as.data.frame(t(throwaway_x_train))
  throwaway_x_test <- as.data.frame(t(throwaway_x_test))
  
  #remove rows with NA's
  throwaway_x_train[complete.cases(throwaway_x_train), ]
  throwaway_x_test[complete.cases(throwaway_x_test), ]
  
  
  
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


