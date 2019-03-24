#####################################################
# Date:      24-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   final                                  #    
#####################################################


#This file will do the preprocsessing for Building 1 
#For more information, visit http://archive.ics.uci.edu/ml/dataB1s/UJIIndoorLoc
#########################################################################################
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Location predictions/")
source(file = "Setup.R")

#remove files building 0 & 2
remove(Building_0_test, Building_0_train, Building_2_test, Building_2_train)

#split the trainingB1 in 2 so the independent variables (WAPS) can be adjusted 
train_B1_yvars <- Building_1_train[c((ncol(Building_1_train)-8):
                                       ncol(Building_1_train))]
train_B1_wapcolumns <- Building_1_train[-c((ncol(Building_1_train)-8):
                                             ncol(Building_1_train))]


#split the testB1 in 2 so the independent variables (WAPS) can be adjusted 
test_B1_yvars <- Building_1_test[c((ncol(Building_1_test)-8):
                                     ncol(Building_1_test))]
test_B1_wapcolumns <- Building_1_test[-c((ncol(Building_1_test)-8):
                                           ncol(Building_1_test))]


#create dataframes for the BUILDINGID & floor specifically
building_floor_train <- train_B1_yvars %>% select(BUILDINGID, FLOOR)
Long_lat_train <- train_B1_yvars %>% select(LONGITUDE, LATITUDE)

#add BUILDINGID & floor for later preprocessing
train_B1_wapcolumns <- bind_cols(train_B1_wapcolumns, building_floor_train)

#add longitude and latitude to set
train_B1_wapcolumns <- bind_cols(train_B1_wapcolumns, Long_lat_train)

#remove dataframes with lat&lot & building and floor again
remove(building_floor_train, Long_lat_train)

#add ID to train and test sets
train_B1_yvars$ID <- seq.int(nrow(train_B1_yvars)) + 101
train_B1_wapcolumns$ID <- seq.int(nrow(train_B1_wapcolumns)) + 101


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


#change WAP values of specific part of floor 1
B1_floor1_train[B1_floor1_train < -99] <- 105

B1_floor0_train[B1_floor0_train < -99] <- 105

#change values of rest
B1_floor_rest_train[B1_floor_rest_train < -99] <- 100


#bind rows back together
train_B1_wapcolumns <- bind_rows(B1_floor1_train, B1_floor_rest_train, B1_floor0_train)

#remove the dataframes for the specific floors
remove(B1_floor1_train, B1_floor_rest_train, B1_floor0_train)

#remove BUILDINGID, FLOOR, LAT & LONG
train_B1_wapcolumns$BUILDINGID <-  NULL
train_B1_wapcolumns$FLOOR <- NULL
train_B1_wapcolumns$LATITUDE <-  NULL
train_B1_wapcolumns$LONGITUDE <-  NULL


#combine dataframe again and remove ID so later on rowvariance can be calculated more easily
Building_1_train <- left_join(train_B1_wapcolumns, train_B1_yvars, by = "ID")

#remove ID´s
Building_1_train$ID <- NULL

#split train set of B1 in 2 again so the independent variables (WAPS) can be adjusted 
train_B1_yvars <- Building_1_train[c((ncol(Building_1_train)-8):
                                       ncol(Building_1_train))]
train_B1_wapcolumns <- Building_1_train[-c((ncol(Building_1_train)-8):
                                             ncol(Building_1_train))]

#split the testB1 in 2 so the independent variables (WAPS) can be adjusted 
test_B1_yvars <- Building_1_test[c((ncol(Building_1_test)-8):
                                     ncol(Building_1_test))]
test_B1_wapcolumns <- Building_1_test[-c((ncol(Building_1_test)-8):
                                           ncol(Building_1_test))]

#change weaks signals to no signal
train_B1_wapcolumns[train_B1_wapcolumns == 100] <- -100
test_B1_wapcolumns[test_B1_wapcolumns == 100] <- -100

#change too strong signals to no signal
train_B1_wapcolumns[train_B1_wapcolumns > -30] <- -100

#remove rows without variance in training B1 
train_B1_yvars <- train_B1_yvars[-which(apply(
  train_B1_wapcolumns, 1, var) == 0), ]
train_B1_wapcolumns <- train_B1_wapcolumns[-which(apply(
  train_B1_wapcolumns, 1, var) == 0), ]


#remove duplicate values
train_B1_yvars <- train_B1_yvars[which
                                 (!duplicated(train_B1_wapcolumns)),]
train_B1_wapcolumns <- train_B1_wapcolumns[which
                                           (!duplicated(train_B1_wapcolumns)),]

#combine dataframes again 
Building_1_train <- bind_cols(train_B1_wapcolumns, train_B1_yvars)
Building_1_test <- bind_cols(test_B1_wapcolumns, test_B1_yvars)

#change class of building and floor to factor
Building_1_train$BUILDINGID <- as.factor(Building_1_train$BUILDINGID)
Building_1_train$FLOOR <- as.factor(Building_1_train$FLOOR)

Building_1_test$BUILDINGID <- as.factor(Building_1_test$BUILDINGID)
Building_1_test$FLOOR <- as.factor(Building_1_test$FLOOR)


#change unix time variable to actual datetime
Building_1_train$DateTime <- anytime(Building_1_train$TIMESTAMP)
Building_1_test$DateTime <- anytime(Building_1_test$TIMESTAMP)



#create empty lists that will be used in the loop
x_list_train <- list()
y_list_train <- list()
x_list_test <- list()
y_list_test <- list()


#Here the values for the data partitioning can be entered
########################################################################

#how big should the data partition be?
no_rows_partition <- 2000

#which values should be added as a dependent variable?
y_names <- c("LONGITUDE", "LATITUDE")



#for loop that creates smaller data frames for each data dependent variable
for (i in 1:length(y_names)){
  set.seed(124)
  
  #create the data partition of the full trainingset  
  train_id <- createDataPartition(y = Building_1_train[,c(y_names[i])], 
                                  p = no_rows_partition/nrow(Building_1_train), 
                                  list = FALSE)
  
  #use the partition to generate the training set that will be used  
  training <- Building_1_train[train_id,]
  
  #remove columns without variance in both test and training set   
  testing <- Building_1_test[-which(apply(training, 2, var) == 0)]
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


