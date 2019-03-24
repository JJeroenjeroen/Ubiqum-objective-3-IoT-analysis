#####################################################
# Date:      24-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   final                                  #    
#####################################################


#This file will give store the results that were gathered when modeling floors for building 0
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 0")
source(file = "2) Building 0 caret.R")

#set working directory to get the results
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results")

#read results into R
predictions <- readRDS(paste(Sys.Date(), "Building 0 floor"))


#add all actual y values in 1 dataframe with the results
predictions <- data.frame(predictions)
y_df_test <- data.frame(y_list_test)
all_y_values <- bind_cols(y_df_test, predictions)


#store all y values:
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results/total")
saveRDS(all_y_values,
        file = paste(Sys.Date(), "BUilding 0", "total"))

