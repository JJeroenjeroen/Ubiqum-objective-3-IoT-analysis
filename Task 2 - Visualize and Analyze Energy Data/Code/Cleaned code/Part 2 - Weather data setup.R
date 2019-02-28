#####################################################
# Date:      13-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Import weather data 
#######################################################################
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 1 - Domain Research and Exploratory Data Analysis/Code/Paris daily weather text files")
weather_data <- read.csv("Paris weather csv.csv", sep = ";")


#Parse dates to useable format
weather_data$YYMMDD = ymd(weather_data$Date)

#Create the same value in the full dataset and combine the data
Full_dataset <- unite(Full_dataset, YYMMDD, Year, Month, Day, sep = "-", remove = FALSE)
Full_dataset$YYMMDD <- as.Date(Full_dataset$YYMMDD)

#Only import precipitation amount as this is the only variable used in time series analysis
Full_dataset <- left_join(Full_dataset, weather_data %>% 
                            select(YYMMDD,
                                   Precipitation.amount.in.mm),
                          by = "YYMMDD")


#add a Date variable named DS that is later used to add precipitation rate to the prophet datafile when forecasting
weather_data$ds <- date(weather_data$YYMMDD)

