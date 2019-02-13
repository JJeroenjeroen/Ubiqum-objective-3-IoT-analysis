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
Full_dataset <- unite(Full_dataset, YYMMDD, Year, Month, Day, sep = "-", remove = FALSE)
Full_dataset$YYMMDD <- as.Date(Full_dataset$YYMMDD)
Full_dataset <- left_join(Full_dataset, weather_data %>% 
                            select(YYMMDD,
                                   Mean.temperature.Celsius, 
                                   Minimum.temperature..Celsius,
                                   Maximum.temperature.Celsius,
                                   Precipitation.amount.in.mm),
                          by = "YYMMDD")


#Create smaller set for faster calculations
Sample_dataset <- sample_n(Full_dataset, 30000)


