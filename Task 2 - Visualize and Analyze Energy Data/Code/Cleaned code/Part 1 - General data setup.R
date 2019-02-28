#####################################################
# Date:      08-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################

#Data setup
#############

#Clear current global environment, Set seed, Change sytem time to provide english names
rm(list = ls())
set.seed(123)
Sys.setlocale("LC_TIME", "C")

#This line of code installs, if not installed yet, the pacman page and loads the packages used in the code
if(!require(pacman))install.packages("pacman")



pacman::p_load('dplyr', 'bbplot', 'lubridate', 'RMySQL', 'tidyr',
               'gapminder', 'readr', 'ggplot2',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales', 'prophet')
               



# Create a database connection
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')



#Save the datasets from server
yr_2007_test <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008_test <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009_test <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010_test <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")



# Combine tables into one dataframe using dplyr
Full_dataset <- bind_rows(yr_2007_test, 
                          yr_2008_test, 
                          yr_2009_test, 
                          yr_2010_test)


# Combine Date and Time attribute values in a new attribute column
Full_dataset <- cbind(Full_dataset, "DateTime" =
                        paste (Full_dataset$Date,
                               Full_dataset$Time),
                      stringsAsFactors=FALSE)



# Move the DateTime attribute within the dataset
Full_dataset <- Full_dataset[,c(ncol(Full_dataset),
                                1:(ncol(Full_dataset)-1))]



#Remove the old date & time variables
Full_dataset$Date <- NULL
Full_dataset$Time <- NULL



#Convert DateTime from POSIXlt to POSIXct
Full_dataset$DateTime <- as.POSIXct(Full_dataset$DateTime, "%Y/%m/%d %H:%M:%S")



# Add the time zone
attr(Full_dataset$DateTime, "tzone") <- "Europe/Paris"


#add time-specific variables depending on the DateTime variable
Full_dataset$Year <- year(Full_dataset$DateTime)
Full_dataset$Quarter <- quarter(Full_dataset$DateTime)
Full_dataset$Month <- month(Full_dataset$DateTime)
Full_dataset$Week <- week(Full_dataset$DateTime)
Full_dataset$Weekday <- weekdays(Full_dataset$DateTime)
Full_dataset$Day <- day(Full_dataset$DateTime)
Full_dataset$Hour <- hour(Full_dataset$DateTime)
Full_dataset$Minute <- minute(Full_dataset$DateTime)


#give names to seasons for use when making plots
Full_dataset$Quarter <- factor(Full_dataset$Quarter,
                               levels = c(1:4),
                               labels = c("Winter",
                                          "Spring",
                                          "Summer",
                                          "Fall"))

