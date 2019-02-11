#####################################################
# Date:      08-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################

#Data setup
#############


#call packages
rm(list = ls())
set.seed(123)
library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)




# Create a database connection 
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database 
dbListTables(con)


# Lists attributes contained in a table
dbListFields(con,'yr_2006')


#Save the datasets from server 
yr_2007_test <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008_test <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009_test <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

# Combine tables into one dataframe using dplyr
Full_dataset <- bind_rows(yr_2007_test, yr_2008_test, yr_2009_test)

# Combine Date and Time attribute values in a new attribute column
Full_dataset <- cbind(Full_dataset, "DateTime" =
                      paste (Full_dataset$Date,
                             Full_dataset$Time),
                      stringsAsFactors=FALSE)

# Move the DateTime attribute within the dataset
Full_dataset <- Full_dataset[,c(ncol(Full_dataset),
                                1:(ncol(Full_dataset)-1))]


#Remove old date & time variables
Full_dataset$Date <- NULL 
Full_dataset$Time <- NULL


#Convert DateTime from POSIXlt to POSIXct 
Full_dataset$DateTime <- as.POSIXct(Full_dataset$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(Full_dataset$DateTime, "tzone") <- "Europe/Paris"
#############

#Add variables and make data better for usasge
#############

#add variables for different timeperiods
Full_dataset$Year <- year(Full_dataset$DateTime)
Full_dataset$Quarter <- quarter(Full_dataset$DateTime)
Full_dataset$Month <- month(Full_dataset$DateTime)
Full_dataset$Week <- week(Full_dataset$DateTime)
Full_dataset$Weekday <- weekdays(Full_dataset$DateTime)
Full_dataset$Day <- day(Full_dataset$DateTime)
Full_dataset$Hour <- hour(Full_dataset$DateTime)
Full_dataset$Minute <- minute(Full_dataset$DateTime)


#Generate a variable that splits the day in 4 parts: Morning, Afternoon, Evening & Night

Full_dataset <- Full_dataset %>%  
  mutate(Daytime = 
           ifelse(Hour <  6, "Night", 
                  ifelse(Hour >= 6 & Hour < 12, "Morning", 
                         ifelse(Hour >= 12 & Hour < 18, "Afternoon", "Evening"))))


#Generate a variable that splits a week in Weekends and Weekdays

Full_dataset <- Full_dataset %>%  
  mutate(Weekend = 
           ifelse(Weekday ==  "zaterdag" | 
                    Weekday ==  "zondag", "Weekend", "Weekday"))




#Order dataset correctly and remove values from 2010 
Full_dataset <- arrange(Full_dataset,
                        Year,
                        Month,
                        Week,
                        Weekday,
                        Day,
                        Hour,
                        Minute)

Full_dataset <- filter(Full_dataset,
                       Year != 2010)




#Factor the necessary variables and label them
#############

#Years
Full_dataset$Year <- factor(Full_dataset$Year,
                                   levels = c(2007:2009),
                                   labels = c("2007",
                                              "2008",
                                              "2009"))
#Seasons
Full_dataset$Quarter <- factor(Full_dataset$Quarter,
                            levels = c(1:4),
                            labels = c("Winter",
                                       "Spring",
                                       "Summer",
                                       "Fall"))


#Months
Full_dataset$Month <- factor(Full_dataset$Month,
                            levels = c(1:12),
                            labels = c("January",
                                       "February",
                                       "March",
                                       "April",
                                       "May",
                                       "June",
                                       "July",
                                       "August",
                                       "September",
                                       "October",
                                       "November",
                                       "December"))

#Weekdays
Full_dataset$Weekday <- as.factor(Full_dataset$Weekday)


#Week numbers
Full_dataset$Week <- as.factor(Full_dataset$Week)


#Day of the month
Full_dataset$Day <- as.factor(Full_dataset$Day)

#Time of the day
Full_dataset$Daytime <- as.factor(Full_dataset$Daytime)                                                 


#############



#Add final variable and create smaller dataset
#############


#add variable for total energy consumption:
Full_dataset <- mutate(Full_dataset,
                       Total_Energy_cons =  Sub_metering_1 + Sub_metering_2 + Sub_metering_3)




#Create smaller set for faster calculations
Sample_dataset <- sample_n(Full_dataset, 30000)
ggplot(Sample_dataset, aes(x = Total_Energy_cons)) + geom_histogram()



summary(Full_dataset)
