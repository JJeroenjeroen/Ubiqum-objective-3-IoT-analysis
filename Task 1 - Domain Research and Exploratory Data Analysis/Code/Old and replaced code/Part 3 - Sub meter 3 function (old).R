#####################################################
# Date:      12-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#function telling whether electric water heater and the air conditioning are turned on or off
#######################################################################



#first name the function and make it ask the person calling for it what time he wants to know what is going on
sub_meter3_on <- function(){
  func1_year <- readline("What year is it?")
  func1_month <- readline("What month is it?")
  func1_day <- readline("What day of the month is it?")
  func1_hrs <- readline("What time is it (hrs)?")
  func1_mins <- readline("What time is it (mins)?")
  
#transform the inputs to numbereric valeues that can be used to call the specific date from the full dataset 
  
  func1_year <- as.numeric(unlist(strsplit(func1_year, ",")))
  func1_month <- as.numeric(unlist(strsplit(func1_month, ",")))
  func1_day <- as.numeric(unlist(strsplit(func1_day, ",")))
  func1_hrs <- as.numeric(unlist(strsplit(func1_hrs, ",")))
  func1_mins <- as.numeric(unlist(strsplit(func1_mins, ",")))
  
  
#filter the full dataset to get the specific time period the person calling for the function wanted  
  func1_heater <- Full_dataset %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute == func1_mins)
  

#for air conditioner the time must be taken a bit more broad because when its on it can have 0 kWh on a specific moment but not averaged 
  
  func1_airco <- Full_dataset %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute > (func1_mins - 5),
           Minute < (func1_mins + 5))
  
  
#provide condition whether sub_meter3 gives indication that the water heater is on
  
  if(func1_heater$Sub_metering_3 > 15){
    Water_heater <- "Your water heater is on"
  } else {
    Water_heater <- "Your water heater is off"
  }

  
  
#provide conditions whether sub_meter3 gives indication that the air conditioner is on
  
  if(mean(func1_airco$Sub_metering_3) > 0 &
     mean(func1_airco$Sub_metering_3 < 1) |
     mean(func1_airco$Sub_metering_3 > 4) &
     mean(func1_airco$Sub_metering_3 < 10)){
    airco <- "Your air conditioning is on"
  } else {
    airco <- "Your air conditioning is off"
  }
  
  
#list both results in a list so it can be returned
  
  sub_meter_3_result <- list(Water_heater, airco)
  
  return(sub_meter_3_result)
  
  }

