#####################################################
# Date:      12-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.1                                    #    
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
  #filter for 1 day and use that dataset for further filtering to save calculation time
  
  func1_dataset <- Full_dataset %>% 
    filter(Year == func1_year,
    Month == func1_month,
    Day == func1_day)
  
  #If both items are on, we need a broader range of minutes to test since we need to find some of the maximums
  ###Note: the way it currently works, issues arise when minute is 00 or minute is 59 is entered. 
  ### I still need to find out how to get around this
  
  func1_both <- func1_dataset %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute > (func1_mins - 60),
           Minute < (func1_mins + 60))
  
  
  
  #For the following filters I use the previously filtered dataset intead of the full set to save calculation time
  func1_heater <- func1_both %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute == func1_mins)
  
  
  #for air conditioner the time must be taken a bit more broad because when its on it can have 0 kWh on a specific moment and be on
 
   
  func1_airco <- func1_both %>% 
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
     mean(func1_airco$Sub_metering_3 < 10) | 
     
     
  #This part should correctly estimate whether the airconditioning is on whenever the water heater is on as well
  
     max(func1_both$Sub_metering_3 > 25) |
     
     max(func1_both$Sub_metering_3 > 17) &
     nrow(filter(func1_both , Sub_metering_3 == 1)) > 12 
     ){
    airco <- "Your air conditioning is on"
  } else {
    airco <- "Your air conditioning is off"
  }
  
  
  func1_plot <- ggplot(func1_dataset, aes(x = DateTime, y = Sub_metering_3)) + geom_line()
  
#list both results in a list so it can be returned
  
  sub_meter_3_result <- list(func1_plot, Water_heater, airco)
  
  return(sub_meter_3_result)
  
}

