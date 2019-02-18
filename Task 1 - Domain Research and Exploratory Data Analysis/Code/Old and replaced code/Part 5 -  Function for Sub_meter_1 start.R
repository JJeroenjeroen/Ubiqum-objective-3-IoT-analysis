#####################################################
# Date:      16-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.1                                    #    
#####################################################


#function telling whether Dishwasher, microwave or oven are turned on or off
#######################################################################



#first name the function and make it ask the person calling for it what time he wants to know what is going on

sub_meter1_on <- function(){
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
  
#for ome items we need a broad range of minutes since we need to find nearby maxima
###Note: the way it currently works, issues arise when minute is 00 or minute is 59 is entered. 
### I still need to find out how to get around this
  
  func1_2hour <- func1_dataset %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute > (func1_mins - 60),
           Minute < (func1_mins + 60))
  
  
#this part helps with the conditions for the precise starting pattern of dishwasher and oven
 
   func1_4minutes <- func1_dataset %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute > (func1_mins - 2),
           Minute < (func1_mins + 2))
  
  
  
#For the following filters I use the previously filtered dataset intead of the full set to save calculation time
 
   func1_microwave <- func1_2hour %>% 
    filter(Year == func1_year,
           Month == func1_month,
           Day == func1_day,
           Hour ==  func1_hrs,
           Minute == func1_mins)
  
  
  
#provide condition whether sub_meter3 gives indication that the microwave is on
  
  if(
    func1_microwave$Sub_metering_1 > 14 &
    func1_microwave$Sub_metering_1 < 19){
    Microwave <- "Your microwave is on"
  } else {
    Microwave <- "Your microwave is off"
  }
  
  

     
#This part should correctly estimate whether the dishwasher is on by itself
#The first condition should make sure that its on during the exact time the client requested
#The second and third condition provide the dish-washer specific electricity usage pattern 
 
   if(
      (nrow(filter(func1_4minutes, Sub_metering_1 >= 1))) > 3 &
      (max(func1_2hour$Sub_metering_1 > 35)) &
      (nrow(filter(func1_2hour, Sub_metering_1 == 1))) > 20){
      
    Dishwasher <- "Your dish washer is on"
  } else {
    Dishwasher <- "Your dish washer is off"
  }
  
  
  func1_plot <- ggplot(func1_dataset, aes(x = DateTime, y = Sub_metering_1)) + geom_line()
  
  #list both results in a list so it can be returned
  
  sub_meter_3_result <- list(func1_plot, Microwave, Dishwasher)
  
  return(sub_meter_3_result)
  }

sub_meter1_on()

2008
04
15
13
40
