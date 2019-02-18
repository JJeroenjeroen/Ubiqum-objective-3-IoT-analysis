#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Start Visualizing time-series data
#######################################################################

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(Full_dataset, Year == 2008 & Month == 1 & Day == 9)

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        type = 'scatter', 
        mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(Full_dataset, Year == 2008 & Month == 1 & Day == 9 & 
                       (Minute == 0 | Minute == 10 | Minute == 20 | Minute == 30 | Minute == 40 | Minute == 50))


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



## Subset the 11th week of January 2008 - hourly frequency
houseWeek <- Full_dataset %>% filter(Year == 2008 & Week == 11) 


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 2-hourly frequency
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$hourly_SM1, 
        name = 'Kitchen', type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseWeek$hourly_SM2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseWeek$hourly_SM3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption week 11, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#ggplot for sub-meter 1,2 and 3 with title - 2-hourly frequency

ggplot(houseWeek) + 
  geom_line(aes(x = DateTime, y = daily_SM1), colour = "blue", size = 1) +
  geom_line(aes(x = DateTime, y = daily_SM2.y), colour = "orange", size = 1) +
  geom_line(aes(x = DateTime, y = daily_SM3.y), colour = "green", size = 1) +
  theme_classic() + 
  labs(x = "Time", y = "Power (watt-hours)",
       title = "Power Consumption week 11, 2008")




## Subset January 2008, daily frequency 
houseMonth <- filter(Full_dataset, Year == 2008 & Month == 1 )


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 2-hourly frequency
plot_ly(houseMonth, x = ~houseMonth$DateTime, y = ~houseMonth$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseMonth$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseMonth$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption week 11, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#ggplot for sub-meter 1,2 and 3 with title

ggplot(houseMonth) + 
  geom_line(aes(x = DateTime, y = daily_SM1), colour = "blue", size = 1) +
  geom_line(aes(x = DateTime, y = daily_SM2.y), colour = "orange", size = 1) +
  geom_line(aes(x = DateTime, y = daily_SM3.y), colour = "green", size = 1) +
  theme_classic() + 
  labs(x = "Time", y = "Power (watt-hours)",
       title = "Power Consumption January, 2008")

