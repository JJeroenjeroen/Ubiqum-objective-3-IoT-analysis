#####################################################
# Date:      11-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################

#Use Dplyr to filter and select the data


#################################################
#group by various time periods and give averages#
#################################################

#Total per weekday
Sample_dataset %>% 
  group_by(Weekday) %>%
  summarise(avg_energy_cons = mean(Global_active_power, na.rm = TRUE))


#Submeter 1 per month
Sample_dataset %>% 
  group_by(Month) %>%
  summarise(avg_energy_sub1 = mean(Sub_metering_1, na.rm = TRUE))


#Submeter 2 per month
Sample_dataset %>% 
  group_by(Month) %>%
  summarise(avg_energy_sub2 = mean(Sub_metering_2, na.rm = TRUE))

#Submeter 3 per month
Sample_dataset %>% 
  group_by(Month) %>%
  summarise(avg_energy_sub3 = mean(Sub_metering_3, na.rm = TRUE))




##############
#yearly stats#
##############

#give the max energy used on a given time by sub_meter_3 for 2007 & 2008
Full_dataset %>% 
  filter(DateTime > "2007-01-01" & DateTime < "2007-12-31") %>%
  summarise(max_energy_2007 = max(Sub_metering_3))

Full_dataset %>% 
  filter(DateTime > "2008-01-01" & DateTime < "2008-12-31") %>%
  summarise(max_energy_2008 = max(Sub_metering_3))




#########
#ggplots#
#########

#create a plot for yearly energy use
data_1_year <- Full_dataset %>% 
  filter(DateTime > "2007-01-06" & DateTime < "2007-12-07")


ggplot(data_1_year, aes(x = DateTime, y = Global_active_power)) + geom_smooth()


#create a plot for daily use of the water heater
data_1_day <- Full_dataset %>% 
  filter(DateTime > "2007-11-13" & DateTime < "2007-11-14")

#sub_meter_3 plotted 
ggplot(data_1_day, aes(x = DateTime, y = Sub_metering_3)) + 
  geom_line(size = .75, color = "deepskyblue") +  
  theme_classic() +
    labs(x = "Day", y = "kWh",
       title = "Water Heater Usage On A Given Day") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=22, colour = "navyblue", face="bold", family = "Perpetua"),
        axis.title.x = element_text(size = 18, colour = "navyblue", family = "Perpetua"),
        axis.title.y = element_text(size = 18, colour = "navyblue", family = "Perpetua"))



#Global active power plotted over the day
ggplot(data_1_day, aes(x = DateTime, y = (Global_active_power*60))) + 
  geom_line(size = .75, color = "deepskyblue") +  
  theme_classic() +
  labs(x = "Day", y = "kWh",
       title = "Global Active Power Usage On A Given Day") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=22, colour = "navyblue", face="bold", family = "Perpetua"),
        axis.title.x = element_text(size = 18, colour = "navyblue", family = "Perpetua"),
        axis.title.y = element_text(size = 18, colour = "navyblue", family = "Perpetua"))


windowsFonts()

#create a plot for daily use of the airconditioner

data_2_day <- Full_dataset %>% 
  filter(DateTime > "2008-08-06" & DateTime < "2008-08-07")

ggplot(data_2_day, aes(x = DateTime, y = Sub_metering_3)) + 
  geom_line(size = 0.8, color = "deepskyblue") +  
  theme_classic() +
  labs(x = "Day", y = "kWh",
       title = "Air conditioner Usage On A Given Day") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=22, colour = "navyblue", face="bold", family = "Perpetua"),
        axis.title.x = element_text(size = 18, colour = "navyblue", family = "Perpetua"),
        axis.title.y = element_text(size = 18, colour = "navyblue", family = "Perpetua"))




#create a plot where both items were used

data_3_day <- Full_dataset %>% 
  filter(DateTime > "2009-09-06" & DateTime < "2009-09-07")

ggplot(data_3_day, aes(x = DateTime, y = Sub_metering_3)) + 
  geom_line(size = 0.8, color = "deepskyblue2") +  
  theme_classic() +
  labs(x = "Day", y = "kWh", title = "Total kWh On A Given Day Of One Submeter") +
  theme(axis.text=element_text(size=15),
      axis.title=element_text(size=14,face="bold"),
      plot.title = element_text(size=22, colour = "navyblue", face="bold", family = "Perpetua"),
      axis.title.x = element_text(size = 18, colour = "navyblue", family = "Perpetua"),
      axis.title.y = element_text(size = 18, colour = "navyblue", family = "Perpetua"))





#summarize monthly weather statistics and plot
weather_electricity <- Full_dataset %>% 
  group_by(Month) %>%
  summarise(avg_temperature = mean(Mean.temperature.Celsius, na.rm = TRUE),
            avg_high_temperature = mean(Maximum.temperature.Celsius, na.rm = TRUE),
            avg_low_temperature = mean(Minimum.temperature..Celsius, na.rm = TRUE),
            avg_electricity = mean(Global_active_power, na.rm = TRUE),
            avg_rain = mean(Precipitation.amount.in.mm, na.rm = TRUE),
            avg_sub_1 = mean(Sub_metering_1, na.rm = TRUE),
            avg_sub_2 = mean(Sub_metering_2, na.rm = TRUE),
            avg_sub_3 = mean(Sub_metering_3, na.rm = TRUE))



## Add a column indicating whether it is summer or winter
weather_electricity <- weather_electricity %>% mutate( Season = ifelse( Month >3 & Month < 10, "Summer", "Winter" ) )

#plot monthly avg temperature monthly energy use on 1 plot  

weather_electricity$Month <- factor(weather_electricity$Month,
                                       levels = c(1:12),
                                       labels = c("Jan",
                                                  "Feb",
                                                  "Mar",
                                                  "Apr",
                                                  "May",
                                                  "Jun",
                                                  "Jul",
                                                  "Aug",
                                                  "Sep",
                                                  "Oct",
                                                  "Nov",
                                                  "Dec"))



##plot barchart with average electricity used

ggplot(weather_electricity) + 
  geom_col(mapping =
             aes(x = Month,
                 y = avg_temperature,
                 fill = "Average temperature in Celsius"),
           size = 1.2,
           alpha = 0.6) +
  scale_fill_manual(values=c("grey")) + 
  
  geom_line(mapping =
              aes(x = Month,
                  y = (avg_electricity * max(weather_electricity$avg_temperature)*0.5),
                  colour = "Avg electricity used in Kilowatt",
                  group = 1),
            size = 1,
            alpha = 1) +
  
  scale_color_manual(values=c("red")) + 
  scale_y_continuous( sec.axis = sec_axis(name = "Global minute-averaged active power (in kilowatt)", ~./max(weather_electricity$avg_temperature)*2)) +
  theme_classic() +
  
  labs(x = "Month", y = "Celsius",
    title = "Temperature & Electricity",
        subtitle = "A negative correlation")



#Hline giving mean electicity usage in summer months
#######################

ggplot(weather_electricity) + 
  geom_col(mapping =
             aes(x = Month,
                 y = avg_temperature,
                 fill = Season),
           size = 1.2,
           alpha = 0.6) +
  
  scale_fill_manual(values=c("Summer" = "orange", "Winter" = "grey")) +
  

  geom_hline(yintercept = mean(
    weather_electricity$avg_electricity[weather_electricity$Season == "Summer"] * max(weather_electricity$avg_temperature)*0.5),
    color= "red") +
  
  scale_y_continuous( sec.axis = sec_axis(name = "Global minute-averaged active power (in kilowatt)", ~./max(weather_electricity$avg_temperature)*2)) +
  
  theme_classic() +
  
  labs(x = "Month", y = "Celsius",
       title = "Temperature & Electricity",
       subtitle = "Low mean during summer months")



#Hline giving mean electicity usage in winter months
#######################

ggplot(weather_electricity) + 
  
  geom_col(mapping =
             aes(x = Month,
                 y = avg_temperature,
                 fill = Season),
           size = 1.2,
           alpha = 0.6) +
  
  scale_fill_manual(values=c("Summer" = "grey", "Winter" = "blue")) +
 
  geom_hline(yintercept = mean(weather_electricity$avg_electricity[weather_electricity$Season == "Winter"] * max(weather_electricity$avg_temperature)*0.5), color= "Blue") +
  
  scale_y_continuous( sec.axis = sec_axis(name = "Global minute-averaged active power (in kilowatt)", ~./max(weather_electricity$avg_temperature)*2)) +
  
  theme_classic() +
  
  labs(x = "Month", y = "Celsius",
       title = "Temperature & Electricity",
       subtitle = "High mean during winter months")


