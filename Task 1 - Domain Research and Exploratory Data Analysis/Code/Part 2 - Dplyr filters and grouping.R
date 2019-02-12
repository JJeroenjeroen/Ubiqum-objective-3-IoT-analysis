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


ggplot(data_1_day, aes(x = DateTime, y = Global_active_power)) + geom_smooth()


#create a plot for daily energy use
data_1_day <- Full_dataset %>% 
  filter(DateTime > "2007-11-11" & DateTime < "2007-11-12")

ggplot(data_1_day, aes(x = DateTime, y = Sub_metering_3)) + geom_line()

