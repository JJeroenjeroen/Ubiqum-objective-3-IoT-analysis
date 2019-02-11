#####################################################
# Date:      11-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################

#Use Dplyr to filter and select the data
#########################################


#filter data to get rid of 0/1/2 energy consumption data:

Sample_dataset_filtered <- filter(Sample_dataset, Total_Energy_cons > 30, Total_Energy_cons < 60)
ggplot(Sample_dataset_filtered, aes(x = Total_Energy_cons)) + geom_density()


#Average energy consumptions per month:
#####
#Total
Sample_dataset %>% 
  group_by(Month, Weekday) %>%
  summarise(avg_energy_cons = mean(Total_Energy_cons, na.rm = TRUE))


#Submeter 1
Sample_dataset %>% 
  group_by(Month) %>%
  summarise(avg_energy_sub1 = mean(Sub_metering_1, na.rm = TRUE))


#Submeter 2
Sample_dataset %>% 
  group_by(Month) %>%
  summarise(avg_energy_sub2 = mean(Sub_metering_2, na.rm = TRUE))

#Submeter 3
Sample_dataset %>% 
  group_by(Month) %>%
  summarise(avg_energy_sub3 = mean(Sub_metering_3, na.rm = TRUE))



#Total
Sample_dataset %>% 
  group_by(Daytime) %>%
  summarise(avg_energy_cons = mean(Total_Energy_cons, na.rm = TRUE))

