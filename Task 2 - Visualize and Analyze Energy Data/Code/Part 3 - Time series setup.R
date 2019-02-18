#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Time Series Setup: Adding averages for each sub-meter
###############################################






#add daily sums of kilowatts for each submeter 
test_1 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(daily_SM1 = sum((1/60)*(Sub_metering_1)), 
                                                          daily_SM2 = sum((1/60)*Sub_metering_2), 
                                                          daily_SM3 = sum((1/60)*Sub_metering_3))



Full_dataset <- left_join(Full_dataset, test_1 %>% 
                            select(YYMMDD,
                                   daily_SM1, 
                                   daily_SM2,
                                   daily_SM3),
                          by = "YYMMDD")


#add hourly averages of each submeter 
test_2 <- Full_dataset %>% group_by(YYMMDD, Hour) %>% summarise(hourly_SM1 = mean(Sub_metering_1), 
                                                                hourly_SM2 = mean(Sub_metering_2), 
                                                                hourly_SM3 = mean(Sub_metering_3))


test_2 <- unite(test_2, YMDH, YYMMDD, Hour, sep = "-", remove = FALSE)
Full_dataset <- unite(Full_dataset, YMDH, YYMMDD, Hour, sep = "-", remove = FALSE)


Full_dataset <- left_join(Full_dataset, test_2 %>% 
                            select(YMDH,
                                   hourly_SM1, 
                                   hourly_SM2,
                                   hourly_SM3),
                          by = "YMDH")



Full_dataset <- Full_dataset %>% select(-YYMMDD.y)
