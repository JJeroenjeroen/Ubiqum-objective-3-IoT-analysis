#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Time Series Setup: Adding averages for each sub-meter
###############################################


#make dataframe for daily average global active power used 
df_daily_GAP <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Global_active_power))
df_daily_GAP <- df_daily_GAP %>% select(ds = YYMMDD, y = y)

#remove days where family not at home in august 2008
df_daily_GAP <- df_daily_GAP %>% filter(y > 0.24)

#generate a boxplot
df_daily_GAP$Year <- year(df_daily_GAP$ds)
df_daily_GAP$Month <- month(df_daily_GAP$ds)
df_daily_GAP$Quarter <- quarter(df_daily_GAP$ds)

#remove outliers for 2007
outliers_GAP_2007 <- df_daily_GAP %>% filter(Year == 2007)
outliers_GAP_2007_2 <- boxplot(outliers_GAP_2007$y ~ outliers_GAP_2007$Month, main="Global active power used per month")$out
outliers_GAP_2007[which(outliers_GAP_2007$y %in% outliers_GAP_2007_2),] <- NA


#remove outliers for 2008
outliers_GAP_2008 <- df_daily_GAP %>% filter(Year == 2008) 
outliers_GAP_2008_2 <- boxplot(outliers_GAP_2008$y ~ outliers_GAP_2008$Month, main="Global active power used per month")$out
outliers_GAP_2008[which(outliers_GAP_2008$y %in% outliers_GAP_2008_2),] <- NA

#remove outliers for 2009
outliers_GAP_2009 <- df_daily_GAP %>% filter(Year == 2009) 
outliers_GAP_2009_2 <- boxplot(outliers_GAP_2009$y ~ outliers_GAP_2009$Month, main="Global active power used per month")$out
outliers_GAP_2009[which(outliers_GAP_2009$y %in% outliers_GAP_2009_2),] <- NA


#add dataframe together again
df_daily_GAP <- rbind(outliers_GAP_2007, outliers_GAP_2008, outliers_GAP_2009)






#make a dataframe hourly average sub_meter_1 used 
#######################################################
df_hourly_subm1 <- Full_dataset %>% group_by(YYMMDD, Hour) %>% summarise(y = mean(Sub_metering_1))

#Add correct time so it can be read by prophet package in 2010
df_hourly_subm1$Minute = 0
df_hourly_subm1$Second = 0
df_hourly_subm1 <- unite(df_hourly_subm1, Time, Hour, Minute, Second, sep = ":", remove = TRUE)
df_hourly_subm1 <- cbind(df_hourly_subm1, "ds" =
                        paste (df_hourly_subm1$YYMMDD,
                               df_hourly_subm1$Time))
df_hourly_subm1$ds <- as.POSIXct(df_hourly_subm1$ds, "%Y/%m/%d %H:%M:%S")
attr(df_hourly_subm1$ds, "tzone") <- "Europe/Paris"
df_hourly_subm1 <- df_hourly_subm1 %>% filter(YYMMDD > "2009-12-20")  %>% select(y, ds)



####################################################


#make a dataframe for daily sums of kilowatts for each submeter 
df_daily_subm1 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Sub_metering_1))
df_daily_subm1 <- df_daily_subm1 %>% select(ds = YYMMDD, y = y)

df_daily_subm2 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Sub_metering_2))
df_daily_subm2 <- df_daily_subm2 %>% select(ds = YYMMDD, y = y)

df_daily_subm3 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Sub_metering_3))
df_daily_subm3 <- df_daily_subm3 %>% select(ds = YYMMDD, y = y)


#make a dataframe hourly averages of each submeter 
df_hourly_subm <- Full_dataset %>% group_by(YYMMDD, Hour) %>% summarise(hourly_SM1 = mean(Sub_metering_1), 
                                                                hourly_SM2 = mean(Sub_metering_2), 
                                                                hourly_SM3 = mean(Sub_metering_3))




remove(weather_data)
remove(yr_2007_test)
remove(yr_2008_test)
remove(yr_2009_test)
remove(yr_2010_test)
