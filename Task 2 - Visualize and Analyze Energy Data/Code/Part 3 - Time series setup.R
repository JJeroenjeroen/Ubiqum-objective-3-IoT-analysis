#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Time Series Setup: Adding averages for each sub-meter
###############################################

#make dataframe for daily average global active power used 
df_daily_GAP <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Global_active_power), 
                                                                temperature = mean((Maximum.temperature.Celsius+Minimum.temperature..Celsius)/2),
                                                                rainfall = mean(Precipitation.amount.in.mm))

df_daily_GAP <- df_daily_GAP %>% select(ds = YYMMDD, y = y, temperature = temperature, rainfall = rainfall)
df_daily_GAP$ds <- date(df_daily_GAP$ds)
df_daily_GAP <- df_daily_GAP %>% filter(!ds >= "2010-01-01")


#remove days where family not at home in august 2008

df_daily_GAP$y[df_daily_GAP$ds >= "2008-08-06" & df_daily_GAP$ds <= "2008-08-25"] <- NA

  
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


#remove outliers for 2009
outliers_GAP_2010 <- df_daily_GAP %>% filter(Year == 2010) 
outliers_GAP_2010_2 <- boxplot(outliers_GAP_2010$y ~ outliers_GAP_2010$Month, main="Global active power used per month")$out
outliers_GAP_2010[which(outliers_GAP_2010$y %in% outliers_GAP_2009_2),] <- NA



#add dataframe together again
df_daily_GAP <- rbind(outliers_GAP_2007, outliers_GAP_2008, outliers_GAP_2009, outliers_GAP_2010)


#Do the same for different sub-meters
##########################################

#make dataframe for daily use of submeter 1
df_daily_subm1 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Sub_metering_1))
df_daily_subm1 <- df_daily_subm1 %>% select(ds = YYMMDD, y = y)
df_daily_subm1$y[df_daily_subm1$ds >= "2008-08-06" & df_daily_subm1$ds <= "2008-08-31"] <- NA


#generate a boxplot
df_daily_subm1$Year <- year(df_daily_subm1$ds)
df_daily_subm1$Month <- month(df_daily_subm1$ds)
df_daily_subm1$Quarter <- quarter(df_daily_subm1$ds)

#remove outliers for 2007
outliers_subm1_2007 <- df_daily_subm1 %>% filter(Year == 2007)
outliers_subm1_2007_2 <- boxplot(outliers_subm1_2007$y ~ outliers_subm1_2007$Month, main="Global active power used per month")$out
outliers_subm1_2007[which(outliers_subm1_2007$y %in% outliers_subm1_2007_2),] <- NA


#remove outliers for 2008
outliers_subm1_2008 <- df_daily_subm1 %>% filter(Year == 2008) 
outliers_subm1_2008_2 <- boxplot(outliers_subm1_2008$y ~ outliers_subm1_2008$Month, main="Global active power used per month")$out
outliers_subm1_2008[which(outliers_subm1_2008$y %in% outliers_subm1_2008_2),] <- NA

#remove outliers for 2009
outliers_subm1_2009 <- df_daily_subm1 %>% filter(Year == 2009) 
outliers_subm1_2009_2 <- boxplot(outliers_subm1_2009$y ~ outliers_subm1_2009$Month, main="Global active power used per month")$out
outliers_subm1_2009[which(outliers_subm1_2009$y %in% outliers_subm1_2009_2),] <- NA


#remove outliers for 2009
outliers_subm1_2010 <- df_daily_subm1 %>% filter(Year == 2010) 
outliers_subm1_2010_2 <- boxplot(outliers_subm1_2010$y ~ outliers_subm1_2010$Month, main="Global active power used per month")$out
outliers_subm1_2010[which(outliers_subm1_2010$y %in% outliers_subm1_2009_2),] <- NA



#add dataframe together again
df_daily_subm1 <- rbind(outliers_subm1_2007, outliers_subm1_2008, outliers_subm1_2009, outliers_subm1_2010)




#make dataframe for daily use of submeter 2 
df_daily_subm2 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Sub_metering_2))
df_daily_subm2 <- df_daily_subm2 %>% select(ds = YYMMDD, y = y)
df_daily_subm2$y[df_daily_subm2$ds >= "2008-08-06" & df_daily_subm2$ds <= "2008-08-31"] <- NA


#generate a boxplot
df_daily_subm2$Year <- year(df_daily_subm2$ds)
df_daily_subm2$Month <- month(df_daily_subm2$ds)
df_daily_subm2$Quarter <- quarter(df_daily_subm2$ds)

#remove outliers for 2007
outliers_subm2_2007 <- df_daily_subm2 %>% filter(Year == 2007)
outliers_subm2_2007_2 <- boxplot(outliers_subm2_2007$y ~ outliers_subm2_2007$Month, main="Global active power used per month")$out
outliers_subm2_2007[which(outliers_subm2_2007$y %in% outliers_subm2_2007_2),] <- NA


#remove outliers for 2008
outliers_subm2_2008 <- df_daily_subm2 %>% filter(Year == 2008) 
outliers_subm2_2008_2 <- boxplot(outliers_subm2_2008$y ~ outliers_subm2_2008$Month, main="Global active power used per month")$out
outliers_subm2_2008[which(outliers_subm2_2008$y %in% outliers_subm2_2008_2),] <- NA

#remove outliers for 2009
outliers_subm2_2009 <- df_daily_subm2 %>% filter(Year == 2009) 
outliers_subm2_2009_2 <- boxplot(outliers_subm2_2009$y ~ outliers_subm2_2009$Month, main="Global active power used per month")$out
outliers_subm2_2009[which(outliers_subm2_2009$y %in% outliers_subm2_2009_2),] <- NA


#remove outliers for 2009
outliers_subm2_2010 <- df_daily_subm2 %>% filter(Year == 2010) 
outliers_subm2_2010_2 <- boxplot(outliers_subm2_2010$y ~ outliers_subm2_2010$Month, main="Global active power used per month")$out
outliers_subm2_2010[which(outliers_subm2_2010$y %in% outliers_subm2_2009_2),] <- NA



#add dataframe together again
df_daily_subm2 <- rbind(outliers_subm2_2007, outliers_subm2_2008, outliers_subm2_2009, outliers_subm2_2010)





#make dataframe for daily use of submeter 3 
df_daily_subm3 <- Full_dataset %>% group_by(YYMMDD) %>% summarise(y = mean(Sub_metering_3))
df_daily_subm3 <- df_daily_subm3 %>% select(ds = YYMMDD, y = y)
df_daily_subm3$y[df_daily_subm3$ds >= "2008-08-06" & df_daily_subm3$ds <= "2008-08-31"] <- NA


#generate a boxplot
df_daily_subm3$Year <- year(df_daily_subm3$ds)
df_daily_subm3$Month <- month(df_daily_subm3$ds)
df_daily_subm3$Quarter <- quarter(df_daily_subm3$ds)

#remove outliers for 2007
outliers_subm3_2007 <- df_daily_subm3 %>% filter(Year == 2007)
outliers_subm3_2007_2 <- boxplot(outliers_subm3_2007$y ~ outliers_subm3_2007$Month, main="Global active power used per month")$out
outliers_subm3_2007[which(outliers_subm3_2007$y %in% outliers_subm3_2007_2),] <- NA


#remove outliers for 2008
outliers_subm3_2008 <- df_daily_subm3 %>% filter(Year == 2008) 
outliers_subm3_2008_2 <- boxplot(outliers_subm3_2008$y ~ outliers_subm3_2008$Month, main="Global active power used per month")$out
outliers_subm3_2008[which(outliers_subm3_2008$y %in% outliers_subm3_2008_2),] <- NA

#remove outliers for 2009
outliers_subm3_2009 <- df_daily_subm3 %>% filter(Year == 2009) 
outliers_subm3_2009_2 <- boxplot(outliers_subm3_2009$y ~ outliers_subm3_2009$Month, main="Global active power used per month")$out
outliers_subm3_2009[which(outliers_subm3_2009$y %in% outliers_subm3_2009_2),] <- NA


#remove outliers for 2009
outliers_subm3_2010 <- df_daily_subm3 %>% filter(Year == 2010) 
outliers_subm3_2010_2 <- boxplot(outliers_subm3_2010$y ~ outliers_subm3_2010$Month, main="Global active power used per month")$out
outliers_subm3_2010[which(outliers_subm3_2010$y %in% outliers_subm3_2009_2),] <- NA



#add dataframe together again
df_daily_subm3 <- rbind(outliers_subm3_2007, outliers_subm3_2008, outliers_subm3_2009, outliers_subm3_2010)








#Make a dataframe for the school-holidays
#########################################

National_holidays <- data_frame(
  holiday = 'National Holidays',
  ds = as.Date(c('2007-01-01', '2008-01-01', '2009-01-01', '2010-01-01',    #NYD
                 '2007-04-06', '2008-03-21', '2009-04-10', '2010-04-02',    #Good friday
                 '2007-04-09', '2008-03-25', '2009-04-13', '2010-04-05',    #Easter monday 
                 '2007-05-01', '2008-05-01', '2009-05-01', '2010-05-01',    #Labour day
                 '2007-05-08', '2008-05-08', '2009-05-08', '2010-05-08',    #end of WW2
                 '2007-05-17', '2009-05-21', '2010-05-13',                  #day of ascension
                 '2007-07-14', '2008-07-14', '2009-07-14', '2010-07-14',    #Bastille day
                 '2007-08-15', '2008-08-15', '2009-08-15', '2010-08-15',    #Assumption of Mary to Heaven
                 '2007-11-01', '2008-11-01', '2009-11-01', '2010-11-01',    #All Saints' Day
                 '2007-11-11', '2008-11-11', '2009-11-11', '2010-11-11',    #end of WW1
                 '2007-25-12', '2008-25-12', '2009-25-12'                   #Christmas
  )),
  lower_window = 0,
  upper_window = 1
)



#School holiday dates:
summer_holidays <- data_frame(
  holiday = 'summer Holidays',
  ds = c(seq.Date(from = date('2007-07-04'), to = date('2007-09-03'), by = "day"), 
         seq.Date(from = date('2008-07-04'), to = date('2008-09-03'), by = "day"),
         seq.Date(from = date('2009-07-02'), to = date('2009-09-01'), by = "day"),
         seq.Date(from = date('2010-07-02'), to = date('2010-09-01'), by = "day"),
         seq.Date(from = date('2011-07-06'), to = date('2011-09-03'), by = "day")), 
  lower_window = 0,
  upper_window = 1)




#School holiday dates:
Saint_holidays <- data_frame(
  holiday = 'All Saints Day',
  ds = c(seq.Date(from = date('2007-10-28'), to = date('2007-11-07'), by = "day"), 
         seq.Date(from = date('2008-10-25'), to = date('2008-11-06'), by = "day"),
         seq.Date(from = date('2009-10-24'), to = date('2009-11-05'), by = "day"),
         seq.Date(from = date('2010-10-23'), to = date('2010-11-03'), by = "day"),
         seq.Date(from = date('2011-10-22'), to = date('2011-11-02'), by = "day")), 
  lower_window = 0,
  upper_window = 1)


#School holiday dates:
Christmas_holidays <- data_frame(
  holiday = 'Christmas Holidays',
  ds = c(seq.Date(from = date('2007-01-01'), to = date('2007-01-08'), by = "day"), 
         seq.Date(from = date('2007-12-23'), to = date('2008-01-06'), by = "day"),
         seq.Date(from = date('2008-12-20'), to = date('2009-01-05'), by = "day"),
         seq.Date(from = date('2009-12-19'), to = date('2010-01-04'), by = "day"),
         seq.Date(from = date('2010-12-18'), to = date('2011-01-02'), by = "day")), 
  lower_window = 0,
  upper_window = 1)


#School holiday dates:
Winter_holidays <- data_frame(
  holiday = 'Winter Holidays',
  ds = c(seq.Date(from = date('2007-02-17'), to = date('2007-03-05'), by = "day"), 
         seq.Date(from = date('2008-02-24'), to = date('2008-03-09'), by = "day"),
         seq.Date(from = date('2009-02-14'), to = date('2009-03-02'), by = "day"),
         seq.Date(from = date('2010-02-20'), to = date('2010-03-08'), by = "day"),
         seq.Date(from = date('2011-02-12'), to = date('2011-02-27'), by = "day")),
  lower_window = 0,
  upper_window = 1)


#School holiday dates:
Easter_holidays <- data_frame(
  holiday = 'Easter Holidays',
  ds = c(seq.Date(from = date('2007-04-07'), to = date('2007-04-23'), by = "day"), 
         seq.Date(from = date('2008-04-20'), to = date('2008-05-04'), by = "day"),
         seq.Date(from = date('2009-04-11'), to = date('2009-04-27'), by = "day"),
         seq.Date(from = date('2010-04-17'), to = date('2010-05-03'), by = "day"),
         seq.Date(from = date('2011-04-08'), to = date('2011-04-25'), by = "day")), 
  lower_window = 0,
  upper_window = 1)

School_holidays <- bind_rows(summer_holidays, 
                             Saint_holidays, 
                             Christmas_holidays, 
                             Winter_holidays, 
                             Easter_holidays)

