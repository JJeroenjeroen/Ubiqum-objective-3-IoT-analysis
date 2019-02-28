#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Time Series Setup: Adding averages for each sub-meter
###############################################

#make TS dataframe for daily average global active power used, with daily precipitation rate as extra variable
df_daily_GAP <- Full_dataset %>% 
  group_by(ds = YYMMDD) %>% 
  summarise(y = mean(Global_active_power),
            rainfall = mean(Precipitation.amount.in.mm))


#Make a dataframe for the school-holidays
#########################################


#School holiday dates:
#Add the summer holidays where they turned almost everydthing off
summer_holidays <- data_frame(
  holiday = 'summer Holidays',
  ds = c(seq.Date(from = date('2008-08-04'),
                  to = date('2008-08-30'),
                  by = "day")),
  lower_window = 0,
  upper_window = 1)


#add the summer holiday where they leave more items on than they do in 2008
Summer_holidays2 <- data_frame(
  holiday = 'Super Holidays',
  
  ds = c(seq.Date(from = date('2007-07-29'), to = date('2007-07-31'), by = "day"), 
         seq.Date(from = date('2009-08-02'), to = date('2009-08-08'), by = "day"),
         seq.Date(from = date('2010-07-29'), to = date('2010-08-14'), by = "day")),
  lower_window = 0,
  upper_window = 1)


#All Saints holiday dates:
Saint_holidays <- data_frame(
  holiday = 'All Saints Day',
  ds = c(seq.Date(from = date('2007-10-28'), to = date('2007-11-03'), by = "day"), 
         seq.Date(from = date('2008-10-26'), to = date('2008-11-03'), by = "day"),
         seq.Date(from = date('2009-10-26'), to = date('2009-10-28'), by = "day"),
         seq.Date(from = date('2010-10-25'), to = date('2010-11-03'), by = "day"),
         seq.Date(from = date('2011-10-22'), to = date('2011-11-02'), by = "day")), 
  lower_window = 0,
  upper_window = 1)


#Christmas holiday dates:
Christmas_holidays <- data_frame(
  holiday = 'Christmas Holidays',
  ds = c(seq.Date(from = date('2007-01-01'), to = date('2007-01-08'), by = "day"), 
         seq.Date(from = date('2007-12-23'), to = date('2008-01-06'), by = "day"),
         seq.Date(from = date('2008-12-20'), to = date('2009-01-05'), by = "day"),
         seq.Date(from = date('2009-12-19'), to = date('2010-01-04'), by = "day"),
         seq.Date(from = date('2010-12-18'), to = date('2011-01-02'), by = "day")), 
  lower_window = 0,
  upper_window = 1)



#Winter holiday dates:
Winter_holidays <- data_frame(
  holiday = 'Winter Holidays',
  ds = c(seq.Date(from = date('2007-02-24'), to = date('2007-03-03'), by = "day"), 
         seq.Date(from = date('2008-02-24'), to = date('2008-03-03'), by = "day"),
         seq.Date(from = date('2009-02-17'), to = date('2009-02-18'), by = "day"),
         seq.Date(from = date('2010-02-28'), to = date('2010-03-07'), by = "day"),
         seq.Date(from = date('2011-02-12'), to = date('2011-02-27'), by = "day")),
  lower_window = 0,
  upper_window = 1)


#Easter holiday dates:
Easter_holidays <- data_frame(
  holiday = 'Easter Holidays',
  ds = c(seq.Date(from = date('2007-04-09'), to = date('2007-04-20'), by = "day"), 
         seq.Date(from = date('2008-04-26'), to = date('2008-05-01'), by = "day"),
         seq.Date(from = date('2009-04-23'), to = date('2009-04-24'), by = "day"),
         seq.Date(from = date('2010-04-28'), to = date('2010-04-30'), by = "day"),
         seq.Date(from = date('2011-04-08'), to = date('2011-04-25'), by = "day")), 
  lower_window = 0,
  upper_window = 1)

#Combine all school holidays into 1 df
School_holidays <- bind_rows(summer_holidays,
                             Summer_holidays2, 
                             Saint_holidays, 
                             Christmas_holidays, 
                             Winter_holidays, 
                             Easter_holidays)


#add the holiday dates to the dataset which will be used for prophet analysis
df_daily_GAP <- left_join(df_daily_GAP, School_holidays, by = "ds")

#generate an extra dummy telling whether it is a holiday at the specific date or not
df_daily_GAP$holiday_dummy <- df_daily_GAP$upper_window 
df_daily_GAP$holiday_dummy[is.na(df_daily_GAP$holiday_dummy)] <- 0

#remove variables which will not be used
df_daily_GAP <- df_daily_GAP %>% select(-lower_window, -upper_window)
