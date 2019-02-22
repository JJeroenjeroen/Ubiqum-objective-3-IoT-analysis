#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Time Series Setup: Adding averages for each sub-meter
###############################################

#make dataframe for daily average global active power used 
df_daily_GAP <- Full_dataset %>% 
  group_by(YYMMDD) %>% 
  summarise(y = mean(Global_active_power),
            temperature = mean((Maximum.temperature.Celsius + Minimum.temperature..Celsius)/2),
            rainfall = mean(Precipitation.amount.in.mm))

df_daily_GAP <- df_daily_GAP %>% 
  select(ds = YYMMDD, y = y,
         temperature = temperature,
         rainfall = rainfall)


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
  ds = c(seq.Date(from = date('2008-08-04'), to = date('2008-08-30'), by = "day")),
         #seq.Date(from = date('2011-07-06'), to = date('2011-09-03'), by = "day")), 
  lower_window = 0,
  upper_window = 1)


#add a holiday where they leave more items on than they do in 2008
Summer_holidays2 <- data_frame(
  holiday = 'Super Holidays',
  ds = c(seq.Date(from = date('2007-07-29'), to = date('2007-07-31'), by = "day"), 
    seq.Date(from = date('2009-08-02'), to = date('2009-08-08'), by = "day"),
    seq.Date(from = date('2010-07-29'), to = date('2010-08-14'), by = "day")),
  #seq.Date(from = date('2011-07-06'), to = date('2011-09-03'), by = "day")), 
  lower_window = 0,
  upper_window = 1)


#School holiday dates:
Saint_holidays <- data_frame(
  holiday = 'All Saints Day',
  ds = c(seq.Date(from = date('2007-10-28'), to = date('2007-11-03'), by = "day"), 
         seq.Date(from = date('2008-10-26'), to = date('2008-11-03'), by = "day"),
         seq.Date(from = date('2009-10-26'), to = date('2009-10-28'), by = "day"),
         seq.Date(from = date('2010-10-25'), to = date('2010-11-03'), by = "day"),
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
  ds = c(seq.Date(from = date('2007-02-24'), to = date('2007-03-03'), by = "day"), 
         seq.Date(from = date('2008-02-24'), to = date('2008-03-03'), by = "day"),
         seq.Date(from = date('2009-02-17'), to = date('2009-02-18'), by = "day"),
         seq.Date(from = date('2010-02-28'), to = date('2010-03-07'), by = "day"),
         seq.Date(from = date('2011-02-12'), to = date('2011-02-27'), by = "day")),
  lower_window = 0,
  upper_window = 1)


#School holiday dates:
Easter_holidays <- data_frame(
  holiday = 'Easter Holidays',
  ds = c(seq.Date(from = date('2007-04-09'), to = date('2007-04-20'), by = "day"), 
         seq.Date(from = date('2008-04-26'), to = date('2008-05-01'), by = "day"),
         seq.Date(from = date('2009-04-23'), to = date('2009-04-24'), by = "day"),
         seq.Date(from = date('2010-04-28'), to = date('2010-04-30'), by = "day"),
         seq.Date(from = date('2011-04-08'), to = date('2011-04-25'), by = "day")), 
  lower_window = 0,
  upper_window = 1)

School_holidays <- bind_rows(summer_holidays,
                             Summer_holidays2, 
                             Saint_holidays, 
                             Christmas_holidays, 
                             Winter_holidays, 
                             Easter_holidays)

