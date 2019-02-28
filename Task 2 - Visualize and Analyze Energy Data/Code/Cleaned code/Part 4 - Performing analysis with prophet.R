#####################################################
# Date:      20-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#Prophet time series analysis
###################################################

## Create prophet TS for daily average energy useage 
TS_dataset_GAP <- prophet(holidays = School_holidays)

TS_dataset_GAP <- add_regressor(TS_dataset_GAP,
                                'rainfall')

TS_dataset_GAP <- fit.prophet(TS_dataset_GAP,
                              df_daily_GAP)


#Generate the prediction dataframe
TS_future_GAP <- make_future_dataframe(TS_dataset_GAP,
                                       periods = 365,
                                       freq = "day")



#Make time in the future frame the proper class for during the cross validation later on
TS_future_GAP$ds<- date(TS_future_GAP$ds)


#Add precipitation rate for dates that were not in the Electricity consumption dataset
TS_future_GAP <- left_join(TS_future_GAP,
                           weather_data %>% 
                             select(ds,
                                    rainfall = Precipitation.amount.in.mm),
                           by = "ds")

#generate the prophet dataframe with forecasted values
forecast_GAP <- predict(TS_dataset_GAP,
                        TS_future_GAP)


#set the minimum of predicted values' lower bound at 0
forecast_GAP$yhat_lower[forecast_GAP$yhat_lower < 0] <- 0




#cross validation & performance statistics of prophet the prophet model
TS_dataset_GAP.cv <- (cross_validation(TS_dataset_GAP, 
                                       initial = 730, 
                                       period = 180, 
                                       horizon = 365, 
                                       units = 'days'))

TS_dataset_GAP.p <- performance_metrics(TS_dataset_GAP.cv)
head(TS_dataset_GAP.p)



