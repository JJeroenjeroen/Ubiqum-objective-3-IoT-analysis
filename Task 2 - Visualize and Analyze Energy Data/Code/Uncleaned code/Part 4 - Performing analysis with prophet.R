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

TS_dataset_GAP <- add_regressor(TS_dataset_GAP, 'rainfall')

TS_dataset_GAP <- fit.prophet(TS_dataset_GAP,
                              df_daily_GAP)

TS_future_GAP <- make_future_dataframe(TS_dataset_GAP,
                                       periods = 365,
                                       freq = "day")

TS_future_GAP$ds <- date(TS_future_GAP$ds)

TS_future_GAP <- left_join(TS_future_GAP,
                           weather_data %>% 
                             select(ds,
                                    rainfall = Precipitation.amount.in.mm),
                           by = "ds")

forecast_GAP <- predict(TS_dataset_GAP,
                        TS_future_GAP)

forecast_GAP$yhat_lower[forecast_GAP$yhat_lower < 0] <- 0




#cross validation & performance statistics of prophet
TS_dataset_GAP.cv <- (cross_validation(TS_dataset_GAP, 
                                       initial = 730, 
                                       period = 180, 
                                       horizon = 365, 
                                       units = 'days'))
head(TS_dataset_GAP.cv)

TS_dataset_GAP.p <- performance_metrics(TS_dataset_GAP.cv)
head(TS_dataset_GAP.p)



