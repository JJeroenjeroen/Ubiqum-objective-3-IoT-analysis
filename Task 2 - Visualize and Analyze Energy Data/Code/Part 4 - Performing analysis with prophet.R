#####################################################
# Date:      19-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################



##Setup time series analysis
###################################################

## Create TS for daily average energy useage (full dataset)
TS_dataset_GAP <- prophet(df_daily_GAP, yearly.seasonality=TRUE)
TS_future_GAP <- make_future_dataframe(TS_dataset_GAP, periods = 365, freq = "day")
forecast_GAP <- predict(TS_dataset_GAP, TS_future_GAP)
plot(TS_dataset_GAP, forecast_GAP)
prophet_plot_components(TS_dataset_GAP, forecast_GAP)




## Create TS for hourly average energy useage (full dataset)
TS_dataset_subm1 <- prophet(df_hourly_subm1, yearly.seasonality=TRUE)
TS_future_subm1 <- make_future_dataframe(TS_dataset_subm1, periods = 240, freq = "hour")
forecast_subm1 <- predict(TS_dataset_subm1, TS_future_subm1)
plot(TS_dataset_subm1, forecast_subm1)
prophet_plot_components(TS_dataset_subm1, forecast_subm1)



############################################



## Create TS for daily average energy useage Sub_meter_1(full dataset) 
TS_dataset_Sub1 <- prophet(df_daily_subm1, yearly.seasonality=TRUE)
TS_future_Sub1 <- make_future_dataframe(TS_dataset_Sub1, periods = 365)
forecast_Sub1 <- predict(TS_dataset_Sub1, TS_future_Sub1)
plot(TS_dataset_Sub1, forecast_Sub1)
prophet_plot_components(TS_dataset_Sub1, forecast_Sub1)





## Create TS for daily average energy useage Sub_meter_2(full dataset) 
TS_dataset_Sub2 <- prophet(df_daily_subm2, yearly.seasonality=TRUE)
TS_future_Sub2 <- make_future_dataframe(TS_dataset_Sub2, periods = 365)
forecast_Sub2 <- predict(TS_dataset_Sub2, TS_future_Sub2)
plot(TS_dataset_Sub2, forecast_Sub2)
prophet_plot_components(TS_dataset_Sub2, forecast_Sub2)



## Create TS for daily average energy useage Sub_meter_3(full dataset) 
TS_dataset_Sub3 <- prophet(df_daily_subm3, yearly.seasonality=TRUE)
TS_future_Sub3 <- make_future_dataframe(TS_dataset_Sub3, periods = 365)
forecast_Sub3 <- predict(TS_dataset_Sub3, TS_future_Sub3)
plot(TS_dataset_Sub3, forecast_Sub3)
prophet_plot_components(TS_dataset_Sub3, forecast_Sub3)
