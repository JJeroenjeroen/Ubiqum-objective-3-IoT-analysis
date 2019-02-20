#####################################################
# Date:      19-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################



##Setup time series analysis
###################################################

## Create prophet TS for daily average energy useage 
TS_dataset_GAP <- prophet(df_daily_GAP, holidays = School_holidays)
TS_future_GAP <- make_future_dataframe(TS_dataset_GAP, periods = 365, freq = "day")
forecast_GAP <- predict(TS_dataset_GAP, TS_future_GAP)
plot(TS_dataset_GAP, forecast_GAP)
prophet_plot_components(TS_dataset_GAP, forecast_GAP)



## Create prophet TS for useage submeter 1
TS_dataset_subm1 <- prophet(df_daily_subm1, holidays = School_holidays)
TS_future_subm1 <- make_future_dataframe(TS_dataset_subm1, periods = 365, freq = "day")
forecast_subm1 <- predict(TS_dataset_subm1, TS_future_subm1)
plot(TS_dataset_subm1, forecast_subm1)
prophet_plot_components(TS_dataset_subm1, forecast_subm1)


## Create prophet TS for useage submeter 2
TS_dataset_subm2 <- prophet(df_daily_subm2, holidays = School_holidays)
TS_future_subm2 <- make_future_dataframe(TS_dataset_subm2, periods = 365, freq = "day")
forecast_subm2 <- predict(TS_dataset_subm2, TS_future_subm2)
plot(TS_dataset_subm2, forecast_subm2)
prophet_plot_components(TS_dataset_subm2, forecast_subm2)



## Create prophet TS for useage submeter 3 
TS_dataset_subm3 <- prophet(df_daily_subm3, holidays = School_holidays)
TS_future_subm3 <- make_future_dataframe(TS_dataset_subm3, periods = 365, freq = "day")
forecast_subm3 <- predict(TS_dataset_subm3, TS_future_subm3)
plot(TS_dataset_subm3, forecast_subm3)
prophet_plot_components(TS_dataset_subm3, forecast_subm3)

