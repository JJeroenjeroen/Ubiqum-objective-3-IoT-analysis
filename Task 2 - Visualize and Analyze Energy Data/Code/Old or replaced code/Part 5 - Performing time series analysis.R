#####################################################
# Date:      18-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################



##Setup time series analysis
###################################################


## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(Full_dataset,
                            Weekday == "maandag"  &
                              Hour == 20 &
                              Minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3,
                         frequency=52,
                         start=c(2007,1))

autoplot(tsSM3_070809weekly,
         ts.colour = 'red',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Sub-meter 3")



## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2,
                         frequency=52,
                         start=c(2007,1))

autoplot(tsSM2_070809weekly,
         ts.colour = 'blue',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Sub-meter 2")



## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1,
                         frequency=52,
                         start=c(2007,1))

autoplot(tsSM1_070809weekly,
         ts.colour = 'green',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Sub-meter 1")



## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
########################

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)

## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")



## Apply time series linear regression to the sub-meter 2 ts object and use summary to obtain R2 and RMSE from the model you built
########################

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM2 <- forecast(fitSM2, h=20)

## Plot the forecast for sub-meter 3. 
plot(forecastfitSM2)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")




## Apply time series linear regression to the sub-meter 1 ts object and use summary to obtain R2 and RMSE from the model you built
##################################################################

fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM1 <- forecast(fitSM1, h=20)

## Plot the forecast for sub-meter 3. 
plot(forecastfitSM1)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


#Decompose for seasonal trends
#############################################################

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)



## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)
## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2
summary(components070809SM2weekly)


## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)
## Plot decomposed sub-meter 31
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 1 
summary(components070809SM1weekly)


#Using the holt winters smoothening technique
#####################################################



## Sub meter 3
########################

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")


## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))





## Sub meter 2
########################

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")


## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))




## Sub meter 1
########################

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")


## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))
