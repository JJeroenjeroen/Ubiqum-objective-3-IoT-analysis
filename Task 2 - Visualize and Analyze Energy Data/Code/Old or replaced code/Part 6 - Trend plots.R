#####################################################
# Date:      21-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################



#plot time series trends etc
###################################################


#plot general trend
ggplot(forecast_GAP) +
  geom_line(aes(x = ds, y = trend),
            colour = "#FAAB18",
            size = 1.2) + 
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333") +
  geom_hline(yintercept = 2,
             size = .1,
             colour="grey") +
  bbc_style() +
  labs(title="General trend",
       subtitle = "Average kWh From 2007-2012")








#Preprocess for all trends combined
Add_y_to_plot <-  df_daily_GAP$y
list_334_NA <- c(1:365) * NA
Add_y_to_plot <- c(Add_y_to_plot, list_334_NA)
forecast_GAP$y <- Add_y_to_plot


#plot all trends
ggplot(forecast_GAP) +
  geom_point(aes(x = ds,
                 y = y*60),
             colour = "black",
             size = 1.1) +
  geom_line(aes(x =ds, y = ((trend)*60)),
            colour = "red3",
            size = .1) + 
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333") +
  bbc_style() +
  labs(title="Prophet Forecast",
       subtitle = "Average kWh Per Day From 2007-2012")


forecast_GAP$ds <- date(forecast_GAP$ds)

#plot all trends
ggplot(forecast_GAP) +
  geom_point(aes(x = ds,
                  y = y*60),
              colour = "darkred",
              size = 1,
             alpha = 0.5) +
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333") +
  bbc_style() +
  labs(title="Prophet Forecast",
       subtitle = "Average kWh Per Day From 2007-2012")









#plot all trends with confidence interval
ggplot(forecast_GAP) +
  geom_line(aes(x = ds,
                y = (yhat_upper)*60),
            colour = "grey",
            size = 0.1,
            alpha = 0.5) + 
  geom_line(aes(x = ds,
                y = (yhat_lower)*60),
            colour = "grey",
            size = 1,
            alpha = 0.5) + 
  geom_ribbon(aes(x = ds,
                  ymin = yhat_lower*60,
                  ymax = yhat_upper*60),
              fill="grey",
              alpha="0.3") +
  geom_point(aes(x = ds,
                 y = y*60),
             colour = "black",
             size = 0.1) +
  geom_line(aes(x = ds,
                y = (yhat)*60),
            colour = "red3",
            size = 0.1) + 
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333") +
  bbc_style() +
  labs(title="Prophet Forecast",
       subtitle = "Average kWh From 2007-2012")






