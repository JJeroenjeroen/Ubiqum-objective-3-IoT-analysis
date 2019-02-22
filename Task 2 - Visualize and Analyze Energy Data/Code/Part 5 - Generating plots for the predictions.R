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
  geom_line(aes(x = ds, y = trend*60),
            colour = "#FAAB18",
            size = 1.2) + 
  geom_hline(yintercept = 50,
             size = 1,
             colour="#333333") +
  geom_hline(yintercept = 100,
             size = .1,
             colour="grey") +
  bbc_style() +
  labs(title="General trend",
       subtitle = "Average kWh From 2007-2012")





#preprocess weekly trend values
weekly_values <- data.frame(values = forecast_GAP$weekly[c(1:7)], 
                            day = c("Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday",
                                    "Sunday"))

weekly_values$day <- as.character(weekly_values$day)
weekly_values$day  <- factor(weekly_values$day, levels=unique(weekly_values$day))



#plot weekly trend
ggplot(weekly_values) +
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333",
             linetype="dashed") +
  geom_path(aes(x = day, y = values),
            colour = "#FAAB18",
            size = 1.2,
            group = 1) + 
  geom_hline(yintercept = -.15, 
             size = 1,
             colour="#333333") +
  bbc_style() +
  labs(title="Weekly Trend",
       subtitle = "Relative electricity usage per day")



#preprocess yearly trend values
year_vector_2007 <- c(seq.Date(from = date('2007-01-01'),
                               to = date('2007-12-31'),
                               by = "day"))
Yearly_values <- data.frame(values = forecast_GAP$yearly[c(1:365)], 
                            day = year_vector_2007)


#plot yearly trend
ggplot(Yearly_values) +
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333",
             linetype="dashed") +
  geom_path(aes(x = day, y = values),
            colour = "#FAAB18",
            size = 1.2,
            group = 1) + 
  geom_hline(yintercept = -.6,
             size = 1,
             colour="#333333") +
  bbc_style() +
  labs(title="Yearly Trend",
       subtitle = "Relative Electricity Usage In A Year")



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
             size = .5) +
  geom_line(aes(x =ds, y = ((trend + yearly + weekly + holidays)*60)),
            colour = "red3",
            size = .1) + 
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






