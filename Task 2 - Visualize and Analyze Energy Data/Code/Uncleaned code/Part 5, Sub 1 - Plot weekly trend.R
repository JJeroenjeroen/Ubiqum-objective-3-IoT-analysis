#####################################################
# Date:      22-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################



#plot Weekly trend
###################################################


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
             colour="darkred",
             linetype="dashed") +
  geom_path(aes(x = day,
                y = (values)),
            colour = "deepskyblue",
            size = 1.2,
            group = 1) +
  #add text  
  geom_label(aes(x = "Thursday", y = .13, label = "Last weekend,\nthe air conditioner was\nturned on all day long!"), 
             hjust = 0, 
             vjust = 0.5, 
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  
  geom_label(aes(x = "Wednesday", y = .035, label = "Wednesday, your \nwashing machine caused an\nincrease in energy consumption"), 
             hjust = +0.39, 
             vjust = 0.6,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  geom_label(aes(x = "Monday", y = -.11, label = "On weekdays, you consumed\nthe least amount of energy!"), 
             hjust = 0.1, 
             vjust = 0.5,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  
  #add arrow  
  geom_curve(aes(x = "Saturday", y = .12, xend = "Friday", yend = .14), 
             colour = "#555555", 
             size=0.5, 
             curvature = +0.3,
             arrow = arrow(length = unit(0.03, "npc")) ) +
  
  
  
  geom_hline(yintercept = -.15, 
             size = 1,
             colour="#333333") +
  
  
  bbc_style() +
  
  
  scale_y_continuous(breaks = c(-0.1, 0, 0.1),
                     labels = c("-10%",
                                "Mean
kWh
", "+10%")) +
  labs(title="Your energy use last week",
       subtitle = "An overview of which days most energy was consumed") + 
  theme(plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 15),
        axis.text.x = element_text(angle=25, hjust=1, size = 15),
        axis.text.y = element_text(colour = "navyblue")) 


