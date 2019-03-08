#####################################################
# Date:      22-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################



#plot Weekly trend
###################################################


#Generate a dataframe containing the daily averaged values in a week using the df created by prophet
weekly_values <- data.frame(values = forecast_GAP$weekly[c(1:7)], 
                            day = c("Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday",
                                    "Sunday"))

#this code will make the days on the x-axis be displayed in the correct order 
weekly_values$day <- as.character(weekly_values$day)
weekly_values$day  <- factor(weekly_values$day, levels=unique(weekly_values$day))



#Plot the weekly trend
ggplot(weekly_values) +
  
  #add line for the weekly mean
  geom_hline(yintercept = 0,
             size = 1,
             colour="darkred",
             linetype="dashed") +
  
  #add the actual weekly trend
  geom_path(aes(x = day,
                y = (values)),
            colour = "deepskyblue",
            size = 1.2,
            group = 1) +
  
  #add texts where necessary  
  geom_label(aes(x = "Thursday", y = .13,
                 label = "During the weekends,\nthe family is home, which\ncauses more energy usage!"), 
             hjust = 0, 
             vjust = 0.5, 
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  
  geom_label(aes(x = "Wednesday", y = .035,
                 label = "On Wednesday, the\nwashing machine often causes\nan increase in energy consumption"), 
             hjust = +0.39, 
             vjust = 0.6,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  geom_label(aes(x = "Monday", y = -.11,
                 label = "On weekdays, the least\namount of energy is consumed!"), 
             hjust = 0.1, 
             vjust = 0.5,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  
  #add arrowfor the text  
  geom_curve(aes(x = "Saturday", y = .12,
                 xend = "Friday", yend = .14), 
             colour = "#555555", 
             size=0.5, 
             curvature = +0.3,
             arrow = arrow(length = unit(0.03, "npc")) ) +
  
  
  #add a line to function as x-axis
  geom_hline(yintercept = -.15, 
             size = 1,
             colour="#333333") +
  
  #style graph
  bbc_style() +
  
  #scale the y-axis
  scale_y_continuous(breaks = c(-0.1, 0, 0.1),
                     labels = c("-10%",
                                "Mean
kWh
", "+10%")) +
  
  #add titles
  labs(title="Weekly energy use",
       subtitle = "An overview of during which days most energy is consumed") + 
  
  #add specifics for styling
  theme(plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 15),
        axis.text.x = element_text(angle=25, hjust=1, size = 15),
        axis.text.y = element_text(colour = "navyblue")) 


