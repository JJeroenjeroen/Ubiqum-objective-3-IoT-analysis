#####################################################
# Date:      25-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#plot yearly trend
###################################################



#Make a dataframe using 1 year to provide the yearly trend values
year_vector_2007 <- c(seq.Date(from = date('2007-01-01'),
                               to = date('2007-12-31'),
                               by = "day"))

Yearly_values <- data.frame(values = forecast_GAP$yearly[c(1:365)], 
                            day = year_vector_2007)



#plot yearly trend
ggplot(Yearly_values) +
  
#add a line for the average kwh used  
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333",
             linetype="dashed") +
  
#add the yearly trend plot  
  geom_path(aes(x = day, y = values),
            colour = "deepskyblue",
            size = 1.2,
            group = 1) + 
  
#add the x-axis  
  geom_hline(yintercept = -.6,
             size = 1,
             colour="#333333") +
  
#add style  
  bbc_style() +
  
#add the y-axis  
  scale_y_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
                     labels = c("-40%","-20%",
                                "Average
 kWh ",
                                "+20%", "+40%")) +
  

#scale the x-axis  
  scale_x_date(date_breaks = "3 months",
               date_labels = c("Fall", "Winter", "Spring",  "Summer")) +
  

#add arrows to point at maxima/minima  
  geom_curve(aes(x = as.Date("2007-10-26"), y = -0.4,
                 xend = as.Date("2007-08-26"), yend = -.4), 
             colour = "#555555", 
             size=0.8,
             curvature = -0.3,
             arrow = arrow(length = unit(0.04, "npc")) ) +
  
  geom_curve(aes(x = as.Date("2007-05-05"), y = 0.22,
                 xend = as.Date("2007-02-20"), yend = .30), 
             colour = "#555555", 
             size=0.8,
             curvature = 0.2,
             arrow = arrow(length = unit(0.04, "npc")) ) +
  
  
#add labels to tell about the maxima/minima  
  geom_label(aes(x = as.Date("2007-05-07"), y = .15,
                 label = "During winter months, you used far\nmore energy than during the other seasons!"), 
             hjust = 0, 
             vjust = 0,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +
  
  
  
  geom_label(aes(x = as.Date("2007-09-28"), y = -.38,
                 label = "During the summer months\nyour energy consumption\nwas at its lowest point"), 
             hjust = 0, 
             vjust = 0,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4) +

#add titles to the plot  
  
  labs(title="Yearly energy consumption",
       subtitle = "An overview of energy consumption over the 4 seasons") + 
  
  
#add some extra elements like grid and style of axislabels  
  theme(plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 15),
        axis.text.x = element_text(hjust = 1, angle = 25, size = 15),
        axis.text.y = element_text(colour = "navyblue", size = 15),
        panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())
