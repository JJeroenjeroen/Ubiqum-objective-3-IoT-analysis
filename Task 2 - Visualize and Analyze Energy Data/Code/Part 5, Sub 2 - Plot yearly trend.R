#####################################################
# Date:      25-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#plot yearly trend
###################################################



#preprocess yearly trend values
year_vector_2007 <- c(seq.Date(from = date('2007-01-01'),
                               to = date('2007-12-31'),
                               by = "day"))
Yearly_values <- data.frame(values = forecast_GAP$yearly[c(1:365)], 
                            day = year_vector_2007)


Yearly_values$id = 0
Yearly_values %>% filter(values < 0) %>% mutate(id = 1)





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
  
  scale_y_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
                     labels = c("-40%","-20%","Yearly
                                Average
                                kWh ",
                                "+20%", "+40%")) +
  
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank()) +
  
  scale_x_date(date_breaks = "3 months",
               date_labels = c("Fall", "Winter", "Spring",  "Summer")) +
  
  geom_curve(aes(x = as.Date("2007-10-26"), y = -0.4, xend = as.Date("2007-08-26"), yend = -.4), 
             colour = "#555555", 
             size=0.8,
             curvature = -0.3,
             arrow = arrow(length = unit(0.04, "npc")) ) +
  
  geom_curve(aes(x = as.Date("2007-05-05"), y = 0.22, xend = as.Date("2007-02-20"), yend = .30), 
             colour = "#555555", 
             size=0.8,
             curvature = 0.2,
             arrow = arrow(length = unit(0.04, "npc")) ) +
  
  
  geom_label(aes(x = as.Date("2007-05-07"), y = .15, label = "During the winter months, the\nhousehold uses, on average,\nfar more energy compared to\nthe other seasons"), 
             hjust = 0, 
             vjust = 0,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  
  
  
  geom_label(aes(x = as.Date("2007-10-12"), y = -.38, label = "During the summer months\nenergy consumption is\nat its lowest point"), 
             hjust = 0, 
             vjust = 0,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  
  theme(axis.text.x = element_text(hjust = 3)) +
  
  
  labs(title="Yearly Electricity Trend",
       subtitle = "Average electricity usage during the year") + 
  theme(plot.subtitle=element_text(face="italic", color="red3"))


