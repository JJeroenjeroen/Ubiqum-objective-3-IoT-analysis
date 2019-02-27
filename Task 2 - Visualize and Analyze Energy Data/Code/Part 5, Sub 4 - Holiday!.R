#####################################################
# Date:      26-02-2019                             #
# Author:    Jeroen Meij                            #
# File:      Analysis of Smart Home electronic data #
# Version:   1.0                                    #    
#####################################################


#plot Holiday
###################################################

df_holidays <- df_daily_GAP %>% 
  group_by(holiday_dummy) %>% 
  summarise(Electricity_Use = mean(y))


ggplot(df_holidays, aes(holiday_dummy, Electricity_Use*60, fill = as.factor(holiday_dummy))) +
  
  geom_bar(stat="identity", width=.5, position = "dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_hline(yintercept = 70, size = 1, colour="white") +
  
  geom_curve(aes(x = 0.3, y = 65, xend = 0.7, yend = 50), 
             colour = "#555555",
             position = "dodge",
             size = 0.8, 
             curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")) ) +
  
  geom_label(aes(x = 0.53, y = 62, label = "-30%"), 
             hjust = 0.0, 
             vjust = 0,
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  
  bbc_style() +
  theme(legend.position = "none",
        plot.subtitle=element_text(color="deepskyblue4", size = 20),
        axis.text.x = element_text(size = 17)) +
  
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  
  labs(title="Holiday",
       subtitle = "It would be so nice!") +
  scale_x_continuous(breaks = c(0, 1), 
                        labels = c("Non Holiday", "Holiday")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 70), 
                    labels = c("0", "20", "40", "60", "kWh"))

