
# C02 emissions work ------------------------------------------------------
datos %>% 
  group_by(group, region, year) %>%   filter(group !="Others"& year <= 2018& year >= 1900) %>%
  summarise(mean_of_x= mean(yearly_co2_emissions_1000_tonnes)) %>%
  ggplot(aes(year,  mean_of_x, fill = group, color = group)) +  
  geom_line(size=2)+
ggtitle("yearly_co2_emissions_1000_tonnes") +  
  labs(x="", y = "") + 
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = cols)

#Emisiones totales - log

  datos %>% 
  group_by(group, region, year) %>%   filter(group !="Others"& year <= 2018& year >= 1910) %>%
  summarise(mean_of_x= mean(yearly_co2_emissions_1000_tonnes)) %>%
  ggplot(aes(year,  mean_of_x, fill = group, color = group)) +  
  geom_line(size=2)+
scale_y_continuous(trans = "log2")+
  ggtitle("yearly_co2_emissions_1000_tonnes")+
  labs(x="", y = "") + 
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = cols)

#Emisiones per capita
  datos %>% 
    group_by(group, region, year) %>%   filter(group !="Others"& year <= 2018& year >= 1910) %>%
    summarise(mean_of_x= mean(co2_emissions_tonnes_per_person)) %>%
    ggplot(aes(year,  mean_of_x, fill = group, color = group)) +  
    geom_line(size=2)+
    ggtitle("co2_emissions_tonnes_per_person")+
    scale_y_continuous(trans = "log2")+
    labs(x="", y = "") + 
    theme(legend.title=element_blank()) +
    theme(axis.text.y = element_text(size=9) ,
          axis.text.x = element_text(angle=90,vjust=0.5, size=9),
          plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = cols)

  
