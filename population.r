
#Fertility vs population

datos %>% 
  group_by(region, country, year) %>%  
  summarise(children_per_woman_total_fertility= mean(children_per_woman_total_fertility), population_total=mean(population_total)) %>%
  filter(!is.na(children_per_woman_total_fertility) & population_total>0 & year%in%c(1945,1960,1990,2000)) %>%
  ggplot(aes(children_per_woman_total_fertility, population_total), fill=region, color=region) +
  geom_point(size=2, shape=21) +
  facet_grid(region~year)+
  scale_y_continuous(trans = "log2") +
  ggtitle("Children per woman")+
  labs(x="", y = "") + 
  # theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5)) 


#Children per woman over time
datos %>% 
  group_by(region, year) %>% filter(year <= 2018 & year >=1900)%>%
  summarise(woman_fertility= mean(children_per_woman_total_fertility)) %>%
  ggplot(aes(year, woman_fertility, fill= region, color=region)) +
  geom_line(size=2) +
  ggtitle("Children per woman")+
  labs(x="", y = "") + 
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size=10) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=10),
        legend.position = "bottom",  legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5)) + theme_wsj()



#Children per woman over time - Eruope only
datos %>% 
  group_by(region, year,country) %>% filter(region =="Europe"& year <= 2018 & year >=1900)%>%
  summarise(woman_fertility= mean(children_per_woman_total_fertility)) %>%
  ggplot(aes(year, woman_fertility, fill= country, color=country)) +
  geom_line(size=2) +
  ggtitle("Children per woman")+
  labs(x="", y = "") + 
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size=10) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=10),
        legend.position = "bottom",  legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5))

#Population over time
datos %>% 
  group_by(region, year)%>% filter(year<=2018) %>%
  summarise(population_total= mean(population_total)) %>%
  ggplot(aes(year, population_total, fill= region, color=region)) +
  geom_line(size=2) +
  ggtitle("Total World population")+  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_economist()+ 
  labs(x="", y = "")+
  theme(legend.position="bottom" ,legend.title=element_blank())



