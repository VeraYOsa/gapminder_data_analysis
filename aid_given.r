
#Aid given by county
datos %>% 
  group_by(country,region, year)%>%  
  summarise(mean_x=mean(aid_given_percent_of_gni),mean_y=mean( ms_mil_xpnd_gd_zs)) %>%
  filter(mean_x>0 & year ==2007) %>%
  ggplot(aes( mean_x, mean_y, fill = country, color = country)) +  
  geom_point(size=3)+ 
  geom_smooth(method = "auto", size = 1.5)+
  ggtitle("Aid given as %GNI vs Military expenditure") +  
  labs(x="", y = "") + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text_repel(aes(x = mean_x, y = mean_y, label = country)) 

#CORRELATION WITH BURGER KING FRANCHISES
burger_king_franchises <- read_excel("burger_king_franchises.xlsx")
burger_king_franchises$country <- burger_king_franchises$`Country/territory`
burger_king_franchises$no_bk <- burger_king_franchises$`Number of locations`
burger_king_franchises$`Country/territory`<- NULL
burger_king_franchises$`Number of locations` <- NULL
filtro2= left_join(filtro, burger_king_franchises)

filtro= datos %>% 
  group_by(country,region, year)%>%  
  summarise(mean_x=mean(aid_given_percent_of_gni),mean_y=mean( ms_mil_xpnd_gd_zs)) %>%
  filter(mean_x>0 & year ==2007)


filtro2 %>% 
  ggplot(aes( mean_x,no_bk, fill = country, color = country)) +  
  geom_point(size=3)+ 
  geom_smooth(method = "auto", size = 1.5)+ 
  scale_y_continuous(trans = "log2")+
  ggtitle("La métrica en el eje vertical es secreta") +  
  labs(x="", y = "") + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text_repel(aes(x =mean_x , y = no_bk, label = country)) 


# smooth density plots - variable counts on y-axis
datos %>%
  group_by(country,region, year)%>%  
  summarise(mean_x=mean(aid_given_percent_of_gni),
            mean_y=mean( mean(ms_mil_xpnd_gd_zs,hdi_human_development_index))) %>%
  filter(mean_x>0 & year%in% c(1996,2008) ) %>%
  ggplot(aes(mean_x,fill = region)) +
  scale_x_continuous(trans = "log2") + 
  geom_density(alpha = 0.2, bw = 0.7, position = "stack") + 
  facet_grid(.~year)+
  ggtitle("Aid given as %GNI") +  
  labs(x="", y = "") + 
  # theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5))

# add group as a factor, grouping regions / countries
datos <- datos %>%
  mutate(group = case_when(
    .$country %in% c("Denmark", "Luxembourg", "Norway", "Sweden") ~ "Nordic",
    .$country =="United States" ~ "United States",
    TRUE ~ "Others"))


# probability distribution ------------------------------------------------
datos %>%
  group_by(country,region,group, year)%>%  
  summarise(mean_x=mean(aid_given_percent_of_gni),
            mean_y=mean( mean(ms_mil_xpnd_gd_zs,hdi_human_development_index))) %>%
  filter(mean_x>0 & year==2008) %>%
  ggplot(aes(x=mean_x)) +
  # geom_histogram(aes(y = ..density..)) +
  geom_density(fill="blue", alpha = 0.2) +
  theme_bw() +
  xlab('') +
  ylab('') 

datos %>%
  group_by(country,region,group, year)%>%  
  summarise(mean_x=mean(hdi_human_development_index)) %>%
  filter(mean_x>0 & year==2008   ) %>%
  ggplot(aes(x=mean_x)) +
  stat_density(kernel="biweight")
