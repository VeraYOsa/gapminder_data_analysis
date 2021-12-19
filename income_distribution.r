

# Income distribution work ------------------------------------------------


# Dolars per day ----------------------------------------------------------


datos %>%
  mutate(group=ifelse(income_per_person_gdppercapita_ppp_inflation_adjusted/366 <=11.52, "Pobres",
                      (ifelse(income_per_person_gdppercapita_ppp_inflation_adjusted/366 >75.54, "Ricos","Medios"))))  %>%
    group_by( region, country, group,year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020)
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = (income_per_person_gdppercapita_ppp_inflation_adjusted)/365, group) %>%
  ggplot(aes(mean_x, fill=group)) +
  scale_x_continuous(trans = "log2", labels = comma) +
  geom_density(alpha = 0.2, bw = 0.4)  + 
  facet_wrap(region ~ .)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Dollars per day")+
  xlab("")+ 
  # theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5)) 
  

datos %>%
  mutate(group=ifelse(income_per_person_gdppercapita_ppp_inflation_adjusted/366 <=11.5, "Pobres",
                      (ifelse(income_per_person_gdppercapita_ppp_inflation_adjusted/365 >75.54, "Ricos","Medios"))))  %>%
  mutate(group = factor(group, levels = c("Ricos","Medios", "Pobres"))) %>%
  group_by( region, country, group,year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020)
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = mean(income_per_person_gdppercapita_ppp_inflation_adjusted)/366, group) %>%
  arrange(desc(mean_x), country) %>%
  ggplot(aes(mean_x, group, fill=group)) + 
  facet_wrap(region~.)+
  scale_x_continuous(trans = "log2")+
  geom_density_ridges(jittered_points = TRUE) +
  scale_fill_brewer(palette = 15) +
  # theme_ridges() +
  theme(legend.position = "none")+
  xlab("") +ylab("")



# Create a world map ------------------------------------------------------
theme_set(    theme_void()  ) #cuidado, cambia la zona de plots

world_map <- map_data("world")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

world_map_label <- world_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

datos %>%  filter(year == 2020) %>%
  mutate( country = ifelse(country == "United States", "USA", country)  )  %>%
  mutate( country = ifelse(country == "Congo, Dem. Rep.", "Democratic Republic of the Congo", country)  )  %>%
  mutate( country = ifelse(country == "United Kingdom", "UK", country)  )  %>%
  ggplot( aes(map_id = country)) +
  geom_map(aes(fill = income_per_person_gdppercapita_ppp_inflation_adjusted/366 ), map = world_map,  color = "white") +
  expand_limits(x = world_map$long, y = world_map$lat)+
  scale_fill_viridis_c(option = "A") +
  # geom_text(aes(label = region), data = world_map_label,  size = 3, hjust = 0.5)+
  # theme_void()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dolares al dia")

# probability distribution ------------------------------------------------
datos %>%
  mutate(group=ifelse(income_per_person_gdppercapita_ppp_inflation_adjusted/366 <=16.1, "Pobres",
                      (ifelse(income_per_person_gdppercapita_ppp_inflation_adjusted/365 >51.8, "Ricos","Medios"))))  %>%
  group_by( region, country, group,year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020)
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = mean(income_per_person_gdppercapita_ppp_inflation_adjusted)/366, group) %>%
  ggplot(aes(x=mean_x)) +
  geom_histogram(bins=30,color="black",aes(y = ..density..)) +
  scale_x_continuous(trans = "log10", labels=comma) +
  geom_density(fill="green", alpha = 0.2) +
  # facet_wrap(region~.)+
  # scale_x_continuous(trans = "log2")+
  scale_fill_brewer(palette = 15)+
  # theme_economist() +
  theme(legend.position = "none")+ 
  geom_vline(xintercept =  51.8, color="red")+ 
  geom_vline(xintercept =  32.7, color="blue")+
  xlab("") +ylab("")

#scatter plot 
datos %>%
  group_by( region, country, year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020) 
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = mean(income_per_person_gdppercapita_ppp_inflation_adjusted)/366, region, population_total) %>%
  ggplot(aes(mean_x, country, color=region, size=population_total)) + 
  geom_point() +
  # scale_y_continuous(trans = "log10", labels = comma)+
  # geom_text_repel(aes(x = mean_x, y = population_total, label = country, size=4)) +
  geom_text(aes(label = country), size = 3, hjust = 0.5)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),axis.text.y=element_text(size=3)) +
  ggtitle("Dollars per day")+
  xlab("") +ylab("")+
  # geom_vline(xintercept =  51.8, color="red")+ 
  # geom_vline(xintercept =  32.7, color="blue")+
  # geom_vline(xintercept =  75.54, color="black")+
  # geom_vline(xintercept =  11.52, color="black")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))



#excel version
all = datos %>%
  group_by( region, country, year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020) 
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = mean(income_per_person_gdppercapita_ppp_inflation_adjusted)/366, region, population_total)
quantile(all$mean_x )
write.csv(all, "test.csv")

rich = datos %>%
  group_by(country, year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) & year %in% c(2020) 
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = median(income_per_person_gdppercapita_ppp_inflation_adjusted)/366,population_total) %>%
  filter(mean_x > 75.54) %>% group_by( year)%>%  summarise(sum(population_total))

poor = datos %>%
  group_by(country, year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020) 
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = median(income_per_person_gdppercapita_ppp_inflation_adjusted)/366,population_total) %>%
  filter(mean_x <11.5) %>%   group_by( year)%>% summarise(sum(population_total))



datos %>%
  group_by(country,year)%>%  
  filter(!is.na(income_per_person_gdppercapita_ppp_inflation_adjusted) 
         & year %in% c(2020) 
         &income_per_person_gdppercapita_ppp_inflation_adjusted>0  ) %>%
  summarise(mean_x = median(income_per_person_gdppercapita_ppp_inflation_adjusted)/366,population_total) %>%
  filter(mean_x <11.5) 
