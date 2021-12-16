
#corruption index
  datos %>% 
    group_by(year,  region) %>%   
    filter(standard_corr_index_comb>0 & year < 2018 & year >= 1995) %>%
    summarise(mean_of_x= mean(standard_corr_index_comb)) %>%
    ggplot(aes(year,  mean_of_x, color = region)) +  
    geom_line(size=2)+
    scale_y_continuous(trans = "log2")+
    ggtitle("standard_corr_index_comb")+
    labs(x="", y = "") + 
    theme(legend.title=element_blank()) +
    theme(axis.text.y = element_text(size=9) ,
          axis.text.x = element_text(angle=90,vjust=0.5, size=9),
          plot.title = element_text(hjust = 0.5)) 

  

# Create a world map ------------------------------------------------------
theme_set(    theme_void()  ) #cuidado, cambia la zona de plots

world_map <- map_data("world")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="lightgray", colour = "white")

world_map_label <- world_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

datos %>%  filter(year == 2017) %>%
    mutate(   country = ifelse(country == "United States", "USA", country)  )  %>%
  mutate(   country = ifelse(country == "Congo, Dem. Rep.", "Democratic Republic of the Congo", country)  )  %>%
    ggplot( aes(map_id = country)) +
    geom_map(aes(fill = corruption_perception_index_cpi), map = world_map,  color = "white") +
    expand_limits(x = world_map$long, y = world_map$lat)+
    scale_fill_viridis_c(option = "D") +
    # geom_text(aes(label = region), data = world_map_label,  size = 3, hjust = 0.5)+
   # theme_void()+
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Corruption perception index")



# Scatter plot corruption and democracy
datos %>% 
  group_by(region,country, year) %>%  
  summarise(demox_eiu= mean(demox_eiu), corruption_perception_index_cpi=mean(corruption_perception_index_cpi)) %>%
  filter(!is.na(demox_eiu) & demox_eiu>0 &  corruption_perception_index_cpi>0 &year==2017) %>%
  ggplot(aes(demox_eiu, corruption_perception_index_cpi, color=region)) +
  geom_point(size=2) +
  # facet_grid(.~region)+
  ggtitle("Democracy index vs Corruption index")+
  labs(x="", y = "") +
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size=9) ,
        axis.text.x = element_text(angle=90,vjust=0.5, size=9),
        plot.title = element_text(hjust = 0.5)) 
  # theme_economist()+
  geom_smooth(method="loess", level=0.95, linetype="dashed",color="blue", fill="blue")


