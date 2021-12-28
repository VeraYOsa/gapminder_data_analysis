library(scales)
library(ggrepel)
library(readr)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(maps)
library(viridis)
library(tidyverse)
library(reshape2)
# Read the file ----------------------------------------------------------

setwd("C:/Users/inesr/OneDrive/Documentos/Gapminder_data_project")
datos <- read_csv("output/gapminder_data.csv", 
                           col_types = cols(X1 = col_skip()))

colnames(datos)

# vaccine work ------------------------------------------------------------

vac <- read_csv("vccin_effect_dag.csv")
vac$`2015` <- NULL
vac$`2016` <- NULL
vac$`2017` <- NULL
vac <- tidyr::pivot_longer(vac, -country, names_to = "year", values_to = "vac")
datos <- merge(datos, vac, by=c("country", "year"))

datos %>%  filter(year==2010 & mcv2_vacc>0 &measles_deaths_in_children_1_59_months_total_deaths>0& country!="Afghanistan") %>%
  group_by(region,country, year) %>%
  summarise(population_total=mean(population_total),
            measles_deaths_in_children_1_59_months_total_deaths=mean(measles_deaths_in_children_1_59_months_total_deaths),
            mcv2_vacc=mean(mcv2_vacc)) %>% 
  ggplot(aes(mcv2_vacc,  measles_deaths_in_children_1_59_months_total_deaths, color= region, size=population_total))+ 
  geom_point() + 
  geom_text(aes(label= country))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))+
  scale_y_continuous(trans = "log10")



ylim.prim <- c(58924, 459135)   # in this example, precipitation
ylim.sec <- c(min(datos %>% filter(year >2000 & year <2017 ) %>% group_by(year)%>% summarise(mcv2_vacc=mean(mcv2_vacc))), 62)    # in this example, temperature
# The following makes the necessary calculations based on these limits, and makes the plot itself:
min(datos %>% filter(year >2000 & year <2017 )%>% summarise(mcv2_vacc=mean(mcv2_vacc)))

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

datos %>% filter(year >2000 & year <2017  ) %>% group_by(year) %>% 
  summarise(mcv2_vacc=mean(mcv2_vacc),
            measles_deaths_in_children_1_59_months_total_deaths=sum(measles_deaths_in_children_1_59_months_total_deaths)) %>%
  arrange((mcv2_vacc)) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = year, y = measles_deaths_in_children_1_59_months_total_deaths), stat = "identity", fill = "grey") +
  geom_line(mapping = aes(x = year, y = a + mcv2_vacc*b), size = 2, color = "red") +
  scale_x_continuous( breaks = scales::pretty_breaks(n = 20))+
  scale_y_continuous(name = "Muertes por sarampión", labels = comma,
                     sec.axis = sec_axis(~ (. - a)/b, name = "Inmunizados con dos vacunas del sarampión",
                                         labels = function(b) { paste0(round(b , 0), "%")})) +
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Muertes por sarampión (barras grises) vs % inmunizados con las dos vacunas (línea roja)")+
  labs(x="", y = "")  

ylim.prim <- c(26, 56)   # in this example, precipitation
ylim.sec <- c(4, 60)    # in this example, temperature
# The following makes the necessary calculations based on these limits, and makes the plot itself:

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]
datos %>% filter(year >1998 & year <2020 ) %>% group_by(year) %>% 
  summarise(six_vacc_rate=mean(six_vacc_rate),
            child_mortality_0_5_year_olds_dying_per_1000_born=mean(child_mortality_0_5_year_olds_dying_per_1000_born)) %>%
  arrange((six_vacc_rate)) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = year, y = child_mortality_0_5_year_olds_dying_per_1000_born), stat = "identity", fill = "grey") +
  geom_line(mapping = aes(x = year, y =six_vacc_rate), size = 2, color = "red") +
  scale_x_continuous( breaks = scales::pretty_breaks(n = 19))+
  scale_y_continuous(name = "% mortalidad niños 0 a 5 años por cada 1000", 
                     sec.axis = sec_axis(~ . * 1, name = "% niños de 1 año con las 6 vacunas",breaks = scales::pretty_breaks(n = 10)),
                     breaks = scales::pretty_breaks(n = 15)) +
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9, angle=60)) + 
  ggtitle("% mortalidad niños 0 a 5 años por cada 1000 vs % niños de 1 año con las 6 vacunas")+
  labs(x="", y = "") 
