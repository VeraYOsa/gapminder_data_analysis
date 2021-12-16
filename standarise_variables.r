library(readr)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(maps)
library(viridis)
# Read the file ----------------------------------------------------------

setwd("C:/Users/inesr/OneDrive/Documentos/Gapminder_data_project")
datos <- read_csv("output/gapminder_data.csv", 
                           col_types = cols(X1 = col_skip()))
colnames(datos)


# variables set up (for filtering / grouping) -----------------------------
years = c(1950,1980,2000,2018)
west = c("North America", "Europe", "Oceania")

# add group as a factor, grouping regions / countries
datos <- datos %>%
  mutate(group = case_when(
    .$country =="China" ~ "China",
    .$country =="India" ~ "India",
    .$country =="United States" ~ "United States",
    .$region =="Europe" ~"Europe",
    TRUE ~ "Others"))

# reorder factor levels
datos <- datos %>%
  mutate(group = factor(group, levels = c("China","United States", "India", "Europe", "Others")))

# cols <- c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA", "#9632B8")

# Standardise corruption index ---------------------------------------------

datos["standard_corr_index_pre2012"]=   scale(datos$corruption_perception_index_cpi_pre2012)
datos["standard_corr_index_pre2012"]=   ifelse(datos$standard_corr_index_pre2012 <=0, 0, datos$standard_corr_index_pre2012) 
datos["standard_corr_index"]=   scale(datos$corruption_perception_index_cpi)
datos["standard_corr_index"]=   ifelse(datos$standard_corr_index <=0, 0, datos$standard_corr_index) 
datos["standard_corr_index_comb"] =   datos$standard_corr_index + datos$standard_corr_index_pre2012


datos %>% 
  group_by(region, year) %>% filter(standard_corr_index_comb>0) %>%
  summarise(standard_corr_index_comb=mean(standard_corr_index_comb)) +
  ggplot(aes(year, standard_corr_index_comb, fill=region)) +
  geom_line()


# Summarise and sort ascending / descending -------------------------------
#High level summary
datos %>% group_by(country)%>% summarise(corruption_perception_index_cpi=mean(corruption_perception_index_cpi))

#Descending
datos %>% group_by(country, year)%>% summarise(corruption_perception_index_cpi=mean(corruption_perception_index_cpi)) %>%
  filter(corruption_perception_index_cpi>0, year ==1995) %>%
  arrange(desc(corruption_perception_index_cpi))

#Ascending
datos %>% group_by(country, year)%>% summarise(corruption_perception_index_cpi=mean(corruption_perception_index_cpi)) %>%
  filter(corruption_perception_index_cpi>0, year ==1995) %>%
  arrange((corruption_perception_index_cpi))
  

