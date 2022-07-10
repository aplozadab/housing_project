
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

hvi <- read_csv(file = 'assets\\Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv', col_types =c('d'))
zori <- read_csv(file = 'assets\\Metro_ZORI_AllHomesPlusMultifamily_Smoothed.csv', col_types =c('d'))
merged <- full_join(hvi, zori, by='RegionID')
merged <- rename(merged, rent = '2022-05')
merged <- rename(merged, home_value ='2022-05-31')
merged$rtv <- as.numeric(merged$rent) / as.numeric(merged$home_value)


merged %>% filter(home_value < 500000) %>% ggplot(., aes(x = home_value, y = rent, label=RegionName.x, color=rtv)) + geom_point() + geom_text(hjust=0, vjust=0)


merged %>% dplyr::filter(home_value < 500000) %>% drop_na() %>% dplyr::select(RegionName.x, home_value, rent, rtv) %>% arrange(-rtv)

s_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
urb_anim = tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)