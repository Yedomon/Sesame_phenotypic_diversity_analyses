## Drawing map of core and whole collection
## By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeBoys27
## 18.03.2021
## National Institute of Agricultural Sciences | Department of Genomics | RDA | Republic of Korea

### Libraries

library(sf)
library(ggspatial)
library(ggplot2)
library(ggrepel)
library(patchwork)


#--Clean the R environment workspace
rm(list = ls())




### Set the working directory

setwd("C:/Users/ANGE/Documents/R MAP")


##########################################---Core collection map------###################################


### Import my shapefile

mySHP= sf::st_read("C:/Users/ANGE/Documents/R MAP/World_Countries.shp")



### Import the data

mycore = read.csv("core.csv", header=TRUE, sep=";", dec = ",")


###  Plot



core_map = ggplot() +
  
  geom_sf(data = mySHP, fill="#e6e3ea", colour="white") +
  
  coord_sf(xlim = c(-180, 180), ylim = c(-55, 83.6236), expand = FALSE) +
  
  geom_point(data = mycore, aes(x=Longitude, 
                                y=Latitude, 
                                size= Number, 
                                color= Region)) +
  
  scale_color_brewer(palette = "Paired", "Region of origin") +
  
  scale_size_continuous(name = "Number of the selected accessions per country", breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
  
  geom_text_repel(data = mycore,
                  aes(x = Longitude, y= Latitude,
                      label = paste(Country, " (", Number, ")", sep = "")),
                  fontface = "bold",
                  color = "black",
                  box.padding = 0.35,
                  point.padding = 0.5,
                  segment.color = "grey10") +
  
  annotation_scale(location = "br",
                   width_hint = 0.5
  ) +
  
  annotation_north_arrow(location = "br",
                         which_north = "TRUE",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  
  theme_bw() +
  
  labs(title="(B) Core collection")


core_map

##########################################---whole collection map------###################################


mywhole = read.csv("whole.csv", header=TRUE, sep=";", dec = ",")

whole_map = ggplot() +
  
  geom_sf(data = mySHP, fill="#e6e3ea", colour="white") +
  
  coord_sf(xlim = c(-180, 180), ylim = c(-55, 83.6236), expand = FALSE) +
  
  geom_point(data = mywhole, aes(x=Longitude, 
                                 y=Latitude, 
                                 size= Number, 
                                 color= Region)) +
  
  scale_color_brewer(palette = "Paired", "Region of origin") +
  
  scale_size_continuous(name = "Number of the selected accessions per country", breaks = c(10, 30, 50, 70, 90, 110)) +
  
  geom_text_repel(data = mywhole,
                  aes(x = Longitude, y= Latitude,
                      label = paste(Country, "^(", Number, ")", sep = "")),
                  fontface = "bold",
                  color = "black",
                  box.padding = 0.35,
                  point.padding = 0.5,
                  segment.color = "grey10"
  ) +
  
  annotation_scale(location = "br",
                   width_hint = 0.5) +
  
  annotation_north_arrow(location = "br",
                         which_north = "TRUE",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  
  theme_bw() +
  
  labs(title="(A) Worldwide panel")


whole_map

### Rendering both maps

(whole_map) / (core_map)
