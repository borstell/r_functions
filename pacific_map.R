library(dplyr)
library(ggplot2)
library(maps)

### Set point for map split 
split_longitude <- -30

### Load a world map (excluding Antarctica; moving Greenland)
pacmap <- fortify(maps::map(fill=TRUE, plot=FALSE, wrap = c(split_longitude, 360+split_longitude))) %>% 
  filter(!region %in% c("Antarctica", "Greenland")) %>% 
  bind_rows(fortify(maps::map(fill=TRUE, plot=FALSE)) %>% 
              filter(region %in% c("Greenland")) %>% 
              mutate(long = long+360) %>% 
              mutate(group = group+99999))

### Load lakes and shift coordinates
lakes <- ggplot2::map_data("lakes") %>%
  mutate(long = if_else(long<split_longitude,long+360,long))

### Plot map
ggplot() + 
  geom_polygon(data=pacmap, aes(x = long, y = lat, group = group), 
               fill = "grey80", 
               color = "grey80",
               linewidth = 0.2) +
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), 
               fill = "white") +
  coord_sf() +
  theme_void()