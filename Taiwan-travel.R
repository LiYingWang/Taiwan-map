devtools::install_github("dkahle/ggmap")
library(ggmap)
library(tidyverse)
library(readxl)
library(ggbeeswarm)

#read in the coordinate data of TW
tw_location <- read_xlsx("Taiwan-location.xlsx")
#View(tw_location)
#use ggbeeswarm to plot 
place <- ggplot(tw_location, aes(city, date, color = type)) +
  geom_beeswarm(cex=4)
place

library(sf)
library(dplyr)

# how far did we travel each day
tw_location_dists <- 
  tw_location %>%
  st_as_sf(coords = c("lon","lat"),
           remove = FALSE) %>% 
  st_set_crs(4326) %>% 
  st_transform(3828) %>% 
  group_by(date) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = TRUE)
  )

tw_location_dists %>% 
  filter(dist < 100000) %>% 
  ggplot(aes(date,
             dist / 1000,
             group = date)) +
  geom_boxplot() +
  geom_beeswarm(size = 5, alpha = 0.3) +
  ylab("Distance between locations (km)") +
  scale_x_datetime(date_breaks = "1 day") +
  theme_minimal(base_size = 16)

library(ggrepel)
ggplot() +
  geom_path(data = tw_location_dists,
            aes(lon, 
                lat,
                colour = as.factor(date)), 
            size = 3) +
  geom_label_repel(data = tw_location_dists, 
                   aes(lon, 
                       lat, 
                       label = str_glue('{round(dist/1000, 1)} km'),
                       size = 5))

#Get map
Taiwan <- c(lon = 120.903170, lat = 23.844516)
Taipei <- c(lon = 121.500470, lat = 25.061506) 
Tainan <- c(lon = 120.196304, lat = 23.015951)
base <- c(lon = 106.600562, lat = 21.285358)

#Get map at zoom level 12 from google and stamen
#TW_map <- get_map(location = Taiwan, zoom = 8, scale = 1)
#use key for google map: 
base_map <- get_map(location = base, maptype = "satellite", zoom = 4)
TP_map <- get_map(location = Taipei, maptype = "satellite", zoom = 12)
TP_map_wac <- get_map(location = Taipei, maptype = "watercolor", source = "stamen", zoom = 12)
TN_map_toner <- get_map(location = Tainan, maptype = "toner", source = "stamen", zoom = 13)

# polygon exercise 

Taipei <- tw_location %>% 
  filter(city == "Taipei")
ggplot(Taipei, aes(lon, lat)) +
  geom_polygon(aes(fill = 'place-name'))

# test
ggmap(base_map)

# Plot TP google map with path by using base layer call
ggmap(TP_map,
      base_layer = ggplot(tw_location, aes(lon, lat))) +
  geom_path(color = 'blue') +
  geom_point(aes(shape = as.character(date), color = as.character(date)))

# Plot TP google map with path
ggmap(TP_map) +
  geom_path(data = tw_location,
            aes(lon, lat),
            color = 'blue') +
  geom_point(data = tw_location, color = 'red', aes(shape = as.character(date)), size = 1)

# Plot TP google map with path, facetting by date
ggmap(TP_map) +
  geom_path(data = tw_location,
            aes(lon, lat),
            color = 'blue') +
  geom_point(data = tw_location, aes(color = type)) +
  facet_wrap(~date)

# Plot TP satellite map with facetting by date and type, base_layer
ggmap(TP_map,
      base_layer = ggplot(tw_location, aes(lon, lat))) +
  geom_point(aes(color = type)) +
  facet_wrap(~date)

ggmap(TP_map,
      base_layer = ggplot(tw_location, aes(lon, lat))) +
  geom_point(aes(color = type)) +
  facet_wrap(~type)

# Plot TP satellite map by using qmplot()
qmplot(lon, lat, data = tw_location,
       geom = "point", color = type) +
  facet_wrap(~date)

# save the map
ggsave("tp_location.png", w = 8, h = 8, dpi = 600)

# Plot TP google map at zoom level 12 
ggmap(TP_map) +
  geom_point(data = tw_location,
             aes(lon, lat),
             color = 'red', size = 1)

# Plot TP toner map at zoom level 12 
ggmap(TP_map_toner) +
  geom_point(data = tw_location,
             aes(lon, lat, color = type)) 
<<<<<<< HEAD
=======

# Plot TN map at zoom level 13 
ggmap(TN_map_toner) +
  geom_point(data = tw_location,
             aes(lon, lat, color = type))
>>>>>>> 17fe0b541cf7f160b016eb2b5f61d83ceb806d0d
