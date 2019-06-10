library(ggmap)
library(tidyverse)
library(readxl)

#read in the coordinate data
HB <- read_xlsx("HB-data.xlsx")
View(HB)

#get map
base <- c(lon = 106.600562, lat = 21.285358)

#use key for google map: 
register_google(key = "AIzaSyDuDHXaul9vHvcYu-9V8FhcUwkMKTzojVg")

#Get satellite map at zoom level 4 from google
#base_map <- get_map(location = base, maptype = "satellite", zoom = 4)

# Plot lines to connect each location
ggmap(base_map,
      base_layer = ggplot(HB, aes(lon, lat))) +
  geom_line(aes(group = order3), color = 'white') +
  geom_line(aes(group = order), color = 'white') +
  geom_point(data = HB[c(1,10,11),], color = 'red', size = 3)

#find the location(seems only get the address, not the place name I want)
#result <- do.call(rbind,
                  #lapply(1:nrow(HB),
                         #function(i)revgeocode(as.numeric(HB[i,2:1]))))
#HB <- cbind(HB,result)

#Task 1: Find the location name for each coordinate on google map, only focus on the name before first comma
#Task 2: Put the location name into the rows in the 'location' column
#Task 3: What is that on the map? I wish you can always feel it! Assign the answer to 'bw' below 

word <- str_split(HB$location, boundary("word"))
word
bw <- "Dopamine"

#Now try to get the secret message
library(stringr)
message <- str_c(str_to_upper(str_sub(word[[4]], 4, 4)), 
            str_sub(bw, 4, 5),
            str_pad(str_sub(str_split(word[[7]], "")[[3]][5]), 2, side = "right", pad = str_sub(bw, 2, 2)),
            paste0(str_sub(word[[9]], 5, 5), "u", str_sub(word[[1]][3], 2, 2), str_sub(word[[6]][1], 4, 4), str_sub(word[[10]][1], 3, 3)),
            str_to_lower(str_sub(word[[7]][3], 1, 2)),
            str_to_lower(str_pad(str_sub(word[[5]][1], 1, 1), 2, side = "right", pad = str_sub(word[[8]][2], 2, 2))),
            paste0(str_sub(word[[7]][1], 3, 3), str_sub(word[[1]][3], 4, 4), str_sub(word[[2]], 2, 2), "r"),
            paste0(str_sub(word[[11]][2], 3, 4), str_sub(word[[3]], 2, 2), "d", str_sub(word[[10]][3], 2, 3), str_sub(word[[11]][2], 4, 4)), "(^○^)", sep = " ")
message

hint1 <- paste0(str_sub(word[[1]][1], 1, 1), str_sub(word[[1]][2], 1, 1), "2")
hint1

hint2 <- paste0(str_sub(word[[11]][1], 1, 1), str_sub(word[[11]][3], 1, 1))
hint2

#make the URL for me, to take me to the google map page, that will make a URL like this
#https://www.google.com/maps/place/25.125387,90.340085
str_glue('https://www.google.com/maps/place/{HB[location_n, ]$lat},{HB[location_n, ]$lon}')


# view map
library(mapview)
library(sf)
HB_sf <-  st_as_sf(HB, 
                   coords = c( 'lon', 'lat'), 
                   crs = "+proj=longlat +datum=WGS84")
mapview(HB_sf)

# zoom in on each location 
location <- 1 # row number of HB to zoom in on
base_location <- 
  get_map(location = HB[location, c(2, 1)], 
          maptype = "hybrid", 
          zoom = 19)

ggmap(base_location,
      base_layer = ggplot(HB[location,], 
                          aes(lon, lat))) +
  geom_point(data = HB[location,], 
             color = 'red', size = 3)

library(ggplot2)
ggplot ()+
  annotate(geom = "text",
           x = 1, y = 1, 
           size = 30, angle = 0, label = "Ƨ") +
  theme_void()
