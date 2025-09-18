if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)

#read fish data
df_fish <- read_csv(here::here("data/data_finsync_nc.csv"))

sf_site <- df_fish %>%
  distinct(site_id,
           lon,
           lat) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) #covert regular dataframe to georeferenced dataframe. Always remember x-axis is longitude, then y-axis is latitude in coordinates
#latitude is north/south
#longitude is west/east
#4326 is most common geodetic crs

#data on the map
mapview(sf_site,
        legend = FALSE)

#export the converted data to folder
saveRDS(sf_site, 
        file = here::here("data/sf_finsync_nc.rds"))


# conversion from geodetic to projected -----------------------------------
sf_ft_wgs <- sf_site %>%
  slice(c(1, 2))
  
sf_ft_utm <- sf_ft_wgs %>%
  st_transform(crs = 32617) #have to pick the right crs that represents the region you are working in! 

mapview(sf_ft_wgs)
#calculate the distance between the two points that the code above^ provides
st_distance(sf_ft_utm)


# 2.6 exercise  -----------------------------------------------------------
df_quakes <- as_tibble(quakes)
print(df_quakes)
  
#convert
sf_quakes <- df_quakes %>%
  st_as_sf(coords = c("long", "lat"), #x-axis is long!!! y-axis is lat!!!
           crs = 4326)

#view data on the map
mapview(sf_quakes,
        legend = FALSE)

#select first two sites, assign to sf_ft_quakes
sf_ft_quakes <- sf_quakes %>%
  slice(c(1, 2))

#convert the geodetic CRS to projected CRS UTM 60S
sf_ft_quakes_proj <- sf_ft_quakes %>%
  st_transform(crs = 32760) #have to pick the right crs that represents the region you are working in! 

mapview(sf_ft_quakes_proj)

#calculate the distance between the two points
st_distance(sf_ft_quakes_proj)
st_distance(sf_ft_quakes) 


#export the converted data to folder
saveRDS(sf_quakes, 
        file = here::here("data/sf_quakes.rds"))
