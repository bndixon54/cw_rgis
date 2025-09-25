#vector data is point data; single combination of latitude and longitude will define where you are. 
#if two points are connected it becomes a line vector
#three lines connected becomes a polygon vector

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview, 
               here)

# read/export vector data -------------------------------------------------
# read a shapefile (e.g., ESRI Shapefile format)

(sf_nc_county <- st_read(dsn = here("data/nc.shp"),  #.shp is the geometric data
                         quiet = TRUE)) # `quiet = TRUE` for cleaner output

#export data as a shapefile
st_write(sf_nc_county,
         dsn = here("data/sf_nc_county.shp"), #location where you want it to save
         append = FALSE) #default is TRUE, if you dont specify this, it will append to a pre-existing file 

#export data as Geopackage 
st_write(sf_nc_county, 
         dsn = here("data/sf_nc_county.gpkg"),
         append = FALSE)

#preferred option:
#export as an rds file (compact and efficient file for RStudio)
saveRDS(sf_nc_county,
        file = here("data/sf_nc_county.rds"))

#to call the vector data from RDS file
sf_nc_county <- readRDS(file = here("data/sf_nc_county.rds"))


# 3.2.3 point data --------------------------------------------------------
#point data as is
sf_site <- readRDS(file = here("data/sf_finsync_nc.rds"))

#map data
mapview(sf_site,
        col.regions = "black", #point fill color
        legend = FALSE) #disable legend

#take the first 10 sites
sf_site_f10 <- sf_site %>% 
          slice(1:10)
#visualize: 
mapview(sf_site_f10,
        col.regions = "black",
        legend = FALSE) 



# 3.2.4 line data ---------------------------------------------------------
sf_str <- readRDS(here("data/sf_stream_gi.rds"))

#map
mapview(sf_str,
        color = "steelblue", #line color
        legend = FALSE) #disable legend

#take first 10 line strings
sf_str_f10 <- sf_str %>% 
  slice(1:10)



# 3.2.5 polygon data ------------------------------------------------------
#map
mapview(sf_nc_county,
        col.regions = "tomato", #polygon's fill color
        legend = FALSE) #disable legend


#only highlight guilford county
sf_nc_gi <- sf_nc_county %>% 
        filter(county == "guilford")
#visualize: 
mapview(sf_nc_gi,
        col.regions = "tomato", 
        legend = FALSE) 



# 3.2.6 mapping vector data -----------------------------------------------
#visualize multiple types of data together

#start by plotting only polygon layers to show county boundaries using ggplot
ggplot() +
  geom_sf(data = sf_nc_county)

#next, add the line layer
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str)  #this part

#then add point later
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str) +
  geom_sf(data = sf_site) #this part

#improved verison of this ^ map:
ggplot() +
  geom_sf(data = sf_nc_gi) + #zooms in to only show guilford county 
  geom_sf(data = sf_str)



# 3.2.7 exercise ----------------------------------------------------------
#read
sf_str_as <- readRDS(here("data/sf_stream_as.rds"))

#check
print(sf_str_as)
print(sf_nc_county)
#not the same CRS!

#Create a map displaying both North Carolina county boundaries from sf_nc_county and Ashe County stream lines from sf_str_as
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str_as) 


#subset county layer to ashe county and remap
sf_nc_as <- sf_nc_county %>% 
  filter(county == "ashe")

ggplot() +
  geom_sf(data = sf_nc_as) + 
  geom_sf(data = sf_str_as)





