#October 9 2025: Raster Data I

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)

(spr_ex <- rast("data/spr_example.tif"))


#export raster object
writeRaster(spr_ex,
            filename = "data/spr_elev.tif",
            overwrite = TRUE)

#visualize raster file
ggplot() +
  geom_spatraster(data = spr_ex)

#convert object class to use mapview function
star_ex <- st_as_stars(spr_ex)
class(spr_ex)
class(star_ex)

mapview(star_ex)

# Raster data type --------------------------------------------------------

#Continuous data

#Continuous raster data represent variables that change smoothly across space and can take on a wide range of numeric values. These are often environmental variables such as elevation, temperature, or precipitation.

v_elev <- values(spr_ex)
head(v_elev, 10)

na.omit(v_elev) %>% 
  mean()

#terra::extract() function allows you to obtain values from specific locations.
extract(spr_ex, y = cbind(6.0000, 50.0000)) #longitude and latitude

#to extract at multiple locations, supply dataframe for second argument
(df_point <- tibble(lon = c(6, 5.9), 
                    lat = c(50, 49.96)))

extract(spr_ex, 
        y = df_point)

#Discrete data

#Discrete raster data (also called categorical raster data) represent classes or categories with distinct boundaries. Each cell stores an integer code that corresponds to a specific class, such as land cover type (e.g., forest, water, urban) or vegetation zone.

#load forest raster
spr_for <- rast("data/spr_forest_nc.tif")

#visualize
ggplot() +
  geom_spatraster(data = spr_for)

unique(spr_for)

#summary info
v_binary <- values(spr_for)
mean(v_binary)

#Types of discrete data - code values with multiple categories
#often used for land cover classification
spr_land <- rast("data/spr_land_reclass.tif")

#examine code values
unique(spr_land) #you have to consult the code table to interperet these values

#sullivan building landuse type
extract(spr_land, cbind(-79.8063, 36.0701)) #longitude and latitude


#Reclass 
#convert these codes into a 0/1 binary representation to compute summary statistics
#write a conversion matrix
#left, original value
#right, value after conversion
(cm <- cbind(c(0, 1001, 1010, 1100),
             c(0, 1, 0, 0)))

#In this cm matrix, the code 1001 will be converted to 1, while all other values will be converted to 0. Plug this matrix into terra::classify() function:
spr_bin <- classify(spr_land,
                    rcl = cm)

v_bin <- values(spr_bin)
mean(v_bin)

#Exercise 

spr_prec_ncne <- rast("data/spr_prec_ncne.tif")
#Number of rows and columns: 162 rows, 532 columns
#Resolution: 0.008333333
#Spatial extent:  -79.89181, -75.45847, 35.24153, 36.59153 
#Coordinate Reference System: lon/lat WGS 84 (EPSG:4326) 
#Minimum and maximum precipitation values: max: 1501.5, min: 1063.1

#edited- dont know how some of my values were so off the first time!

#visualize raster file
ggplot() +
  geom_spatraster(data = spr_prec_ncne)

#extract values
sf_site <- readRDS("data/sf_finsync_nc.rds")
df_xy <- st_coordinates(sf_site)
df_land <- extract(spr_land, df_xy)

#identify landuse types at the sampling sites
table(df_land)

#most common landuse type: 1001 (forest)

#Reclassify
cu <- cbind(c(0, 1001, 1010, 1100),
            c(0, 0, 0, 1))

spr_urban <- classify(spr_land,
                      rcl = cu)

values(spr_urban) %>% 
  mean()
