#GIS 05

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               terra,
               tidyterra,
               stars,
               mapview, 
               here)

# Crop --------------------------------------------------------------------
spr_prec <- rast(here("data/spr_prec_us.tif"))

#inspect the spatial extent 
ext(spr_prec)

#Crop function, direct entry
spr_prec_crop <- crop(x = spr_prec,
                      y = c(-80, -75, 34, 37)) #order xmin, xmax, ymin, ymax

ext(spr_prec_crop)

#load county vector 
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

ggplot() +
  geom_spatraster(data = spr_prec_crop) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25) #alpha = 0.25 makes the polygon layer transparent

#vector layer and mask layer
spr_prec_nc <- crop(x = spr_prec,
     y = sf_nc_county)

ggplot() +
  geom_spatraster(data = spr_prec_nc) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

# Merge -------------------------------------------------------------------
#opposite of crop function, combines multiple raster layers into one single raster

#two tiles:
spr_nw <- rast(here("data/spr_prec_ncnw.tif")) #Northwest NC
spr_ne <- rast(here("data/spr_prec_ncne.tif")) #Northeast NC
spr_sw <- rast(here("data/spr_prec_ncsw.tif")) #Southwest NC
spr_se <- rast(here("data/spr_prec_ncse.tif")) #Southeast NC

#visual check
ggplot() +
  geom_spatraster(data = spr_nw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#merge tiles into single raster
spr_n <- merge(spr_nw, spr_ne)

#The spr_n layer is a combination of the northern tiles and should now cover the northern half of the state.
ggplot() +
  geom_spatraster(data = spr_n) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#compare extent 
ext(spr_nw)
ext(spr_n)


#Merge more than two tiles
#gather individual raster layers into a list:
list_spr <- list(spr_nw,
                 spr_ne,
                 spr_sw,
                 spr_se)

#convert:
spr_col <- sprc(list_spr)

#merge:
spr_merge <- merge(spr_col)

#all of north carolina:
ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)


# Stack -------------------------------------------------------------------
#load the layers into R
spr_prec_nc <- rast(here("data/spr_prec_nc.tif"))
spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))

#combine layers
spr_pt_nc <- c(spr_prec_nc,
               spr_tmp_nc)

print(spr_pt_nc)

#access each layer separately 
spr_pt_nc$precipitation


# reprojection ------------------------------------------------------------
print(spr_prec_nc)

#convert/reproject for raster
(spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617", #the CRS you want to project onto
                             method = "bilinear")) 

#continuous data - bilinear interpolation
#discrete data - nearest neighbor


# Exercise ----------------------------------------------------------------
#merge raster files
nw_temp <- rast(here("data/spr_tmp_ncnw.tif"))
ne_temp <- rast(here("data/spr_tmp_ncne.tif"))
sw_temp <- rast(here("data/spr_tmp_ncsw.tif"))
se_temp <- rast(here("data/spr_tmp_ncse.tif"))

list_spr <- list(nw_temp,
                 ne_temp,
                 sw_temp,
                 se_temp)

#convert:
spr_col <- sprc(list_spr)

#merge:
spr_merge <- merge(spr_col)

#all of north carolina:
ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#crop raster
sf_camden <- sf_nc_county %>%
  filter(county == "camden")

ext(sf_camden)
  
spr_tmp_camden <- crop(x = spr_merge,
                       y = sf_camden)
  
#load county data
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

ggplot() +
  geom_spatraster(data = spr_tmp_camden) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#reproject

print(spr_tmp_camden)

#convert/reproject for raster
spr_tmp_camden_proj <- project(x = spr_tmp_camden,
                             y = "EPSG:32618", #the CRS you want to project onto
                             method = "bilinear")
print(spr_tmp_camden_proj)
