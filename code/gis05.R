#GIS 05

# Merge -------------------------------------------------------------------
#opposite of crop function, combines multiple raster layers into one single raster

#two tiles:
spr_nw <- rast("data/spr_prec_ncnw.tif") # Northwest NC
spr_ne <- rast("data/spr_prec_ncne.tif") # Northeast NC
spr_sw <- rast("data/spr_prec_ncsw.tif") # Southwest NC
spr_se <- rast("data/spr_prec_ncse.tif") # Southeast NC

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

#Merge more than two tiles

#gather individual tiles into a list:
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
spr_prec_nc <- rast("data/spr_prec_nc.tif")
spr_tmp_nc <- rast("data/spr_tmp_nc.tif")
