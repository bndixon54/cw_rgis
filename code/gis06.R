#GIS 06

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               terra,
               tidyterra,
               exactextractr,
               here)

#finsync survey site
sf_site <- readRDS(here("data/sf_finsync_nc.rds"))

#county polygons
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

#precipitation raster
spr_prec_nc <- rast(here("data/spr_prec_nc.tif"))


# pointwise extraction ----------------------------------------------------
#overlay survey sites
ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_site) +
  scale_fill_viridis_c() + 
  theme_bw()


(sf_site_prec <- extract(x = spr_prec_nc,
                         y = sf_site,
                         bind = TRUE) %>% 
    st_as_sf())

#map
ggplot() +
  geom_sf(data = sf_nc_county,
          fill = "grey") + 
  geom_sf(data = sf_site_prec, 
          aes(color = precipitation)) +
  scale_color_viridis_c() +         
  theme_classic()  


# Zonal statistics --------------------------------------------------------
sf_nc_county_proj <- st_transform(sf_nc_county,
                                  crs = 32617)

spr_prec_nc_proj <- terra::project(x = spr_prec_nc, 
                                   y = "EPSG:32617",
                                   method = "bilinear") 

#summary statistics of each county, "mean"
(df_prec_county <- exact_extract(x = spr_prec_nc_proj, #raster layer
                                 y = sf_nc_county_proj, #county layer
                                 fun = "mean", #specify stats u want to calculate
                                 append_cols = TRUE) %>% 
    as_tibble() %>% 
    rename(precipitation = mean)) 

#summary statistics for "standard deviation"
(df_prec_county_sd <- exact_extract(x = spr_prec_nc_proj, #raster layer
                                 y = sf_nc_county_proj, #county layer
                                 fun = "stdev", #can look up what other functions work here
                                 append_cols = TRUE) %>% 
    as_tibble())


#link county name with vector layer
sf_nc_county_prec <- left_join(sf_nc_county,
                               df_prec_county,
                               by = "county") 

#visualize output
ggplot() +
  geom_sf(data = sf_nc_county_prec,
          aes(fill = precipitation))

#alternative approach in textbook might be more useful for large scale datasets (multiple continents)


# Buffer-based analysis ---------------------------------------------------
#considering environmental conditions in a larger radius. Useful for when looking into animals species that can move around (birds)

#transform CRS
sf_site_proj <- sf_site %>%
  st_transform(crs = 32617)

#create buffers around the point
sf_site_buff_proj <- sf_site_proj %>%
  st_buffer(dist = 10000) #meters in a projected CRS

#create a map with one layer on top of another
ggplot() +
  geom_sf(data = sf_site_buff_proj) +
  geom_sf(data = sf_site_proj) +
  geom_sf(data = sf_nc_county_proj)



#get mean precipitation for each site buffer
mean_prec <- exact_extract(x = spr_prec_nc_proj,
                           y = sf_site_buff_proj,
                           fun = "mean", 
                           append_cols = TRUE) %>%
  as_tibble() %>%
  rename(precipitation = mean)

#link these values to site layer
sf_site_prec_buff <- sf_site %>% 
    left_join(mean_prec,
              by = "site_id")

#map precipitation value at each site
ggplot() + 
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_site_prec_buff,
          aes(color = precipitation)) +
  scale_color_viridis_c() +
  theme_classic()


#identify the top three hihg-prec sites
sf_site_prec_buff %>%
  arrange(desc(precipitation)) %>%
  slice(1:3)

