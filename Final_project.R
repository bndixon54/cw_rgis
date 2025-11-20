#Final Project

#Generalized Linear Model 
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here,
               janitor)


#load data
data <-read.csv(file="gis_nc_final.csv", header=TRUE)

#check
unique(data$locality) #57 unique localities
unique(data$species) #27 unique species

#clean NA values
clean_data <- data %>%
  drop_na(collection_object_id,
          lon,
          lat,
          locality,
          elevation) 

#remove captures not identified to species level
clean_data <- clean_data %>%
  filter(species != "Peromyscus",
         species != "Sorex")

#check
unique(clean_data$locality) #49 unique localities
unique(clean_data$species) #24 unique species

#manipulate and get both presence and absence
df_nc_spec <- clean_data %>% 
  pivot_wider(id_cols = c(locality, lon, lat),
              names_from = species,
              values_from = presence, 
              values_fill = 0) #changes NAs into 0
#If i only use locality in id_cols(), the function doesnt work. However, it pulls too much data if i also include the lon/lat values, because those are slightly different even within the same locality. I think this is going to complicate things down the line...


# fix ---------------------------------------------------------------------
#suggestion: collapse coordinates by locality
clean_data_new <- clean_data %>%
  group_by(locality) %>%
  mutate(
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  ) %>%
  ungroup()

unique(clean_data_new$lat) #49 unique latitueds lines up with 40 unique localities, so this should work to clean things up...


#remove any duplicates for the sake of just looking at presence vs absence 
clean_data_unique <- clean_data_new %>%
  distinct(locality, lon, lat, species, .keep_all = TRUE)


#NOW we can pivot wider
df_nc_spec <- clean_data_unique %>% 
  pivot_wider(
    id_cols = c(locality, lon, lat),
    names_from = species,
    values_from = presence,
    values_fill = 0
  )

#now this finally lines up with my locality/species info... 

#--------------------------------------------------------------------------

#Myodes gapperi occurrence data
df_gapperi <- df_nc_spec %>%
  select(locality,
         lon,
         lat,
         "Myodes gapperi") %>% 
  rename(y = "Myodes gapperi")


#georeference data
sf_gapperi <- st_as_sf(df_gapperi,
                       coords = c("lon", "lat"),
                       crs = 4326) 

print(sf_gapperi)
#Bounding: xmin: -83.4631 ymin: 35.02472 xmax: -78.99502 ymax: 36.17197


#temperature raster
tmp_nc <- rast("data/spr_tmp_nc.tif")

## extract values at the survey sites
(sf_tmp <- extract(x = tmp_nc,
                   y = sf_gapperi,
                   bind = TRUE) %>% 
    st_as_sf())


#for map
(sf_nc_county <- readRDS("data/sf_nc_county.rds"))

#mapping raster layer with temperature, put survey sites on top
map <- ggplot() +
  geom_spatraster(data = tmp_nc) + #spatial info
  geom_sf(data = sf_nc_county,
          alpha = .25) +
  geom_sf(data = sf_gapperi,    
          aes(color = factor(y))) + 
  scale_color_manual(values = c("#BF4102", "#07ACE3")) + 
  scale_fill_viridis_c(option = "cividis") +   
  theme_bw()   

print(map)

###How do I change the size of my points on this map? I want them smaller but adding "size = 0.05" makes them way bigger









# peromyscus maniculatus --------------------------------------------------

#Peromyscus maniculatus occurrence data
df_manic <- df_nc_spec %>%
  select(locality,
         lon,
         lat,
         "Peromyscus maniculatus") %>% 
  rename(y = "Peromyscus maniculatus")


#georeference data
sf_manic <- st_as_sf(df_manic,
                       coords = c("lon", "lat"),
                       crs = 4326) 

print(sf_manic)
#Bounding: xmin: -83.4631 ymin: 35.02472 xmax: -78.99502 ymax: 36.17197


#temperature raster
tmp_nc <- rast("data/spr_tmp_nc.tif")

## extract values at the survey sites
(sf_tmp_manic <- extract(x = tmp_nc,
                   y = sf_manic,
                   bind = TRUE) %>% 
    st_as_sf())


#for map
(sf_nc_county <- readRDS("data/sf_nc_county.rds"))

#mapping raster layer with temperature, put survey sites on top
map <- ggplot() +
  geom_spatraster(data = tmp_nc) + #spatial info
  geom_sf(data = sf_nc_county,
          alpha = .25) +
  geom_sf(data = sf_manic,    
          aes(color = factor(y))) + 
  scale_color_manual(values = c("#BF4102", "#07ACE3")) + 
  scale_fill_viridis_c(option = "cividis") +   
  theme_bw()   

print(map)


#I'm trying to think of a way to plot each species as a different color so I can show all the different species at once, but I dont think that really makes sense as a spatial map, especially when I have 27 different species. More of a bar graph situation where individual bars for each locality I think? (I do know how to do this, but it's not necessary right now!)


# Analysis of data --------------------------------------------------------

m_maniculatus <- glm(y ~ temperature,
          data = sf_tmp_manic,
          family = "binomial")

summary(m_maniculatus)

#intercept estimate is 4.5740, p-value is 0.0272
#coefficient for temperature is -0.3895, p-value is 0.0303
#p-values are both much higher than 0.01, meaning effect of temperature is likely due to random chance. this makes sense 



#if I wanted to test this for effect of temperature on overall species richness, could I easily do that without building 27 individual dataframes for each species, and then adding each of them to the data category in the glm??? I think that could potentially show a slightly more significant relationship. Same with ectoparasites, but I am still struggling to understand how I would need to code that  to make it all work within this framework. 











#Old stuff I am keeping here to show that I did try a few different things before switching my dataset... Also deleted several other project attempts, so this is only a portion of the amount of tinkering I did before changing my mind 


#visualize
mapview(nv_site,
        legend = FALSE)

#save spatial object
saveRDS(nv_site, file = "data/nv_site.rds")


#read temperature raster
bio_data <-rast("CHELSA_clim_mean.tif")

## extract values at the survey sites
nv_tmps <- extract(x = bio_data,
                       y = nv_site,
                       bind = TRUE) %>% 
    st_as_sf()



#presence/absence
nv_tmps %>% 
  mutate(presence = 1) %>% # all recorded species are "presence" = 1
  pivot_wider(id_cols = c(locality, long, lat),
              names_from = species,
              values_from = presence,
              values_fill = 0)




#dataframe
(df_nv_tmps <- as_tibble(nv_tmps) %>% 
    select(-geometry))


#visualize
ggplot() +
  geom_spatraster(data = bio_data) + 
  geom_sf(data = nv_site,
          aes(color = factor(y))) + 
  scale_fill_viridis_c() +   
  theme_bw() 



#plot the relationship!!!
nv_site %>%
  ggplot(aes(y = y, 
             x = temperature)) +
  geom_point() +
  theme_bw()

















#Elevation (use “elevatr::get_elev_raster()” function in R with bounding box)

