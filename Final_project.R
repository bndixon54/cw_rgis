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
#take average lat/lon at each locality
clean_data <- clean_data %>%
  group_by(locality) %>%
  mutate(lon = mean(lon, na.rm = TRUE),
         lat = mean(lat, na.rm = TRUE)) %>%
  ungroup()

#remove any duplicates for the sake of just looking at presence vs absence 
clean_data <- clean_data %>%
  distinct(locality, lon, lat, species, .keep_all = TRUE)

#NOW we can pivot wider to get presence/absence of all species
df_nc_spec <- clean_data_unique %>% 
  pivot_wider(id_cols = c(locality, lon, lat),
              names_from = species,
              values_from = presence,
              values_fill = 0)




#Myodes gapperi occurrence data -------------------------------------------------

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
#bounding: xmin: -83.4631 ymin: 35.02472 xmax: -78.99502 ymax: 36.17197

#read temperature raster for north carolina
tmp_nc <- rast("data/spr_tmp_nc.tif")

#extract values at the survey sites
sf_tmp_gap <- extract(x = tmp_nc,
                  y = sf_gapperi,
                  bind = TRUE) %>% 
  st_as_sf()

#county polygons for map
sf_nc_county <- readRDS("data/sf_nc_county.rds")

#mapping raster layer with temperature, plot survey sites on top
gapperi_map <- ggplot() +
  geom_spatraster(data = tmp_nc) + #spatial info
  geom_sf(data = sf_nc_county,
          alpha = .25) +
  geom_sf(data = sf_tmp_gap,    
          aes(color = factor(y)),
          size = 0.5) + 
  scale_color_manual(values = c("#BF4102", "#07ACE3")) + 
  scale_fill_viridis_c(option = "cividis") +   
  theme_classic()   

print(gapperi_map)


# Analysis of gapperi data --------------------------------------------------------
#convert
df_tmp_gap <- as_tibble(sf_tmp_gap) %>% 
  select(-geometry)

m_gapperi <- glm(y ~ temperature,
                     data = sf_tmp_gap,
                     family = "binomial")

summary(m_gapperi)

#intercept estimate is 1.2809, p-value is 0.607
#coefficient for temperature is -0.2778, p-value is 0.224
#p-values are both much higher than 0.01, meaning effect of temperature is likely due to random chance. this makes sense 





# Peromyscus maniculatus occurrence data ----------------------------------
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

#temperature raster
tmp_nc <- rast("data/spr_tmp_nc.tif")

#extract values at the survey sites
(sf_tmp_manic <- extract(x = tmp_nc,
                   y = sf_manic,
                   bind = TRUE) %>% 
    st_as_sf())

#for map
(sf_nc_county <- readRDS("data/sf_nc_county.rds"))

#mapping raster layer with temperature, put survey sites on top
map_manic <- ggplot() +
  geom_spatraster(data = tmp_nc) + #spatial info
  geom_sf(data = sf_nc_county,
          alpha = .25) +
  geom_sf(data = sf_manic,    
          aes(color = factor(y)),
          size = .5) + 
  scale_color_manual(values = c("#BF4102", "#07ACE3")) + 
  scale_fill_viridis_c(option = "cividis") +   
  theme_bw()   

print(map_manic)


# Analysis of maniculatus data --------------------------------------------

#convert to df 
df_tmp_manic <- as_tibble(sf_tmp_manic) %>% 
  select(-geometry)

#glm
m_maniculatus <- glm(y ~ temperature,
          data = df_tmp_manic,
          family = "binomial")

summary(m_maniculatus)
#intercept estimate is 4.5740, p-value is 0.0272
#coefficient for temperature is -0.3895, p-value is 0.0303
#p-values are both much higher than 0.01, meaning effect of temperature is likely due to random chance once again

#visualize
df_pred <- ggpredict(m_maniculatus,
                     terms = "temperature [all]")

ggplot() +
  geom_point(data = df_tmp_manic,
             aes(x = temperature,
                 y = y)) +
  geom_line(data = df_pred,
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "grey",
              alpha = 0.3) +
  labs(x = "Air temperature",
       y = "Probability of occurrence") +
  theme_classic()


#if I wanted to test this for effect of temperature on overall species richness, could I easily do that without building 27 individual dataframes for each species, and then adding each of them to the data category in the glm??? I think that could potentially show a slightly more significant relationship. Same with ectoparasites, but I am still struggling to understand how I would need to code that  to make it all work within this framework. 
