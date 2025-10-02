#if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, sf, mapview, here)

# spatial join ------------------------------------------------------------

#point vector
sf_site <- readRDS(file = here("data/sf_finsync_nc.rds"))

#polygon vector
sf_nc_county <- readRDS(file = here("data/sf_nc_county.rds"))


#st_join eveluates two geometric layers
sf_site_join <- st_join(x = sf_site,
                        y = sf_nc_county)

#check how it works
sf_one <- sf_site %>%
  slice(1)

mapview(sf_nc_county) + mapview(sf_one)

#specify county
sf_site_guilford <- sf_site_join %>% 
  filter(county == "guilford")

#extract only guilford county
sf_nc_guilford <- sf_nc_county %>% 
  filter(county == "guilford")

sf_str_guilford <- readRDS(file = here("data/sf_stream_gi.rds"))

#create a map
ggplot() + 
  geom_sf(data = sf_nc_guilford) +
  geom_sf(data = sf_str_guilford) +
  geom_sf(data = sf_site_guilford)


#count the number of points in each county 
#determine county that has the most sites (mecklenburg)
#function n() 
df_n <- sf_site_join %>%
  as_tibble() %>%
  group_by(county) %>%
  summarize(n_site = n()) %>%
  arrange(desc(n_site))
  


#sf_nc_county - this is a "geospatial" object
#df_n - number of sites by county 
#combine them with left_join() function
sf_nc_n <- sf_nc_county %>%
  left_join(df_n,
            by = "county") %>%
  mutate(n_site = ifelse(is.na(n_site),
                          0,
                          n_site))
  
#mapping 
ggplot() +
  geom_sf(data = sf_nc_n,
          aes(fill = n_site))



# geometric analysis ------------------------------------------------------

#length calculation 

#change to projected CRS first
sf_str_proj <- st_transform(sf_str_guilford,
                            crs = 32617)

#calculate
v_str_l <- st_length(sf_str_proj)

#print the first 10 elements
head(v_str_l)
  
#add vector of stream lengths in a new column into our original data  
sf_str_w_len <- sf_str_guilford %>%
  mutate(length = as.numeric(v_str_l))
  
ggplot() +
  geom_sf(data = sf_str_w_len,
          aes(color = length))
  

#area calculation 
(sf_nc_county_proj <- st_transform(sf_nc_county, crs = 32617))


v_area <- st_area(sf_nc_county_proj)

sf_nc_county_w_area <- sf_nc_county %>%
  mutate(area = as.numeric(v_area))

ggplot() +
  geom_sf(data = sf_nc_county_w_area,
          aes(fill = area))



#exercises 
sf_quakes <- readRDS(here("data/sf_quakes.rds"))

sf_nz <- readRDS(here("data/sf_nz.rds"))

mapview(sf_nz) + mapview(sf_quakes)

#1
sf_quakes_join <- st_join(x = sf_quakes,
                          y = sf_nz) 

sf_quakes_nz <- drop_na(sf_quakes_join, fid)
nrow(sf_quakes_nz)

#2
df_n <- sf_site_join %>%
  as_tibble() %>%
  group_by(county) %>%
  summarize(n = n())

sf_n_site <- sf_site_join %>%
  left_join(df_n,
            by = "county")

#3 subset 
sf_n10 <- sf_n_site %>%
            filter(n > 10)

#4 mapping
#create a map
ggplot() + 
  geom_sf(dat = sf_nc_county) +
  geom_sf(data = sf_n_site,
          color = "grey") +
  geom_sf(data = sf_n10,
          color = "salmon")
