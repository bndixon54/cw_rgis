#SDM 01

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here)


# prepare ecological data -------------------------------------------------
(df_finsync <- read_csv(here("data/data_finsync_nc.csv")))
 
#examine first survey site
(df_st1 <- df_finsync %>% 
    filter(site_id == "finsync_nrs_nc-10013"))
#eight rows corresponds to the 8 species present at this site


#to manipulate and get both presence and absence... use pivot_wider
df_w <- df_finsync %>% 
  pivot_wider(id_cols = c(site_id, lon, lat), #specifies columns we want to use to identify unique site
              names_from = latin,
              values_from = presence,
              values_fill = 0) #this table now has a bunch of NA's, which we want to replace with zeros using this code

#Redbreast Sunfish (Lepomis auritus) occurrence data
df_rbs <- df_w %>%
    select(site_id,
           lon,
           lat,
           "Lepomis auritus") %>% 
    rename(y = "Lepomis auritus")
#now the dataframe has presence and absence, which we can now associate with environmental conditions! 

#Linking to Environment: tie occurrence dataframe df_rbs to air temperature at each survey site using the point-wise extraction method from Chapter 5
#create sf object with both pieces of data 
sf_rbs <- st_as_sf(df_rbs,
                   coords = c("lon", "lat"),
                   crs = 4326) 

#redbreast sunfish is a warm water species!!! Yaaaaaay


#read the raster layer that contains air temperature data
spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))


#then use the terra::extract() to obtain values at the survey sites using the sf object 
sf_rbs_tmp <- extract(x = spr_tmp_nc,
                      y = sf_rbs, #where we want to get the data from
                      bind = TRUE) %>% 
    st_as_sf() #convert to easier format to work with

#mapping raster layer with temperature, put survey sites on top
map <- ggplot() +
  geom_spatraster(data = spr_tmp_nc) + #spatial info
  geom_sf(data = sf_rbs_tmp,    #fish at survey sites
          aes(color = factor(y))) + #add attributes from point data. this is presence or absence
  scale_fill_viridis_c() +   
  theme_bw()   


# Statistical analysis ----------------------------------------------------
df_rbs_w_tmp <- as_tibble(sf_rbs_tmp)


#plot the relationship between species occurrence and air temperature using a scatter plot 
df_rbs_w_tmp %>%
  ggplot(aes(y = y, 
             x = temperature)) +
  geom_point() + #individual points for each survey site
  theme_bw()
  

#now we incorporate the statistical analysis............. specifically a Binomial Regression Model to evaluate the statistical influence of air temperature on the distribution of Redbreast Sunfish

m_rbs <- glm(y ~ temperature, #response(presence or absence of fish) ~ predictor
          data = df_rbs_w_tmp, #make sure the dataframe you are using contains the column name you are using as response variable
          family = "binomial")

summary(m_rbs)


#visualize the output, draw predictive line
df_pred <- ggpredict(m_rbs, #model we developed
                     terms = "temperature [all]") 

ggplot() +
  geom_point(data = df_rbs_w_tmp,
             aes(x = temperature,
                 y = y)) +
  geom_line(data = df_pred, #the expected line
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_pred,  #shade behind the line suggests uncertainty
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "grey",
              alpha = 0.2) +
  labs(x = "Air temperature",
       y = "Probability of occurrence") +
  theme_bw()

#ggpredict is useful for a model with multiple predictor values. this function fixes the other predictors 




