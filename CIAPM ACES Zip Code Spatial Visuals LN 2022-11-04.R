#Title: CIAPM ACES Zip Code Spatial Visuals Script
#File: CIAPM ACES Zip Code Spatial Visuals Script 2022-11-04
#Authors: Liam North

#REFERRENCES###############################################################
  #https://rpubs.com/walkerke/zcta
  #https://community.rstudio.com/t/heatmap-using-tigris-zcats-mapping-zipcodes/116689#> Linking to GEOS 3.9.1, GDAL 3.3.2, PROJ 8.1.1
  #https://stackoverflow.com/questions/60419762/mapping-my-data-to-a-zip-code-area-map-in-r
  #https://www.infoworld.com/article/3505897/how-to-do-spatial-analysis-in-r-with-sf.html

#LOAD NECESSARY PACKAGES ##################################################
pacman::p_load(pacman, party, psych, rio, tidyverse, dplyr, magrittr, ggplot2, car, ggrepel,ggpubr, epiDisplay, tigris, sp, rgeos, leaflet, sf, tmap, tmaptools, rgeos, viridis)
options(tigris_use_cache = TRUE)

#IMPORT DATA###############################################################
#Df1 for participant zip code csv named "CIAPM ACES zip codes 2022-11-04"
df1 <- data.frame(read.csv(file.choose()))
head(df1)
tail(df1)
as_tibble(df1)

#MAKE ZIP CODE TABLE AND WRANGLE###########################################
tab1(df1$address_zip, sort.group = "decreasing", cum.percent = TRUE)

plot_ready <- tibble(
  zip_code = c(93033,
               91766,
               91706,
               91402,
               90650,
               90255,
               90033,
               93065,
               93063,
               93030,
               93003,
               91768,
               91722,
               91356,
               90660,
               90247,
               90201,
               90061,
               90022,
               90011,
               90004,
               90003,
               90002,
               93536,
               93060,
               91803,
               91789,
               91780,
               91767,
               91746,
               91733,
               91710,
               91506,
               91381,
               91360,
               91355,
               91354,
               91344,
               91342,
               91340,
               91335,
               91326,
               91324,
               91205,
               91104,
               91030,
               90717,
               90716,
               90640,
               90638,
               90606,
               90605,
               90604,
               90601,
               90501,
               90302,
               90293,
               90280,
               90278,
               90262,
               90241,
               90221,
               90065,
               90063,
               90057,
               90047,
               90046,
               90043,
               90042,
               90037,
               90032,
               90026,
               90023,
               90020,
               90017,
               90016,
               90015,
               90012
  ),
  freq_zips = c(4,
                3,
                3,
                3,
                3,
                3,
                3,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                2,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1,
                1
                
                          )) %>%
  mutate(zip_to_zcta = zip_code %/% 1000)

zip_to_zcta <- plot_ready$zip_code %/% 1000

#CONVERT ZIP TO CHARACTER##################################################
plot_ready <- plot_ready %>%
  mutate(zip_code = as.character(zip_code))

#MERGE ZIP CODES TO ZCTA OBJECT############################################
poly_zctas <- zctas(cb = TRUE, starts_with = zip_to_zcta) %>%
  left_join(plot_ready, by = c("ZCTA5CE20" = "zip_code"))

#CREATE URBAN AREA LAYER FOR MAP###########################################
uas <- urban_areas() %>% filter(str_detect(NAME10, "Los Angeles"))

#CREATE LA ZCTAS BACKGROUND LAYER##########################################
la_zctas <- zctas(
  cb = TRUE, 
  starts_with = c("900", "901", "902", "903", "904", "905", "906", "907", "908", "909", "910", "911", "912", "913","914", "915", "916", "917", "918", "919", "920", "921", "922", "923", "924", "925", "926", "927", "928", "929", "930", "931", "932", "933", "934", "935"),
  year = 2020
)

#ENSURE CORRECT CRS########################################################
la_zctas <- st_transform(la_zctas, crs = 4326)
la_zctas

#MAP OF LA + VENTURA COUNTY ZIP CODE DENSITY GREY##########################
ggplot() +
  #geom_sf(data = uas) +
  geom_sf(data = la_zctas, fill = '#828282')+
  geom_sf(data = poly_zctas, aes(fill = freq_zips))+
  scale_fill_gradientn(colours = terrain.colors(10))+
  coord_sf(xlim = c(-119.5, -117.5), ylim = c(33.6, 35))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 18),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#MAP OF LA + VENTURA COUNTY ZIP CODE DENSITY WHITE#########################
ggplot() +
  #geom_sf(data = uas, fill = '#FFFFFF') +
  geom_sf(data = la_zctas, fill = '#FFFFFF')+
  geom_sf(data = poly_zctas, aes(fill = freq_zips))+
  scale_fill_gradientn(colours = terrain.colors(10), na.value = "white", name="Número de participantes\npor código postal")+
  coord_sf(xlim = c(-119.5, -117.5), ylim = c(33.6, 35))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 18),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key.size = unit(1.5, "line"), legend.text=element_text(size=12), legend.title = element_text(size=14))+
  labs(title = "Family First Study Participant Heat Map by Zip Code")


