# Create basic plots of the YALI fellows for DRG office.
# Uses YALI data provided by team
# Author: Tim Essam, PhD, USAId GeoCenter, tessam@usaid.gov
# Date: 2017_07_06
#-------------------------------------------------------------

library(tidyverse)
library(stringr)
library(forcats)
library(leaflet)
library(sp)
library(RColorBrewer)
library(maptools)
library(mapdata)

base_dir = '~/Zambia/'
df = read_csv(str_c(base_dir, "RLC_geocoded.csv"))


df_sub = df %>% 
  select(lat, lon, Fellow, Year, Track_name, City, n)
write_csv(df_sub, str_c(base_dir, "RLC_geocoded_subset.csv"))


# Quick leaflet map of all fellows
leaflet(df_sub) %>%
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

# Define country polygons for borders
  zmb = map_data("worldHires", "Zambia")
  basemap =  ggplot() + 
    geom_polygon(data = zmb, 
                 aes(x=long, y = lat, group = group), 
                 fill = "white", color="black") + 
    coord_fixed()
 
 basemap + geom_point(data = df_sub, aes(x = lon, y = lat, fill = Track_name, size = log(n)), 
               color = "gray", shape=21, alpha = 0.75) +
   facet_wrap(~Track_name) + theme_bw() +
   theme(legend.position="none") +
   ggtitle("DRAFT: RLF YALI Fellow locations by track type (all years)", 
           subtitle = "Circle size corresponds to total number of fellows at location.")
 ggsave("RLF_YALI_draft.png")
 
 # What is the breakdown of fellows over time (bar plot) by city
 df %>%
   filter(!is.na(City), Track_name != "NA", !is.na(Year)) %>% 
   mutate(City = City %>% fct_infreq() %>% fct_rev()) %>% 
   ggplot(aes(City, fill = Track_name)) +
   geom_bar()+
   coord_flip() + 
   facet_wrap(~Year, scales = "free")
 
 
 ggplot(df) + geom_bar(aes(City, fill = Year))
 
 

# ---------------------------------------------------------------
# Load in the mwf_alumns.csv
 mwf = read_csv(str_c(base_dir, "mwf_alumns.csv"))
 
 # Subset data to remove anything that could be considered sensitive
 mwf_sub = mwf %>% 
   select(Mailing_city, Track_name, Year, Host_university_std, lat, lon, n.x, n.y)

 # Plot by Track
 basemap + geom_point(data = mwf_sub, aes(x = lon, y = lat, fill = Track_name, size = log(n.x)), 
                      color = "gray", shape=21, alpha = 0.75) +
   facet_wrap(~Track_name) + theme_bw() +
   theme(legend.position="none") +
   ggtitle("DRAFT: MWF Alumni locations by track type (all years)", 
           subtitle = "Circle size corresponds to total number of fellows at location.")
 ggsave("MWF_Alumni.png")
   
