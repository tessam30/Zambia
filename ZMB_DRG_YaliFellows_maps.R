
# Map out the Yali Fellows program----------------------------
# Calls outpus from ZMB_DRG_YaliFellows_geocode.r
# Tim Essam, USAID | GeoCenter, 6 July 2017, tessam@usaid.gov

# setup -------------------------------------------------------------------
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(maptools)
library(ggmap)
library(maps)
library(geosphere)


# Read in data required
base_dir = '~/Zambia/'
 flow_map = read_csv(str_c(base_dir, "mwf_alumns.csv")) %>% 
 rlc = read_csv(str_c(base_dir, "RLC_geocoded.csv"))
 
 map("world")
  lat_max = max(flow_map$lat_univ, na.rm = TRUE)
  lon_min = min(flow_map$lon_univ, na.rm = TRUE)
  lon_max = max(flow_map$lon, na.rm = TRUE)
  lat_min = min(flow_map$lat, na.rm = TRUE)

  xlim <- c(-135, 45)
  ylim <- c(-25, 50)
  
  map("world", col = "#f2f2f2", fill = TRUE, 
      bg = "white", lwd = 0.05, xlim = xlim, ylim = ylim)
  
