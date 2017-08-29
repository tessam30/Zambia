
# Import Zambia geographic data -------------------------------------------
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 5 July 2017


# Installation instructions -----------------------------------------------
# To install on Mac, have to first get GDAL, PROJ, GEOS (from QGIS installation, via http://www.kyngchaos.com/software/qgis)
# Will install in library/Frameworks
# Had to use Homebrew to get PROJ linked in right location: brew install proj
# Also requires XCode to be installed (to compile C)
# https://github.com/edzer/sfr
# https://github.com/edzer/sfr/issues/335
# https://stat.ethz.ch/pipermail/r-sig-mac/2017-June/012429.html
# 
# then at terminal: (default location seems to be unlinked when try to install from within RStudio)
# R CMD INSTALL sf_0.5-1.tar.gz --configure-args='-with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/2.1/unix/bin/gdal-config -with-geos-config=/Library/Frameworks/GEOS.framework/Versions/3/unix/bin/geos-config' 


# setup -------------------------------------------------------------------

# taken care of in ZMB_E01_helpers.R
# library(sf)
# library(ggplot2)
# library(dplyr)
# library(readxl)
# library(stringr)
# library(data.table)
# 
# base_dir = '~/Documents/GitHub/Zambia/'

# Sources -----------------------------------------------------------------
# Zambia election constituencies were split from 150 constituencies to 156 for 2016 election. (https://zambiareports.com/2015/05/13/cabinet-creates-six-new-constituencies/)
# 1) Zambia 2016 Constituencies source: https://github.com/lightonphiri/data-zambia-shapefiles/tree/master/cso-shapefiles
# Based on: http://lightonphiri.org/blog/mapping-the-zambia-2016-presidential-election-results
# 2) Zambia pre-2016 Constituencies from U. Michigan Constituency-Level Elections Archive
# http://www.electiondataarchive.org/datacenter-gred.html
# 3) Zambia district, province data from John Booz, USAID | Zambia, from Ministry of Land, circa 2015.
# 4) Election data from Zambian government: https://www.elections.org.zm/general_election_2016.php

# import master name list. ------------------------------------------------
# Creates a crosswalk between the shapefile names and those used on election website (with vote count)
# Also connects the 150 constituencies from pre-2016 to the 156 afterwards.
# Note that while the names may be the same, the boundaries have shifted and in some cases are quite different.
# Boundary crosswalk created by pulling geographic names from the election 
geo_base = read_excel(paste0(base_dir, 'ZMB_admin_crosswalk.xlsx'), sheet = 2)


# import / clean geographic constituency data -----------------------------

setwd('geodata/constituencies/')

# import data
zmb15 = st_read(dsn = '.', layer = 'Zambia_Const_150_simpl')
zmb16 = st_read(dsn = '.', layer = 'Zambia_Const_156_simpl')

# fix "Shiwang\x92Andu" -- gives merging issues
zmb15 = zmb15 %>% mutate(ConstName = ifelse(as.character(CST_N) %like% 
                                              'Shiwang', "Shiwang'Andu", as.character(CST_N)))

# title case convert constituency name, for merging
zmb16 = zmb16 %>% mutate(shp2016 = str_to_title(ConstName1))

# re-project
proj_str = '+proj=laea +lon_0=27.509765625'
st_crs(zmb15)
zmb16 = st_transform(zmb16, proj_str)
zmb15 = st_transform(zmb15, proj_str)

# Can't intersect b/c of topology self-intersections.
# isect = st_intersection(zmb15, zmb16)




# merge crosswalk data ----------------------------------------------------
zmb15 = left_join(zmb15, geo_base, by = c('ConstName' = "shp2015"))
zmb16 = left_join(zmb16, geo_base, by = c('shp2016' = "shp2016"))


# quick plot (example) --------------------------------------------------------------
# p1 = plot(zmb16 %>% select(province))

p2 = ggplot(zmb16) + geom_sf(aes(fill = constituency), show.legend = FALSE)

# Reset working directory -------------------------------------------------
setwd(base_dir)
