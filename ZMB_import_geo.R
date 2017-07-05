
# Installation instructions -----------------------------------------------
# To install on Mac, have to first get GDAL, PROJ, GEOS (from QGIS installation, via KChang...)
# Will install in library/Frameworks
# Had to use Homebrew to get PROJ linked in right location: brew install proj
# https://github.com/edzer/sfr
# https://github.com/edzer/sfr/issues/335
# https://stat.ethz.ch/pipermail/r-sig-mac/2017-June/012429.html
# 
# then at terminal: R CMD INSTALL sf_0.5-1.tar.gz --configure-args='-with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/2.1/unix/bin/gdal-config -with-geos-config=/Library/Frameworks/GEOS.framework/Versions/3/unix/bin/geos-config' 

library(sf)
library(ggplot2)
library(dplyr)

base_dir = '~/Documents/GitHub/Zambia/'

# import master name list. ------------------------------------------------
geo_base = read_excel(paste0(base_dir, 'ZMB_admin_crosswalk.xlsx'), sheet = 2)

# import data
zmb15 = st_read(paste0(base_dir, 'geodata/constituencies/'), layer = 'GRED_Zambia_2006_beta2')
zmb16 = st_read(paste0(base_dir, 'geodata/constituencies/'), layer = 'Zambia_Const_156')

# re-project
proj_str = '+proj=laea +lon_0=27.509765625'
st_crs(zmb15)
zmb16 = st_transform(zmb16, proj_str)
zmb15 = st_transform(zmb15, proj_str)

# Can't intersect b/c of topology self-intersections.
# isect = st_intersection(zmb15, zmb16)


p = ggplot(zmb16) + geom_sf()

zmb15 = left_join(zmb15, geo_base, by = c('CST_N' = "shp2015"))
zmb16 = left_join(zmb16, geo_base, by = c('ConstName1' = "shp2016"))
