
# Import Zambia election constituency shpfiles ----------------------------
# Imports shape files for Zambia's election data
# Laura Hughes, USAID | GeoCenter, 5 July 2017, lhughs@usaid.gov


# Sources -----------------------------------------------------------------
# Zambia election constituencies were split from 150 constituencies to 156 for 2016 election. (https://zambiareports.com/2015/05/13/cabinet-creates-six-new-constituencies/)
# 1) Zambia 2016 Constituencies source: https://github.com/lightonphiri/data-zambia-shapefiles/tree/master/cso-shapefiles
# Based on: http://lightonphiri.org/blog/mapping-the-zambia-2016-presidential-election-results
# 2) Zambia pre-2016 Constituencies from U. Michigan Constituency-Level Elections Archive
# http://www.electiondataarchive.org/datacenter-gred.html
# 


# setup -------------------------------------------------------------------

library(geocenter)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)

base_dir = '~/Documents/GitHub/Zambia/'

# PROJ-4 string to reproject data before converting it to lat/lons
# Equatorial Lambert azimuthal equal-area PROJ.4
# Central meridian: 027º 30' E
proj_str = '+proj=laea +lon_0=27.509765625'

# import shp --------------------------------------------------------------

zmb156 = shp2df(baseDir = paste0(base_dir, 'geodata/constituencies/'), 
                layerName = 'Zambia_Const_156', getCentroids = FALSE,
                reproject = TRUE, projection = proj_str)

zmb150 = shp2df(baseDir = paste0(base_dir, 'geodata/constituencies/'), 
                layerName = 'GRED_Zambia_2006_beta2', getCentroids = FALSE,
                reproject = TRUE, projection = proj_str)


# Pull out unique names ---------------------------------------------------

zmb156 = zmb156 %>% 
  mutate(shp2016 = str_to_title(ConstName1))

zmb150 = zmb150 %>% 
  mutate(shp2015 = str_to_title(CST_N))


cst156 = zmb156 %>% select(shp2016) %>% mutate(constituency = shp2016) %>% distinct()
cst150 = zmb150 %>% select(shp2015) %>% mutate(constituency = shp2015) %>% distinct() 

# Merging, crosswalks -----------------------------------------------------

# from ZMB_assembly2016_clean.R
cst156_website = read.csv('~/Documents/GitHub/Zambia/processeddata/ZMB_2016_adminnames.csv') %>% select(-X)

# from ZMB_pres2015_clean.R
cst150_website = pres15 %>% select(constName) %>% distinct()

cst156_crosswalk = full_join(cst156, cst156_website %>% mutate(website2016 = constituency), 
                             by = c("constituency" = "constituency"))

write.csv(cst156_crosswalk, '~/Documents/GitHub/Zambia/processeddata/cst156_crosswalk.csv')


cst150_crosswalk = full_join(cst150, cst150_website %>% mutate(website2015 = constName), by = c("constituency" = "constName"))
write.csv(cst150_crosswalk, '~/Documents/GitHub/Zambia/processeddata/cst150_crosswalk.csv')

crosswalk = full_join(cst150_crosswalk, cst156_crosswalk, by = c("constituency"))

crosswalk = crosswalk %>% mutate(merge_flag = ifelse(is.na(website2015) | is.na(website2016) | 
                                                       is.na(shp2015) | is.na(shp2016), 1, 0))

write.csv(crosswalk, '~/Documents/GitHub/Zambia/processeddata/cst_crosswalk.csv')

# After identifying where the discrepancies are, merged by hand into ZMB_admin_crosswalk.xlsx
# Used Zambia Electoral Commission's Public Notice on 7 May 2016 for changes to constituencies
# Merged by hand w/ similar names, double checking with overlaid shapefiles.
# !!! NOTE that shapefiles b/w pre-2016 and post-2016 do not exactly overlay, and some parts of the districts were cut into pieces.


# import master name list. ------------------------------------------------
geo_base = read_excel(paste0(base_dir, 'ZMB_admin_crosswalk.xlsx'), sheet = 2)

# merge to shpfiles
zmb150 = left_join(zmb150, geo_base, by = c("shp2015"))

zmb156 = left_join(zmb156, geo_base, by = c("shp2016"))


# plot --------------------------------------------------------------------
# p = ggplot(zmb156, aes(x = long, y = lat, fill = district, group = group, order = order)) +
#   geom_polygon() +
#   geom_path(size = 0.1, color = 'white') +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position = 'none')
