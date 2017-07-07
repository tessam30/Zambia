
# Merge together all Zambia election data ---------------------------------
# In previous files, election data from Zambian elections were imported
# and cleaned.  This file merges everything together.

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017

setwd('~/Documents/GitHub/Zambia/')

# combine presidential data: constituency level breakdowns -----------------------------------------------

# 2016 data
source('ZMB_pres2016_clean.R')

# 2015 data
source('ZMB_pres2015_clean.R')

# 2011 data
source('ZMB_pres2011_clean.R')

# bind together data before 2016.
pr_votes = bind_rows(pr15, pr11)

# merge to geodata --------------------------------------------------------
source('ZMB_import_geo.R')

# 2016 data required to be merged to 2016 shapefile
zmb16 = full_join(pr16, zmb16, by = c("constituency" = "website2016", 
                                      "province" = "province", "district" = "district"))

# pre-2016 data must be merged to 2015 shape file
zmb15 = full_join(pr_votes, zmb15, by = c("constituency" = "website2015", 
                                      "province" = "province", "district" = "district"))