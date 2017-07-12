
# Merge together all Zambia election data ---------------------------------
# In previous files, election data from Zambian elections were imported
# and cleaned.  This file merges everything together.

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017



# setup -------------------------------------------------------------------
# Set base directory for the files, outputs, source code.
base_dir = '~/Documents/GitHub/Zambia/'
setwd(base_dir)

# helper functions, including libraries
source('ZMB_E01_helpers.R')

# combine presidential data: constituency level breakdowns -----------------------------------------------

# 2016 data
source('ZMB_E03_pres2016_clean.R')

# 2015 data
source('ZMB_E04_pres2015_clean.R')

# 2011 data
source('ZMB_E05_pres2011_clean.R')

# 2008 data
source('ZMB_E06_pres2008_clean.R')

# 2006 data
source('ZMB_E07_pres2006_clean.R')

# bind together data
pr_votes = bind_rows(pr16, pr15)
pr_votes = bind_rows(pr_votes, pr11)

# merge to geodata --------------------------------------------------------
source('ZMB_E02_import_geo.R')

# 2016 data required to be merged to 2016 shapefile
zmb16 = full_join(zmb16, pr16, by = c("website2016" = "constituency",
                                      "province" = "province", "district" = "district"))

# pre-2016 data must be merged to 2015 shape file
zmb11 = full_join(zmb15, pr11, by = c("website2011" = "constituency", 
                                      "province" = "province", "district" = "district"))

zmb15 = full_join(zmb15, pr15, by = c("website2015" = "constituency"))
