
# Merge together all Zambia election data ---------------------------------
# In previous files, election data from Zambian elections were imported
# and cleaned.  This file merges everything together.

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017



# setup -------------------------------------------------------------------
# Set base directory for the files, outputs, source code.
base_dir = '~/Documents/GitHub/Zambia/'
# base_dir = '~/GitHub/Zambia/'


export_dir = paste0(base_dir, 'exported_fromR/')
data_dir = paste0(base_dir, 'processed_data')

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


# merge to colors ---------------------------------------------------------
# import colors
parties = read_csv(paste0(base_dir, '/party_crosswalk.csv'))
pr06 = pr06 %>% left_join(parties, by = c('party' = 'pres2006'))
pr08 = pr08 %>% left_join(parties, by = c('party' = 'pres2008'))
pr11 = pr11 %>% left_join(parties, by = c('party' = 'pres2011'))
pr15 = pr15 %>% left_join(parties, by = c('party' = 'pres2015'))
pr16 = pr16 %>% left_join(parties, by = c('party' = 'pres2016'))


# bind data ---------------------------------------------------------------

# bind together data

pr_votes_06_15 = bind_rows(pr15, pr11)
pr_votes_06_15 = bind_rows(pr_votes_06_15, pr08)
pr_votes_06_15 = bind_rows(pr_votes_06_15, pr06)

pr_votes = bind_rows(pr16, pr_votes_06_15)

# bind together data -- turnout numbers

pr_turnout_06_15 = bind_rows(pr15_total, pr11_total)
pr_turnout_06_15 = bind_rows(pr_turnout_06_15, pr08_total)
pr_turnout_06_15 = bind_rows(pr_turnout_06_15, pr06_total)
pr_turnout = bind_rows(pr16_total, pr_turnout_06_15)



# merge to geodata --------------------------------------------------------
source('ZMB_E02_import_geo.R')

# 2016 data required to be merged to 2016 shapefile
pres16 = full_join(zmb16, pr16, by = c("constituency" = "constituency", "province" = "province", "district2016" = "district"))

turnout16 = full_join(zmb16, pr16_total, by = c("constituency" = "constituency", "province" = "province", "district2016" = "district"))

# pre-2016 data must be merged to 2015 shape file
pres_06_15 = full_join(zmb15, pr_votes_06_15, by = c("constituency"))

turnout_06_15 = full_join(zmb15, pr_turnout_06_15, by = c("constituency"))

