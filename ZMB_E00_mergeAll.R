
# Merge together all Zambia election data ---------------------------------
# In previous files, election data from Zambian elections were imported
# and cleaned.  This file merges everything together.

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017


# NOTES ON DATA FRAMES ----------------------------------------------------
# Data are organized into 5 major categories:
# 1. Presidential vote totals by candidate, by constituency (by year). Includes:
# - prX, where X is the last 2 digits of the year, e.g. pr16.  Totals for a given year.
# - pr_votes_06_15: combined totals for 2006-2015; used to merge to shapefile w/ 150 constituencies 
#   (in 2016, shifted to 156 constituencies to keep things interesting)
# - pr_votes: all combined data, 2006-2016. Should not be used to merge to geographic data
# 2. Presidential turnout by constituency (by year). Includes:
# - prX_total, where X is the last 2 digits of the year, e.g. pr16_total.  Totals for a given year.
# - pr_turnout_06_15: combined totals for 2006-2015; used to merge to shapefile w/ 150 constituencies 
# - pr_turnout: all combined data, 2006-2016. Should not be used to merge to geographic data
# 3. Assembly/parlimentary vote totals by candidate, by constituency (by year). Includes:
# - asX, where X is the last 2 digits of the year, e.g. as16.  Totals for a given year.
# - as_votes_06_15: combined totals for 2006-2015; used to merge to shapefile w/ 150 constituencies 
# - as_votes: all combined data, 2006-2016. Should not be used to merge to geographic data
# 4. Assembly/parlimentary turnout by constituency (by year). Includes:
# - asX_total, where X is the last 2 digits of the year, e.g. as16_total.  Totals for a given year.
# - as_turnout_06_15: combined totals for 2006-2015; used to merge to shapefile w/ 150 constituencies 
# - as_turnout: all combined data, 2006-2016. Should not be used to merge to geographic data
# 5. Geographic data: shapefiles of the 150 or 156 constituencies 
# --> zmb15 (shapefile for 2015 and before)
# --> zmb16 (shapefile for 2016)
# --> pres16 (shp + pr16)
# --> parl16 (shp + as16)
# --> turnout16 (shp + pr16_total)
# --> pres06_15 (shp + pr_votes_06_15)
# --> parl06_15 (shp + as_votes_06_15)
# --> turnout_06_15 (shp + pr_votes_total_06_15)


# setup -------------------------------------------------------------------
# Set base directory for the files, outputs, source code.
# laura
base_dir = '~/Documents/GitHub/Zambia/'

# tim
# base_dir = '~/GitHub/Zambia/'


export_dir = paste0(base_dir, 'exported_fromR/')
data_dir = paste0(base_dir, 'processeddata/')

setwd(base_dir)

# helper functions, including libraries
source('ZMB_E01_helpers.R')

# [1 & 2] combine presidential data: constituency level breakdowns -----------------------------------------------

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



# [3 & 4] combine assembly data: constituency level breakdowns -----------------------------------------------
source('ZMB_E08_assembly2016_clean.R')
source('ZMB_E09_assembly2011_clean.R')
source('ZMB_E10_assembly2006_clean.R')

# merge to colors ---------------------------------------------------------
as06 = as06 %>% left_join(parties, by = c('party' = 'pres2006'))
as11 = as11 %>% left_join(parties, by = c('party' = 'pres2011'))
as16 = as16 %>% left_join(parties, by = c('party' = 'pres2016'))


# [5] import geodata --------------------------------------------------------
source('ZMB_E02_import_geo.R')

# bind data; create dummy data for empty constituencies -------------------


# bind to data frame with empty data frame, to make sure have a polygon for each party/year/constituency combo
# If not... choropleths will have holes!
bind_zeroVals = function(votes, shp) {
  # get all unique constituencies
  cons = shp %>% pull(constituency)
  # get all unique years
  yrs = votes %>% distinct(year) %>% pull()
  # replicate each constituency by each year
  cons = data.frame(year = rep(yrs, length(cons)), constituency = rep(cons, each = length(yrs)))
  
  parties =  votes %>% select(year, party_name, color) %>% distinct()
  
  cons_parties = full_join(cons, parties, by = 'year')
  
  replaceNAs = function(df) {
    full_join(df, cons_parties, c('year', 'constituency', 'party_name', 'color')) %>% 
      # replace NAs with 0
      mutate(
        vote_count = coalesce(vote_count, 0),
        pct_cast = coalesce(pct_cast, 0),
        pct_votes = coalesce(pct_votes, 0)
      )
  }
  
  # replace NAs for each of the datasets.
  votes = replaceNAs(votes)
  
  return(votes)
}

# add in dummy constituencies
na_color = "#c7c8ca"

as_votes_06_11 = bind_zeroVals(bind_rows(as11, as06), zmb15) %>% 

# fix constituencies which didn't have elections.  Replacing with NAs
mutate(vote_count = ifelse(year == 2006 & constituency == 'Lupososhi', NA, vote_count),
       pct_cast = ifelse(year == 2006 & constituency == 'Lupososhi', NA, pct_cast),
       pct_votes = ifelse(year == 2006 & constituency == 'Lupososhi', NA, pct_votes),
       color = ifelse(year == 2006 & constituency == 'Lupososhi', na_color, color),
       
       vote_count = ifelse(year == 2006 & constituency == 'Manyinga', NA, vote_count),
       pct_cast = ifelse(year == 2006 & constituency == 'Manyinga', NA, pct_cast),
       pct_votes = ifelse(year == 2006 & constituency == 'Manyinga', NA, pct_votes),
       color = ifelse(year == 2006 & constituency == 'Manyinga', na_color, color),
       
       vote_count = ifelse(year == 2011 & constituency == 'Nakonde', NA, vote_count),
       pct_cast = ifelse(year == 2011 & constituency == 'Nakonde', NA, pct_cast),
       pct_votes = ifelse(year == 2011 & constituency == 'Nakonde', NA, pct_votes),
       color = ifelse(year == 2011 & constituency == 'Nakonde', na_color, color),
       
       vote_count = ifelse(year == 2011 & constituency == 'Magoye', NA, vote_count),
       pct_cast = ifelse(year == 2011 & constituency == 'Magoye', NA, pct_cast),
       pct_votes = ifelse(year == 2011 & constituency == 'Magoye', NA, pct_votes),
       color = ifelse(year == 2011 & constituency == 'Magoye', na_color, color))

as16 = bind_zeroVals(as16, zmb16)

as_votes = bind_rows(as16, as_votes_06_11)

# bind together data -- turnout numbers
as_turnout_06_11 = bind_rows(as11_total, as06_total)
as_turnout = bind_rows(as16_total, as_turnout_06_11)



# merge geodata -----------------------------------------------------------


# 2016 data required to be merged to 2016 shapefile
pres16 = full_join(zmb16, pr16, by = c("constituency" = "constituency", "province" = "province", "district2016" = "district"))
parl16 = full_join(zmb16, as16, by = c("constituency" = "constituency", "province" = "province", "district2016" = "district"))

turnout16 = full_join(zmb16, pr16_total, by = c("constituency" = "constituency", "province" = "province", "district2016" = "district"))

# pre-2016 data must be merged to 2015 shape file
pres_06_15 = full_join(zmb15, pr_votes_06_15, by = c("constituency"))
parl_06_11 = full_join(zmb15, as_votes_06_11, by = c("constituency"))

turnout_06_15 = full_join(zmb15, pr_turnout_06_15, by = c("constituency"))

