# Import and clean the 2006 Zambia Presidental Election results -----------
# At the constituency level
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 12 July 2017

# setup -------------------------------------------------------------------
# taken care of in ZMB_E01_helpers.R

# library(readxl)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(tidyverse)


# import data -------------------------------------------------------------
# data pulled from https://www.elections.org.zm/media/2006_presidential_results.pdf
# on 29 June 2017
# Exported from Adobe Acrobat into Excel to begin the cleaning process

pr06_raw = read_csv('rawdata/tabula-2006_presidential_results.csv')

pr06_raw = pr06_raw %>% 
  # fill the geographic data down the rows
  mutate(province = prov, district = council, constit = cons) %>% 
  fill(province) %>% 
  fill(district) %>% 
  fill(constit) %>% 
  
  mutate(
    # add in year
    year = 2006,
    # extract constiuency name
    constituency = pretty_strings(str_replace_all(constit, '[0-9]', '')),
    # fix vote_count
    vote_count = str2num(votes),
    # convert to pct
    pct_cast_web = pct_cast_web/100,
    pct_registered_web = pct_registered_web/100,
    pct_rejected_web = pct_rejected_web/100,
    turnout_web = turnout_web/100,
    # make pretty strings
    district = pretty_strings(district),
    province = pretty_strings(province)
  )


# filter out just the total turnout ---------------------------------------
pr06 = pr06_raw %>% 
  filter(is.na(rejected), !is.na(vote_count)) %>% 
  split_candid() %>% 
  calc_stats() %>% 
  select(-cast, -registered)


# pull the total turnout by consituency -----------------------------------
pr06_total = pr06_raw %>% 
  filter(!is.na(rejected)) %>% 
  # calculate turnout stats
  calc_turnout() %>% 
  # merge with geo
  # merge_geo('website2015')
  right_join(geo_base %>% select(-province), by = c('constituency' = 'website2015'))




# merge in turnout data ---------------------------------------------------
pr06 = merge_turnout(pr06, pr06_total)



# check values ------------------------------------------------------------

check_pct(pr06)
check_turnout(pr06_total)


# Clean up the rubbish ----------------------------------------------------
pr06 = filter_candid(pr06)
pr06_total = filter_turnout(pr06_total)

rm(pr06_raw)
