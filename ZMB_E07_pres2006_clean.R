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
    website2006 = pretty_strings(str_replace_all(constit, '[0-9]', '')),
    # fix Shiwang'andu
    website2006 = ifelse(website2006 %like% 'Shiwang', "Shiwang'andu", website2006),
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
  select(-cast, -registered) %>% 
  # merge w/ geo
  left_join(geo_base %>% select(constituency, website2006), by = 'website2006') %>% 
  calc_stats()


# for merging together; intially trying website2011 as the base
# write.csv(full_join(geo_base %>% mutate(common = website2011), pr06 %>% select(province2006 = province, district2006 = district, website2006) %>% 
#                       distinct() %>% mutate(common = website2006), 
#                      by = c('common')), '2006_geonames.csv')


# pull the total turnout by consituency -----------------------------------
pr06_total = pr06_raw %>% 
  filter(!is.na(rejected)) %>% 
  # calculate turnout stats
  calc_turnout() %>% 
  # merge w/ geo
  left_join(geo_base %>% select(constituency, website2006), by = 'website2006')




# merge in turnout data ---------------------------------------------------
pr06 = merge_turnout(pr06, pr06_total)



# check values ------------------------------------------------------------

check_pct(pr06)
check_turnout(pr06_total)


# Clean up the rubbish ----------------------------------------------------
pr06 = filter_candid(pr06)
pr06_total = filter_turnout(pr06_total)

rm(pr06_raw)
