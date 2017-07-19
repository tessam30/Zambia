# Import and clean the 2008 Zambia Presidental Election results -----------
# At the constituency level
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 12 July 2017

# setup -------------------------------------------------------------------
# taken care of in ZMB_E01_helpers.R

# library(readxl)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(tidyverse)


# goals -------------------------------------------------------------------
# (1) get votes by candidate
# (2) get voting turnout

# import data -------------------------------------------------------------
# data pulled from https://www.elections.org.zm/media/2008_presidential_election_results.pdf
# on 29 June 2017
# Tables exported from Tablula

pr08_raw = read_csv('rawdata/tabula-2008_presidential_election_results.csv') %>% 
  mutate(
    # specify the year
    year = 2008)


# [1] Pull candidate totals, by constituency ----------------------------------
# split into information about the totals for the constituency, and the candidate-breakdown
pr08_raw2 = pr08_raw %>% 
  filter(!col1 %like% 'Totals') %>% 
  # First thing to do: pull out the constituency name for each of the rows; constituencies prefaced by a number.
  mutate(constit = ifelse(str_detect(col1, '[0-9]'), col1, NA)) %>% 
  fill(constit) %>% 
  # normalize constituency names
  mutate(website2008 = pretty_strings(str_replace_all(constit, '[0-9]', '')))


pr08_raw2 = pr08_raw2 %>% 
  # There are 5 possible configurations for each row from the import:
  # (1) col1 = constituency; everything else is NA (just constit name)
  # (2) col1 = NA, col2 = NA, col3 = candid, col4 = party, col5 = vote_count, col6 = pct_cast, col7 = pct_registered_web
  # (3) col1 = NA, col2 = candid, col3 = NA, col4 = party, col5 = vote_count, col6 = pct_cast, col7 = pct_registered_web
  # (4) col1 = NA, col2 = candid, col3 = party, col4 = vote_count, col5 = pct_cast, col6 = pct_registered_web
  # (5) col1 = candid, col2 = NA, col3 = NA, col4 = party, col5 = vote_count, col6 = pct_cast, col7 = pct_registered_web
  # (6) col1 = candid, col2 = NA, col3 = party, col4 = vote_count, col5 = pct_cast, col6 = pct_registered_web
  mutate(config = case_when(is.na(pr08_raw2$col2) & is.na(pr08_raw2$col3) & is.na(pr08_raw2$col4) ~ 1,
                            is.na(pr08_raw2$col1) & is.na(pr08_raw2$col2) & !is.na(pr08_raw2$col3) ~ 2,
                            !is.na(pr08_raw2$col2) & is.na(pr08_raw2$col3) ~ 3,
                            !is.na(pr08_raw2$col2) & !is.na(pr08_raw2$col3) ~ 4,
                            is.na(pr08_raw2$col2) & is.na(pr08_raw2$col3) ~ 5,
                            !is.na(pr08_raw2$col1) & is.na(pr08_raw2$col2) & !is.na(pr08_raw2$col3) ~ 6,
                            TRUE ~ 10)) %>% 
  # Get rid of column labels taken along for the ride...
  filter(is.na(col9),
         config != 1)

# Okay... so in terms of the numeric values, there are two options: either values are in cols 4-7 (#2, #3, #5)
# or they're in cols 3-6 (#4, #6)
pr08_unshifted = pr08_raw2 %>% 
  filter(config %in% c(2, 3, 5)) %>% 
  mutate(candid = ifelse(config == 2, col3,
                         ifelse(config == 3, col2,
                                ifelse(config == 5, col1, NA_character_)))) %>% 
  mutate(party = col4, vote_count = str2num(col5), pct_cast_web = str2num(col6), pct_registered_web = str2num(col7)) %>% 
  select(website2008, year, candid, party, vote_count, pct_cast_web, pct_registered_web)

pr08_shifted = pr08_raw2 %>% 
  filter(config %in% c(4,6)) %>% 
  mutate(candid = ifelse(config == 4, col2,
                         ifelse(config == 6, col1, NA_character_))) %>% 
  mutate(party = col3, vote_count = str2num(col4), pct_cast_web = str2num(col5), pct_registered_web = str2num(col6)) %>% 
  select(website2008, year, candid, party, vote_count, pct_cast_web, pct_registered_web)

pr08 = bind_rows(pr08_shifted, pr08_unshifted)




# [2] Pull turnout totals for constituency -------------------------------

# split into information about the totals for the constituency, and the candidate-breakdown
pr08_total_raw = pr08_raw %>% 
  filter(col1 %like% 'Totals') %>% 
  # split total column into consituency name
  separate(col1, into = c('total', 'website2008'), sep = 'Totals for') %>% 
  # a few columns have constituency shifted into the next column...
  mutate(website2008 = ifelse(website2008 == '' & !is.na(col2), col2, website2008)) %>% 
  select(-total, -col2, -col3, -col4, -col5, -col6)

# split into two tables: shifted, and not
tot_shifted = pr08_total_raw %>% 
  # Annoyingly, there are frame shifts within the data.  Identifying them if the last column is NA
  filter(is.na(col12)) %>% 
  mutate(rejected = str2num(col7), pct_rejected_web = str2num(col8), 
         cast = str2num(col9), registered = str2num(col10), turnout_web = str2num(col11)) %>% 
  select(website2008, year, cast, registered, rejected, pct_rejected_web, turnout_web)

tot_unshifted = pr08_total_raw %>% 
  # Annoyingly, there are frame shifts within the data.  Identifying them if the last column is NA
  filter(!is.na(col12)) %>% 
  mutate(rejected = str2num(col8), pct_rejected_web = str2num(col9), 
         cast = str2num(col10), registered = str2num(col11), turnout_web = str2num(col12)) %>% 
  select(website2008, year, cast, registered, rejected, pct_rejected_web, turnout_web)

# combine together and calculate values that aren't there
pr08_total = bind_rows(tot_shifted, tot_unshifted) %>% 
  mutate(
    # convert to percent
    pct_rejected_web = pct_rejected_web/100,
    turnout_web = turnout_web/100,
    # prettify strings
    website2008 = pretty_strings(website2008)) %>% 
  calc_turnout()


# test merge to align to lookup table -------------------------------------
# intially trying website2006 as the base
# write.csv(full_join(geo_base %>% mutate(common = website2006), pr08 %>% select(website2008) %>% 
#                       distinct() %>% mutate(common = website2008), 
#                     by = c('common')), '2008_geonames.csv')


# merge to province and district names ------------------------------------
# Assuming 2006 province/district names similar to those in 2008.
pr08_total =   pr08_total %>% # merge w/ geo
  left_join(geo_base %>% select(constituency, district2006, province2006, website2008), by = 'website2008') %>% 
  rename(province = province2006, district = district2006)

# Verify that the numbers I calculate are (roughly) equal to those in the pdf
check_turnout(pr08_total)

# Calculate values for candidate totals by candidate ----------------------
pr08 =  pr08 %>% 
  left_join(geo_base %>% select(constituency, district2006, province2006, website2008), by = 'website2008') %>% 
  rename(province = province2006, district = district2006) %>% 
  split_candid() %>%
  calc_stats() %>% 
  mutate(
    # convert to percents
    pct_cast_web = pct_cast_web /100,
    pct_registered_web = pct_registered_web/100
  )

pr08 = merge_turnout(pr08, pr08_total)

# check calcs
# NOTE: when looking through the data, it's very clear that the pct_cast and pct_registered numbers 
# they report have been rounded to very differnt levels.  pct_cast rounded to nearest percent;
# pct_registered rounded to nearest _hundredth_ of a percent
check_pct(pr08)

# check total is equal b/w two tables
check_constit(pr08, pr08_total)




# Remove interim data frames -----------------------------------------------
pr08 = filter_candid(pr08)
pr08_total = filter_turnout(pr08_total)

rm(tot_shifted, tot_unshifted, pr08_raw, pr08_raw2, pr08_shifted, pr08_total_raw, pr08_unshifted)

