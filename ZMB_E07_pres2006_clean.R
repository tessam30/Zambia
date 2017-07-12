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
    vote_count = str2num(votes)
  )


# filter out just the total turnout ---------------------------------------
pr06 = pr06_raw %>% 
  filter(is.na(rejected), !is.na(vote_count))


# pull the total turnout by consituency -----------------------------------
pr06_total = pr06_raw %>% 
  filter(!is.na(rejected))



