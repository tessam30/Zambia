library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)

# import data -------------------------------------------------------------
# data pulled from https://www.elections.org.zm/media/2008_presidential_election_results.pdf
# on 29 June 2017
# Tables exported from Tablula

pr08_raw = read_csv('rawdata/tabula-2008_presidential_election_results.csv') %>% 
  mutate(
    # specify the year
    year = 2008)


# Pull candidate totals, by constituency ----------------------------------
# split into information about the totals for the constituency, and the candidate-breakdown
pr08_raw2 = pr08_raw %>% 
  filter(!col1 %like% 'Totals') %>% 
  # First thing to do: pull out the constituency name for each of the rows; constituencies prefaced by a number.
  mutate(constit = ifelse(str_detect(col1, '[0-9]'), col1, NA)) %>% 
  fill(constit) 


pr08_raw2 = pr08_raw2 %>% 
  # There are 5 possible configurations for each row from the import:
  # (1) col1 = constituency; everything else is NA (just constit name)
  # (2) col1 = NA, col2 = NA, col3 = candid, col4 = party, col5 = vote_count, col6 = pct_cast, col7 = pct_registered
  # (3) col1 = NA, col2 = candid, col3 = NA, col4 = party, col5 = vote_count, col6 = pct_cast, col7 = pct_registered
  # (4) col1 = NA, col2 = candid, col3 = party, col4 = vote_count, col5 = pct_cast, col6 = pct_registered
  # (5) col1 = candid, col2 = NA, col3 = NA, col4 = party, col5 = vote_count, col6 = pct_cast, col7 = pct_registered
  # (6) col1 = candid, col2 = NA, col3 = party, col4 = vote_count, col5 = pct_cast, col6 = pct_registered
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
  mutate(party = col4, vote_count = str2num(col5), pct_cast = str2num(col6), pct_registered = str2num(col7)) %>% 
  select(constit, year, candid, party, vote_count, pct_cast, pct_registered)

pr08_shifted = pr08_raw2 %>% 
  filter(config %in% c(4,6)) %>% 
  mutate(candid = ifelse(config == 4, col2,
                         ifelse(config == 6, col1, NA_character_))) %>% 
  mutate(party = col3, vote_count = str2num(col4), pct_cast = str2num(col5), pct_registered = str2num(col6)) %>% 
  select(constit, year, candid, party, vote_count, pct_cast, pct_registered)

pr08 = bind_rows(pr08_shifted, pr08_unshifted)


# Pull turnout totals for constituency -------------------------------

# split into information about the totals for the constituency, and the candidate-breakdown
pr08_total_raw = pr08_raw %>% 
  filter(col1 %like% 'Totals') %>% 
  # split total column into consituency name
  separate(col1, into = c('total', 'constituency'), sep = 'Totals for') %>% 
  # a few columns have constituency shifted into the next column...
  mutate(constituency = ifelse(constituency == '' & !is.na(col2), col2, constituency)) %>% 
  select(-total, -col2, -col3, -col4, -col5, -col6)

# split into two tables: shifted, and not
tot_shifted = pr08_total_raw %>% 
  # Annoyingly, there are frame shifts within the data.  Identifying them if the last column is NA
  filter(is.na(col12)) %>% 
  mutate(rejected = str2num(col7), pct_rejected = str2num(col8), 
         cast = str2num(col9), registered = str2num(col10), turnout = str2num(col11)) %>% 
  select(constituency, year, cast, registered, rejected, pct_rejected, turnout)

tot_unshifted = pr08_total_raw %>% 
  # Annoyingly, there are frame shifts within the data.  Identifying them if the last column is NA
  filter(!is.na(col12)) %>% 
  mutate(rejected = str2num(col8), pct_rejected = str2num(col9), 
         cast = str2num(col10), registered = str2num(col11), turnout = str2num(col12)) %>% 
  select(constituency, year, cast, registered, rejected, pct_rejected, turnout)

# combine together and calculate values that aren't there
pr08_total = bind_rows(tot_shifted, tot_unshifted) %>% 
  mutate(vote_count = cast - rejected)
