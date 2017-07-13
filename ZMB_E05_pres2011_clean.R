
# Import and clean the 2011 Zambia Presidental Election results -----------
# At the constituency level
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter


# setup -------------------------------------------------------------------

# taken care of in ZMB_E01_helpers.R
# library(readxl)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(readr)
# 
# base_dir = '~/Documents/GitHub/Zambia/'

# goals -------------------------------------------------------------------
# (0) Import the data
# (1) Calculate the vote totals for each candidate by constituency
# (2) Calculate voting turnout per constituency

# import data -------------------------------------------------------------
# data pulled from http://www.elections.org.zm/media/28092011_public_notice_-_2011_presidential_election_results.pdf
# on 29 June 2017
# Extracted from pdf using Tabula (http://tabula.technology/) to begin the cleaning process

pr11_raw = read_csv('rawdata/tabula-2011_presidential_election_results.csv')

# glance at structure
glimpse(pr11_raw)

# check if columns are necessary
t(pr11_raw %>% summarise_all(funs(sum(!is.na(.)))))



# (0) import and prelim clean ---------------------------------------------

# So, annoyingly, the data is structured as such:
# candidate_region contains an initial row specifying the constituency name
# then are the constituency vote totals by party
# followed by the constituency summary totals

pr11_all = pr11_raw %>% 
  mutate(
    # constituencies are all labeled by numbers (yay order);
    # province totals specified by "Province" (though tried to remove all of them so they don't muck things up)
    constit = ifelse(str_detect(candidate_region, '[0-9]') | str_detect(candidate_region, 'Province'), candidate_region, NA),
    # constituency totals have NA in the candidate regions
    total = ifelse(is.na(candidate_region), 1, 0),
    # convert votes to numeric
    vote_count = str2num(Votes),
    year = 2011,
    
    # create copy of candidate_region and a flag for if it's a candidate row
    # candidate names and their parties are separated by a comma
    isCandidate = ifelse(str_detect(candidate_region, '\\,'), 1, 0)
  ) %>% 
  # fill down constituency names
  fill(constit) %>% 
  
  # split constit into id and name
  separate(constit, into = c('constit_id', 'constit1', 'constit2'), sep = ' ') %>%
  # split candidate_region into candidate name and party affiliation
  separate(candidate_region, into = c('candidate', 'party'), sep = '\\,', remove = FALSE) %>% 
  # split name into first, middle, last name
  separate(candidate, into = c('last_name', 'first_name', 'middle_name'), sep = ' ') %>% 
  
  # make constit purty
  mutate(constituency = ifelse(is.na(constit2), str_to_title(constit1),
                               str_to_title(paste(constit1, constit2))),
         # make names pretty
         first_name = ifelse(isCandidate, str_trim(str_to_title(paste(first_name, middle_name))), NA),
         last_name = ifelse(isCandidate, str_trim(str_to_title(last_name)), NA),
         candidate = ifelse(isCandidate, paste(first_name, last_name), NA)
  ) %>% 
  select(-middle_name)


# check import looks right ------------------------------------------------
# should all have 12 
View(pr11_all %>% count(constit_id))



# # (1) Votes per candidate -----------------------------------------------
pr11 = pr11_all %>% 
  filter(isCandidate == 1) %>% 
  select(constit_id, constituency, year, 
         candidate, first_name, last_name, party,
         vote_count, contains('pct')) %>% 
  mutate(
    # convert to number, percent
    pct_cast_web = str2pct(pct_cast_web),
    pct_registered_web = str2pct(pct_registered_web)) %>% 
  calc_stats()


#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt



# (2) Voting turnout by constituency --------------------------------------

# totals at the constituency level; also includes number of rejected votes
pr11_total = pr11_all %>% 
  filter(total == 1,
         # remove extra text taken along for the ride
         !is.na(vote_count)
  ) %>% 
  mutate(
    # convert to number
    rejected = str2num(rejected_ballotpapers),
    cast = str2num(total_votes),
    registered = str2num(total_registered),
    # convert to number, percent
    pct_cast_web = str2pct(pct_cast_web),
    pct_registered_web = str2pct(pct_registered_web),
    turnout_web = str2pct(turnout_web),
    pct_rejected_web = str2pct(pct_rejected_web)
  ) %>% 
  calc_turnout()

# merge geo
pr11_total = pr11_total %>% right_join(geo_base, by = c('constituency' = 'website2011'))

# After check by eye, pct_rejected calc looks on target with what Zambia reports; dropping their formatted number
# pct_poll is equivalent to turnout.


# finish pr11 calcs -------------------------------------------------------

pr11 = pr11 %>% merge_turnout(pr11_total)


# run checks --------------------------------------------------------------

check_pct(pr11)

check_turnout(pr11_total)
check_constit(pr11, pr11_total)


# cleanup -----------------------------------------------------------------
rm(pr11_all, pr11_raw)

pr11 = filter_candid(pr11)
pr11_total = filter_turnout(pr11_total)
