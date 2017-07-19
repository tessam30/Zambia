
# Import and clean the 2011 Zambia Assembly Election results -----------
# At the constituency level
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 6 July 2017


# KNOWN ISSUES ------------------------------------------------------------
# 1. 2 constituencies are missing data; assume they didn't have elections in 2011
# 2. Chawama candidate MUSONDAMWAUME Borniface K lacks info on party affiliation
# 3. Nalikawana candidate LUNGWANGWA Lungwangwa G lacks party affiliation.  
#       Initally assumed to be UPND: https://www.elections.org.zm/candidates/2016_national_assembly_elections/candidate/lungwangwa,lungwangwa,upnd
#       ... but there's already a UPND candidate that year

# setup -------------------------------------------------------------------
# taken care of in ZMB_E01_helpers.R
# library(readxl)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(readr)
# 
# setwd('~/Documents/GitHub/Zambia/')

# import data -------------------------------------------------------------
# data pulled from http://www.elections.org.zm/media/28092011_2011_national_assembly_elections_results.pdf
# on 29 June 2017
# Extracted from pdf using Tabula (http://tabula.technology/) to begin the cleaning process

# Goal is two-fold:
# 1) Pull the breakdown of votes by candidate / constituency
# 2) Pull the turnout numbers by constituency
# 3) Clean and calculate basic stats

as11_raw = read_csv('rawdata/tabula-2011_national_assembly_elections_results.csv')

# glance at structure
# glimpse(as11_raw)

# check if columns are necessary
# t(as11_raw %>% summarise_all(funs(sum(!is.na(.)))))

# So, annoyingly, the data is structured as such:
# candidate_region contains an initial row specifying the constituency name
# then are the constituency vote totals by party
# followed by the constituency summary totals

as11 = as11_raw %>% 
  rename(vote_count = Votes) %>% 
  mutate(
    # constituencies are all labeled by numbers (yay order);
    # province totals specified by "Province" (though tried to remove all of them so they don't muck things up)
    constit = ifelse(str_detect(candidate_region, '[0-9]') | str_detect(candidate_region, 'Province'), 
                     candidate_region, NA),
    # constituency totals have NA in the candidate regions
    total = ifelse(is.na(candidate_region), 1, 0),
    # convert votes to numeric
    year = 2011,
    
    # create copy of candidate_region and a flag for if it's a candidate row
    # candidate names and their parties are separated by a comma
    # NOTE: 2 candidates have names that are too long
    isCandidate = ifelse(str_detect(candidate_region, '\\,') |
                           str_detect(candidate_region, 'MUSONDAMWAUME Borniface K') |
                           str_detect(candidate_region, 'LUNGWANGWA Lungwangwa G'), 1, 0),
    candidate = candidate_region
  ) %>% 
  # fill down constituency names
  fill(constit) %>% 
  # split constit into id and name
  separate(constit, into = c('constit_id', 'constit1', 'constit2'), sep = ' ') %>%
  # split candidate_region into candidate name and party affiliation
  separate(candidate, into = c('candidate', 'party'), sep = '\\,') %>% 
  # split name into first, middle, last name
  split_candid('candidate') %>% 
  
  mutate(
    # make constit purty
    website2011 = ifelse(is.na(constit2), str_to_title(constit1),
                         str_to_title(paste(constit1, constit2))),
    # replace non-candidate names w/ NAs
    first_name = ifelse(isCandidate, first_name, NA),
    last_name = ifelse(isCandidate, last_name, NA),
    candidate = ifelse(isCandidate, candidate, NA),
    
    # convert strings to numbers
    rejected = str2num(rejected_ballotpapers),
    cast = str2num(total_votes),
    registered = str2num(total_registered),
    pct_cast_web = str2pct(pct_cast_web),
    pct_registered_web = str2pct(pct_registered_web),
    pct_rejected_web = str2pct(pct_rejected_web),
    turnout_web = str2pct(pct_poll)
  ) %>% 
  # merge geo
  left_join(geo_base %>% select(constituency, province2006, district2006, website2011), by = 'website2011') %>% 
  rename(province = province2006, district = district2006)




# totals at the constituency level; also includes number of rejected votes
as11_total = as11 %>% 
  filter(total == 1,
         # remove extra text taken along for the ride
         !is.na(vote_count)
  ) %>% 
  calc_turnout()

# After check by eye, pct_rejected calc looks on target with what Zambia reports; dropping their formatted number
# pct_poll is equivalent to turnout.




# get individual data -----------------------------------------------------
as11 = as11 %>% 
  filter(isCandidate == 1) %>% 
  # remove cols that are only available in the turnout, sumamry lines
  select(-cast, -registered, -rejected, -turnout_web, -total_registered, -pct_rejected_web) %>% 
  merge_turnout(as11_total) %>% 
  calc_stats()


# checks ------------------------------------------------------------------
# Note: Nakonde and Magoye missing: no parliamentary elections that year?
if(nrow(as11_total) != 150) {
  warning('check if districts are missing')
}

check_pct(as11)

# check website totals agree
# Note: no percentages were calculated in the candidate-level breakdown
check_turnout(as11_total)

# collapse votes down to constituency level -------------------------------
# Check to make sure that the constitency-level individual vote tallies (in `votes`) 
# match with the summaries provided in `registered`

check_constit(as11, as11_total)


# filter out just the good stuff ------------------------------------------
as11 = filter_candid(as11)
as11_total = filter_turnout(as11_total)

rm(as11_raw)

