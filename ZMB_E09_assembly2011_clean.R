
# Import and clean the 2011 Zambia Assembly Election results -----------
# At the constituency level
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 6 July 2017


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
    isCandidate = ifelse(str_detect(candidate_region, '\\,'), 1, 0),
    candidate = candidate_region
  ) %>% 
  # fill down constituency names
  fill(constit) %>% 
  # split constit into id and name
  separate(constit, into = c('constit_id', 'constit1', 'constit2'), sep = ' ') %>%
  # split candidate_region into candidate name and party affiliation
  separate(candidate, into = c('candidate', 'Party'), sep = '\\,') %>% 
  # split name into first, middle, last name
  separate(candidate, into = c('last_name', 'first_name', 'middle_name'), sep = ' ') %>% 

  mutate(
    # make constit purty
    constituency = ifelse(is.na(constit2), str_to_title(constit1),
                               str_to_title(paste(constit1, constit2))),
    # make names pretty
    first_name = ifelse(isCandidate, str_trim(str_to_title(paste(first_name, middle_name))), NA),
    last_name = ifelse(isCandidate, str_trim(str_to_title(last_name)), NA),
    candidate = ifelse(isCandidate, paste(first_name, last_name), NA)
    ) %>% 
  select(-middle_name)


# check import looks right ------------------------------------------------
# should all have 12 
View(as11 %>% count(constit_id))

# totals at the constituency level; also includes number of rejected votes
as11_total = as11 %>% 
  filter(total == 1,
         # remove extra text taken along for the ride
         !is.na(votes)
         ) %>% 
    mutate(rejected = as.numeric(str_replace_all(rejected_ballotpapers, ',', '')),
           cast = as.numeric(str_replace_all(total_votes, ',', '')),
           registered = as.numeric(str_replace_all(total_registered, ',', '')),
           pct_rejected_lab = pct_rejected,
           pct_rejected = rejected/cast,
           turnout = cast / registered,
           valid_turnout = votes / registered
    )

# After check by eye, pct_rejected calc looks on target with what Zambia reports; dropping their formatted number
# pct_poll is equivalent to turnout.
  select(constit_id, constituency, year, registered, cast, votes,
         rejected, pct_rejected, turnout, valid_turnout)
  
  
  
  
# get individual data -----------------------------------------------------
as11_indiv = as11 %>% 
    filter(isCandidate == 1) %>% 
    select(constit_id, constituency, year, 
           candidate, first_name, last_name, Party,
           vote_count, contains('pct'),
           -pct_poll, -pct_rejected) %>% 
    group_by(constituency) %>% 
    mutate(pct_votes = vote_count / sum(vote_count),
           rank = min_rank(desc(vote_count)),
           won = ifelse(rank == 1, 1, 0),
           margin_victory = ifelse(rank == 1, vote_count - lead(vote_count), NA),
           pct_margin = ifelse(rank == 1, pct_votes - lead(pct_votes), NA)) %>% 
    # fill margin for the entire consituency
    fill(pct_margin, margin_victory)
