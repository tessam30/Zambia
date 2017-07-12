
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
    vote_count = as.numeric(str_replace_all(Votes, ',', '')),
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
         vote_count, contains('pct'),
         -pct_poll, -pct_rejected) %>% 
  group_by(constituency) %>% 
  mutate(pct_votes = vote_count / sum(vote_count),
         # convert to number, percent
         pct_cast = as.numeric(str_replace_all(pct_cast, '%', ''))/100,
         rank = min_rank(desc(vote_count)),
         won = ifelse(rank == 1, 1, 0),
         margin_victory = ifelse(rank == 1, vote_count - lead(vote_count), NA),
         pct_margin = ifelse(rank == 1, pct_votes - lead(pct_votes), NA)) %>% 
  # fill margin for the entire consituency
  fill(pct_margin, margin_victory) %>% 
  ungroup() %>% 
  group_by(party) %>% 
  mutate(natl_votes = sum(vote_count)) %>% 
  ungroup() %>% 
  mutate(party_natl_pct = natl_votes/sum(vote_count))



#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt


# merge w/ lookup table ---------------------------------------------------
# Creates a crosswalk between the shapefile names and those used on election website (with vote count)
# Also connects the 150 constituencies from pre-2016 to the 156 afterwards.
# Note that while the names may be the same, the boundaries have shifted and in some cases are quite different.

geo_base = read_excel(paste0(base_dir, 'ZMB_admin_crosswalk.xlsx'), sheet = 2)

pr11 = left_join(pr11, geo_base, by = c("constituency" = "website2011"))

# pull out just the relevant vars -----------------------------------------

pr11 = pr11 %>% 
  select(year, province, district, constituency, 
         party, candidate, first_name, last_name,
         vote_count, rank, won, 
         pct_cast, pct_votes, margin_victory, pct_margin, party_natl_pct)


# (2) Voting turnout by constituency --------------------------------------

# totals at the constituency level; also includes number of rejected votes
pr11_total = pr11_all %>% 
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
  