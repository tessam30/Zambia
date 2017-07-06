
# Import and clean the 2011 Zambia Presidental Election results -----------
# At the constituency level
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)


# import data -------------------------------------------------------------
# data pulled from http://www.elections.org.zm/media/28092011_public_notice_-_2011_presidential_election_results.pdf
# on 29 June 2017
# Extracted from pdf using Tabula (http://tabula.technology/) to begin the cleaning process

pr11_raw = read_csv('rawdata/tabula-2011_presidential_election_results.csv')

# glance at structure
glimpse(pr11_raw)

# check if columns are necessary
t(pr11_raw %>% summarise_all(funs(sum(!is.na(.)))))

# So, annoyingly, the data is structured as such:
# candidate_region contains an initial row specifying the constituency name
# then are the constituency vote totals by party
# followed by the constituency summary totals

pr11 = pr11_raw %>% 
  mutate(
    # constituencies are all labeled by numbers (yay order);
    # province totals specified by "Province" (though tried to remove all of them so they don't muck things up)
    constit = ifelse(str_detect(candidate_region, '[0-9]') | str_detect(candidate_region, 'Province'), candidate_region, NA),
    # constituency totals have NA in the candidate regions
    total = ifelse(is.na(candidate_region), 1, 0),
    # convert votes to numeric
    votes = as.numeric(str_replace_all(Votes, ',', '')),
    year = 2011
  ) %>% 
  # fill down constituency names
  fill(constit) %>% 
  # split constit into id and name
  separate(constit, into = c('constit_id', 'constit1', 'constit2'), sep = ' ') %>%
  # make constit purty
  mutate(constituency = ifelse(is.na(constit2), str_to_title(constit1),
                               str_to_title(paste(constit1, constit2))))


# check import looks right ------------------------------------------------
# should all have 12 
View(pr11 %>% count(constit_id))

# totals at the constituency level; also includes number of rejected votes
pr11_total = pr11 %>% 
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
  