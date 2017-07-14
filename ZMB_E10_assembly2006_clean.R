
# import Zambia 2006 NATIONAL ASSEMBLY ELECTIONS --------------------------
# Scrape out two pieces of info from the consituency elections from 2006:
# https://www.elections.org.zm/media/parliamentary_results_2006.pdf
# 1. overall poll stats (how many people voted, registered)
# 2. candidate/election results


# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017


# setup -------------------------------------------------------------------
# # taken care of in ZMB_E01_helpers.R
# library(rvest)
# library(stringr)
# library(dplyr)
# library(tidyr)


# goals -------------------------------------------------------------------
# [0] Convert pdf to something usable using Tabula; scrape assembly data from Excel workbook
# [1] Pull the turnout by constituency --> as06_total
# [2] Pull out the votes by candidate by constituency --> as06




# Import data ---------------------------------------------------
as06_raw = read_csv(paste0(base_dir, 'rawdata/tabula-2006_parliamentary_results.csv'))

# unfortunately, there are some frame shifts that make things a bit onboxious.

# [1] clean turnout numbers ---------------------------------------------------



# [2] CALC VOTES PCT BY constit -------------------------------------------


# clean votes
as16 =  as16_raw %>% 
  split_candid('Candidate Name', sep = ', ') %>% 
  # merge votes w/ district and province name
  left_join(dists, by = "constituency") %>% 
  rename(party = Party) %>% 
  mutate(year = 2016,
         vote_count = str2num(Votes)) %>% 
  # calc percentages
  calc_stats() %>% 
  # calc pct_turnout
  merge_turnout(as16_total)



  if(sum(is.na(as16$district)) > 0) {
    warning('merge of district names w/ registered votes did not work')
  }



# checks ------------------------------------------------------------------
# check website totals agree
# Note: no percentages were calculated in the candidate-level breakdown
check_turnout(as16_total, incl_rejected = FALSE)

# collapse votes down to constituency level -------------------------------
# Check to make sure that the constitency-level individual vote tallies (in `votes`) 
# match with the summaries provided in `registered`

check_constit(as16, as16_total)


# filter out just the good stuff ------------------------------------------
as16 = filter_candid(as16)
as16_total = filter_turnout(as16_total)

rm(vote, reg, dists, constit, as16_total_raw, as16_raw)




