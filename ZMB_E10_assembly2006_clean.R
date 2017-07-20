
# import Zambia 2006 NATIONAL ASSEMBLY ELECTIONS --------------------------
# Scrape out two pieces of info from the consituency elections from 2006:
# https://www.elections.org.zm/media/parliamentary_results_2006.pdf
# 1. overall poll stats (how many people voted, registered)
# 2. candidate/election results


# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017


# known errors ------------------------------------------------------------
# * Totals from Nakonde constituency are screwed up. Total counted by constituency breakdown is greater than the turnout
#   line item from the constituency total.  Crossreferencing with pdf shows that the constituency total does not include
#   the winner of the constituency, so it's short 11,935 votes.
#   Assume: number of rejected ballots and registered is correct.
#   Assume: cast = cast + 11935; vote_count = vote_count + 11935
#   ** MANUALLY CORRECTED **
# * Ignores data from Lupososhi and Kabompo East constituencies; both had elections delayed till 2006-10-26 b/c of deaths and aren't reported.


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


# [0] — get the data frame into a workable format -------------------------

# unfortunately, there are some frame shifts that make things a bit onboxious.
# luckily, there are only two possible frame shifts. 
# 1) chunk where col1 = prov, col2 = dist, col3 = constit, col4 = candid, col5 = party, 
#                col6 = vote_count, col7 = rejected, col8 = cast, col9 = registered, col10 = NA
# 2) chunk where col1 = NA, col2 = prov, col3 = dist, col4 = constit, col5 = candid, col6 = party, 
#                col7 = vote_count, col8 = rejected, col9 = cast, col10 = registered

# Approach:
# 1. filter out the column labels: col1 | col 2 = 'PROVINCE'; everything is NA; col8 | col9 = 'CAST', col9 != 'TOTAL'
# 2. Find where the constituency is: col3 or col4 (which is the first row in a chunk)
# 3. Fill down the shiftFlag down the rest of the chunk, till you hit the next constituency
# 4. Create new cols where the data are consistent.
# 5. Fill down prov, dist, constit
# 6. Clean up strings: prov, dist, constit, candidates
# 7. Convert strings to numbers: vote_count, rejected, registered, cast
# 8. Merging to geo_base not required, since already have prov + dist
# 9. Tag as total and separate totals by candidate and totals by constituency
# 10. calc pcts.

as06 = as06_raw %>% 
  # Get rid of some of the goo
  filter(is.na(col1) | col1 != 'PROVINCE', 
         is.na(col2) | col2 != 'PROVINCE' , 
         is.na(col8) | col8 != 'CAST', 
         is.na(col9) | (col9 != 'CAST' & col9 != 'TOTAL')) %>% 
  # identify if the frame is shifted or not: constituencies contain numbers, while candidates and district names do not.
  mutate(shiftFlag = ifelse(str_detect(col3, '[0-9]'), 0,
                            ifelse(str_detect(col4, '[0-9]'), 1, NA))) %>% 
  # fill down the shiftFlag
  fill(shiftFlag) %>% 
  # create common frame
  # also make strings pretty, numbers numbers
  #                col6 = vote_count, col7 = rejected, col8 = cast, col9 = registered, col10 = NA
  mutate(year = 2006, 
         province = ifelse(shiftFlag == 0, pretty_strings(col1), pretty_strings(col2)),
         district = ifelse(shiftFlag == 0, pretty_strings(col2), pretty_strings(col3)),
         constituency = ifelse(shiftFlag == 0, pretty_strings(str_replace_all(col3, '[0-9]', '')), 
                               pretty_strings(str_replace_all(col4, '[0-9]', ''))),
         # fix Shiwang'andu's obno curly quote
         constituency = ifelse(constituency %like% 'Shiwang', "Shiwang'andu", constituency),
         candid = ifelse(shiftFlag == 0, col4, col5),
         isCandidate = ifelse(is.na(candid), 0, 1), # tag value as being total or candidate
         party = ifelse(shiftFlag == 0, col5, col6),
         vote_count = ifelse(shiftFlag == 0, str2num(col6), str2num(col7)),
         rejected =  ifelse(shiftFlag == 0, str2num(col7), str2num(col8)),
         cast = ifelse(shiftFlag == 0, str2num(col8), str2num(col9)),
         registered = ifelse(shiftFlag == 0, str2num(col9), str2num(col10))
  ) %>% 
  # fill down provinces, districts, constituencies
  fill(province) %>% 
  fill(district) %>% 
  fill(constituency) %>% 
  # split apart candidate names
  split_candid('candid') %>% 
  # filter out the last of the rando data
  filter(!is.na(vote_count)) 

# Side note: count unique constituencies that held elections.
# Verified that there should be all 150 constituencies.
allCons = data.frame(constit = c(as06_raw$col3, as06_raw$col4)) %>% 
  mutate(isC = ifelse(str_detect(constit, '[0-9]'),1, 0)) %>% 
  filter(isC == 1) %>% 
  separate(constit, into = c('constit_id', 'constit1', 'constit2'), sep = ' ', remove = FALSE) %>% 
  arrange(constit_id)

# [1] clean turnout numbers ---------------------------------------------------

as06_total = as06 %>% 
  filter(isCandidate == 0) %>% 
  # MANUALLY FIX NAKONDE CONSTITUENCY
  # In initial checks, the constituency vote counts don't agree.
  # Detective work shows they omitted the winning candidate from the vote_count, cast numbers
  #   Assume: number of rejected ballots and registered is correct.
  #   Assume: cast = cast + 11935; vote_count = vote_count + 11935
  mutate(cast = ifelse(constituency == 'Nakonde', cast + 11935, cast),
         vote_count = ifelse(constituency == 'Nakonde', vote_count + 11935, vote_count)) %>% 
  calc_turnout()

# [2] CALC VOTES PCT BY constit -------------------------------------------


# clean votes
as06_ =  as06 %>% 
  filter(isCandidate == 1) %>% 
  # remove unnecessary cols
  select(-rejected, -cast, -registered) %>% 
  # calc percentages
  calc_stats() %>% 
  # calc pct_turnout
  merge_turnout(as06_total)




# checks ------------------------------------------------------------------
# Note: no percentages were calculated in any breakdowns, so can't check my calcs.

# collapse votes down to constituency level -------------------------------
# Check to make sure that the constitency-level individual vote tallies (in `votes`) 
# match with the summaries provided in `registered`

check_constit(as06_, as06_total)


# filter out just the good stuff ------------------------------------------
as06_ = filter_candid(as06_)
as06_total = filter_turnout(as06_total)

rm(as06_raw)
