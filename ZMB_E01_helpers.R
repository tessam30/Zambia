
# Helper functions to clean up Zambia election data -----------------------
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 12 July 2017


# load requisite packages -------------------------------------------------
library(data.table) # version 1.10.4
library(dplyr) # version 0.5.0
library(tidyr) # version 0.6.3
library(sf) # version 0.5-1
library(ggplot2) # 2.2.1.9000 dev version -- required for sf
library(forcats) # version 0.2.0
library(stringr) # version 1.2.0
library(readr) # version 1.1.1
library(readxl) # version 1.0.0
library(rvest) # version 0.3.2


# convert strings to numbers ----------------------------------------------
str2num = function(value){
  as.numeric(str_replace_all(value, ',', ''))
}


# convert percents to percent ---------------------------------------------

str2pct = function(value){
  as.numeric(str_replace_all(value, '%', ''))/100
}

# summarise and calc pct of total votes -----------------------------------
calc_pctVotes = function(df, group1 = 'party', group2 = 'year') {
  summ = df %>% 
    group_by_(group1, group2) %>% 
    summarise(tot_votes = sum(vote_count)) %>% 
    ungroup() %>% 
    group_by_(group2) %>%
    mutate(pct = tot_votes / sum(tot_votes)) %>% 
    arrange(desc(pct))
  
  return(summ)
}


# convert any strings to nice title case sans spaces ----------------------
pretty_strings = function(string) {
  # df = df %>% 
  #   mutate_(.dots = setNames(list(paste0('stringr::str_to_title(stringr::str_trim(', column, "))")), column))
  # 
  # return(df)
  
  stringr::str_to_title(stringr::str_trim(string))
}



# split candidate name into first name, last name, middle, and rejoin --------
# candidate name given LAST FIRST MIDDLE
split_candid = function(df, column = 'candid', sep = ' ') {
  df %>% 
    separate_(col = column, into = c('last_name', 'first_name', 'middle_name'), sep = sep) %>% 
    mutate(first_name = ifelse(is.na(middle_name), pretty_strings(first_name),
                               paste(pretty_strings(first_name), pretty_strings(middle_name))),
           last_name = pretty_strings(last_name),
           candidate = paste(first_name, last_name)) %>% 
    select(-middle_name)
}

# merge w/ lookup table ---------------------------------------------------
# Creates a crosswalk between the shapefile names and those used on election website (with vote count)
# Also connects the 150 constituencies from pre-2016 to the 156 afterwards.
# Note that while the names may be the same, the boundaries have shifted and in some cases are quite different.

geo_base = read_excel(paste0(base_dir, 'ZMB_admin_crosswalk.xlsx'), sheet = 2, na = 'NA')

# NOTE: SOOOOO province, district names are not consistent.  
# Within each dataset, "province" and "district" are the province/district that's either given by the data (not the 2016 values),
# or the closest value to the year.

# Calc individual election stats ------------------------------------------

calc_stats = function(df) {
  df %>%  
    group_by(constituency) %>% 
    mutate(
      pct_votes = vote_count / sum(vote_count),
      rank = min_rank(desc(vote_count)),
      won = ifelse(rank == 1, 1, 0),
      margin_victory = ifelse(rank == 1, vote_count - lead(vote_count), NA),
      pct_margin = ifelse(rank == 1, pct_votes - lead(pct_votes), NA)) %>% 
    # fill margin for the entire constituency
    fill(pct_margin, margin_victory)  %>% 
    ungroup() %>% 
    group_by(party) %>% 
    mutate(natl_votes = sum(vote_count)) %>% 
    ungroup() %>% 
    mutate(party_natl_pct = natl_votes/sum(vote_count))
}


# Calc turnout  -----------------------------------------------------------

calc_turnout = function(df){
  df %>% 
    mutate(pct_rejected = rejected / cast,
           turnout = cast / registered,
           vote_count = cast - rejected,
           valid_turnout = vote_count / registered
    )
  
  
}

# merge turnout and candidate totals --------------------------------------
merge_turnout = function(candid_df, turnout_df) {
  candid_df %>% 
    left_join(turnout_df %>% select(province, contains('district'), 
                                    contains('website'),
                                    contains('shp'),
                                    contains('constituency'), cast, registered), by = 'constituency') %>% 
    mutate(pct_cast = vote_count / cast,
           pct_registered = vote_count / registered)
}


# verification ------------------------------------------------------------
# Check my calcs of turnout jibe with those on website or in pdf
check_turnout = function(df, ndigits = 2) {
  errors = df %>% 
    mutate(turnout_ok = round(turnout_web, ndigits) == round(turnout, ndigits),
           rejected_ok = round(pct_rejected_web, ndigits) == round(pct_rejected, ndigits)) %>% 
    filter(turnout_ok == FALSE | rejected_ok == FALSE) %>% 
    select(constituency, turnout_web, turnout, pct_rejected_web, pct_rejected)
  
  if(nrow(errors) > 0) {
    warning('Website numbers do not agree with calculation')
    return(errors)
  }
}

# Check my calcs of pct_cast jibe with those on website or in pdf
check_pct = function(df, ndigits = 2) {
  errors = df %>% 
    mutate(pct_cast_round = round(pct_cast, ndigits),
           pct_cast_round_web = round(pct_cast_web, ndigits),
           cast_ok = pct_cast_round_web == pct_cast_round,
           pct_reg_round =  round(pct_registered, ndigits),
           pct_reg_round_web = round(pct_registered_web, ndigits),
           registered_ok = pct_reg_round_web == pct_reg_round) %>% 
    filter(cast_ok == FALSE | registered_ok == FALSE)
  
  if(nrow(errors) > 0) {
    warning('Website numbers do not agree with calculation')
    return(errors %>% select(constituency, candidate, pct_cast_web, pct_cast, pct_cast_round_web, pct_cast_round, 
                             pct_registered_web, pct_registered, pct_reg_round_web, pct_reg_round))
  }
}

# Check constituency totals match
check_constit = function(candid_df, turnout_df) {
  candid_sum = candid_df %>% 
    ungroup() %>% 
    group_by(constituency) %>% 
    summarise(candid_count = sum(vote_count))
  
  comb = full_join(candid_sum, turnout_df, by = 'constituency') %>% 
    mutate(tot_ok = candid_count == vote_count) %>% 
    filter(tot_ok == FALSE)
  
  if(nrow(comb) > 0) {
    warning('Candidate summary numbers and turnout do not agree')
    return(comb)
  }
  
}

# Filter out just the good stuff (to standardize)
# for the candidate totals by constituency
filter_candid = function(df){
  df %>% select(
    province, district, constituency, contains('website'),
    year, party, candidate, first_name, last_name,
    vote_count, cast, registered,
    pct_cast, pct_votes, 
    rank, won, 
    margin_victory, pct_margin, party_natl_pct
  )
}

# for the turnout numbers by constituency
filter_turnout = function(df){
  df %>% select(
    province, district, constituency, contains('website'),
    year,
    vote_count, cast, registered,
    rejected, pct_rejected,
    turnout, valid_turnout
  )
}

