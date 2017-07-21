
# ZMB_E201_pct_byparty ----------------------------------------------------

# Calculate national averages for pct of vote by party --------------------
# For Zambian elections between 2006-2016.
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017

# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')


maj_parties = c('PF', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')


# Presidential elections, 2006-2016. --------------------------------------

total_byparty = function(df) {
  
  # total number of votes cast. duplicated across all political parties, 
  # so selecting unique total cast per constituency and adding
  cast_total = df %>% 
    select(year, constituency, cast) %>% 
    filter(!is.na(cast)) %>% 
    distinct() %>% 
    group_by(year) %>% 
    summarise(cast_total = sum(cast))
  
  df %>% 
    # lump parties getting few votes into "other" category
    mutate(party_grp = ifelse(party %in% maj_parties, party, 'other')) %>% 
    
    # group by party
    group_by(party_grp, party_name, year, color) %>% 
    
    # calculate total # votes by party
    summarise(total = sum(vote_count)) %>% 
    
    # calculate percent of the total, national party
    ungroup() %>% 
    left_join(cast_total, by = 'year') %>% 
    group_by(year) %>% 
    mutate(pct_valid = total/sum(total),
           pct = total / cast_total,
           pct_lab = percent(pct, 0)) %>% 
    arrange(desc(total))
}

pres_tot = pr_votes %>% total_byparty()
# export
write_csv(pres_tot, paste0(data_dir, 'ZMB_presvotes_byparty.csv'))

# Assembly elections ------------------------------------------------------


parl_tot = as_votes %>% total_byparty()

as_winners = as_votes %>% 
  filter(won == 1) %>% 
  count(party_name, color, year) %>% 
  arrange(year, desc(n))

# export
write_csv(parl_tot, paste0(data_dir, 'ZMB_assemblyvotes_byparty.csv'))




