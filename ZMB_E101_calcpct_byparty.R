
# ZMB_E201_pct_byparty ----------------------------------------------------

# Calculate national averages for pct of vote by party --------------------
# For Zambian elections between 2006-2016.
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017

# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')


maj_parties = c('PF', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')


# Presidential elections, 2006-2016. --------------------------------------

total_byparty = function(df) {
  
  df %>% 
    # lump parties getting few votes into "other" category
    mutate(party_grp = ifelse(party %in% maj_parties, party, 'other')) %>% 
    
    # group by party
    group_by(party_grp, year) %>% 
    
    # calculate total # votes by party
    summarise(total = sum(vote_count)) %>% 
    
    # calculate percent of the total, national party
    ungroup() %>% group_by(year) %>% 
    mutate(pct = total/sum(total),
           pct_lab = percent(pct, 0),
           party = party_grp) %>% 
    arrange(desc(total))
}

pres_tot = pr_votes %>% total_byparty()
# export
write_csv(pres_tot, paste0(data_dir, 'ZMB_presvotes_byparty.csv'))

# Assembly elections ------------------------------------------------------


parl_tot = as_votes %>% total_byparty()

# export
write_csv(parl_tot, paste0(data_dir, 'ZMB_assemblyvotes_byparty.csv'))




