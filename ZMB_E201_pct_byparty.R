
# ZMB_E201_pct_byparty ----------------------------------------------------

# Calculate national averages for pct of vote by party --------------------
# For Zambian elections between 2006-2016.
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017

# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')


maj_parties = c('PF', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')


# Presidential elections, 2006-2016. --------------------------------------

party_tot = pr_votes %>% 
  mutate(party_grp = ifelse(party %in% maj_parties, party, 'other')) %>% 
  group_by(party_grp, year, color) %>% 
  summarise(total = sum(vote_count)) %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(pct = total/sum(total),
         pct_lab = percent(pct, 0),
         party = party_grp) %>% 
  arrange(desc(total))

write_csv(party_tot, paste0(data_dir, 'ZMB_presvotes_byparty.csv'))



# Parliamentary elections, 2006-2016. -------------------------------------
parl_tot = as16 %>% 
  mutate(party_grp = ifelse(party %in% maj_parties, party, 'other')) %>% 
  group_by(party_grp, year, party) %>% 
  summarise(total = sum(vote_count)) %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(pct = total/sum(total),
         pct_lab = percent(pct, 0)) %>% 
  arrange(desc(total))

