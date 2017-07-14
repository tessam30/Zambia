
# ZMB_E201_pct_byparty ----------------------------------------------------

maj_parties = c('PF', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')

party_tot = pr_votes %>% 
  mutate(party_grp = ifelse(party %in% maj_parties, party, 'other')) %>% 
  group_by(party_grp, year, color) %>% 
  summarise(total = sum(vote_count)) %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(pct = total/sum(total),
         pct_lab = percent(pct, 0),
         party = party_grp) %>% 
  arrange(desc(total))
