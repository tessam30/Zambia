
maj_parties = c('PF', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')

party_tot = pr_votes %>% 
  mutate(party_grp = ifelse(party %in% maj_parties, party, 'other')) %>% 
  group_by(party_grp, year) %>% 
  summarise(total = sum(vote_count)) %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(pct = total/sum(total)) %>% 
  arrange(desc(total))

party_order = unique(party_tot$party_grp)

party_tot$party_grp = factor(party_tot$party_grp, levels = party_order)


ggplot(party_tot, aes(x = year, y = pct, fill = color)) +
  geom_area(stat = 'identity', alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_identity() +
  # facet_wrap(~party_grp) +
  theme_bw()

ggplot(party_tot, aes(x = year, y = pct, fill = party_grp)) +
  geom_bar(stat = 'identity', alpha = 1) +
  scale_y_continuous(labels = scales::percent) + 
  theme_bw()

