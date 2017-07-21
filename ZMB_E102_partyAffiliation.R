
# look at changes in party affiliation between candidates -----------------

affil = as_votes %>% 
  select(candidate, year, party, constituency) %>% 
  spread(year, party) %>% 
  filter(!is.na(`2016`), !is.na(`2011`)) %>% 
  mutate(partyChg = `2016` == `2011`)

affil %>% filter(partyChg == 0) %>% count(`2011`, `2016`)

# Create a unique ID to track the 
as_affil = as_votes %>% 
  mutate(candidID = dense_rank(paste(candidate, constituency))) %>% 
  group_by(candidID) %>% 
  mutate(num_elections = n(),
         partyChg = lead(party_name) != party_name) %>% 
  fill(partyChg) %>% 
  ungroup() %>% 
  filter(num_elections > 1) %>% 
  group_by(party_name) %>% 
  mutate(party_ct = n())

pr_votes = pr_votes

affil = pr_votes %>% 
  select(candidate, year, party, constituency) %>% 
  spread(year, party) %>% 
  mutate(partyChg = `2016` == `2011`)

ggplot(as_affil, aes(x = year, y = fct_reorder(party_name, party_ct),
                     color = partyChg, group = candidID)) +
  geom_line(size = 3, alpha = 0.2) +
  scale_color_manual(values = c('lightblue', 'orange')) +
  ggtitle('Change in party between Parliamentary elections') +
  theme_minimal()
