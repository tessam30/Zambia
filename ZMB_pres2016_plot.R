# ZMB_plot_2016pres
# Plot 2016 Presidential results

library(tidyverse)

# filter cutoff -----------------------------------------------------------


# import colors
parties = read_csv('~/Documents/GitHub/Zambia/party_crosswalk.csv')

# source('ZMB_elections._analysis.r')
vote_results = pr11 %>% 
  filter(!is.na(votes), total == 0) %>% 
  group_by(constituency) %>% 
mutate(vote_pct = votes/sum(votes))
  
pct_breaks = c(seq(-5, 15, by = 5), seq(20, 100, by = 20))

vote_results = vote_results %>%
  mutate(vote_cat = cut(vote_pct, breaks = pct_breaks),
         party = str_to_upper(Party))

vote_results = vote_results %>% left_join(parties, by = c('party' = 'pres16'))

pres = left_join(zmb16, vote_results, by = c("website2016" = 'constName'))

library(RColorBrewer)


order = vote_results %>% group_by(party) %>% 
  summarise(tot=sum(vote_count)) %>% 
  arrange(desc(tot)) %>% ungroup() %>% 
  mutate(pct = tot/sum(tot))

pres$party = factor(pres$party, order$party)

p = ggplot(pres, aes(fill = color, alpha = vote_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_identity() +
  facet_wrap(~party) +
  theme_void() +
  theme(legend.position = 'none')


ggplot(vote_results %>% filter(vote_pct > 1), aes(x = vote_pct)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

ggsave('ZMB_pres2016_party.pdf')


# plot legend -------------------------------------------------------------
colors = parties %>% select(party, color) %>% distinct() 

lgnd = data.frame(color = rep(colors$color, length(pct_breaks)),
                  party = rep(colors$party, length(pct_breaks)),
                  pct = rep(pct_breaks, each = nrow(colors)),
                  x = rep(1:length(pct_breaks), each = nrow(colors))) %>% 
  arrange(party, pct) %>% 
  mutate(vote_cat = cut(pct, breaks = pct_breaks)) %>% 
  # mutate(label = ifelse(pct == -5, 0, paste0(pct, ' - ', lead(pct)))) %>% 
  filter(!is.na(vote_cat))

  
ggplot(lgnd, aes(x = vote_cat, y = 1, label = pct,
                 fill = color, alpha = vote_cat)) +
  geom_tile(color = '#333333', size = 0.1) +
  geom_text(nudge_x = 0.5, nudge_y = -0.65, vjust = 1, colour = '#333333', alpha = 1) + 
  scale_y_continuous(expand = c(0.25, 0.2)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_fill_identity() +
  facet_wrap(~party, ncol = 2) +
  theme_void() +
  # coord_equal() +
  theme(legend.position = 'none')
