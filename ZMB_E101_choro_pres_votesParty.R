# Plot Zambian election data, by party and constituency ---------------------------------
# In previous files, election data from Zambian elections were imported
# cleaned, and merged.
# This file plots choropleths 
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017


# setup -------------------------------------------------------------------
library(tidyverse)
library(forcats)

export_dir = '~/Documents/GitHub/Zambia/exported_fromR/'

width = 7
height = 7

# import colors
parties = read_csv('~/Documents/GitHub/Zambia/party_crosswalk.csv')

# choropleth breaks
pct_breaks = c(seq(-5, 15, by = 5), seq(20, 100, by = 20))/100


# import data -------------------------------------------------------------
source('ZMB_pres_mergeAll.R')


# filter just parties with larger vote counts -----------------------------------------------------------

# order for plotting the parties
party_order = c('PF', 'UPND', 'MMD')
# , 'FDD', 'ADD', 'UNIP')

plot_votes = function(geo_df, sel_year, order = party_order) {
  
  # geodata frame, complete with voting results by party/constituency merged in
  geo_df = geo_df %>%
    filter(year == sel_year,
           party %in% order) %>% 
    mutate(vote_cat = cut(pct_votes, breaks = pct_breaks)) %>% 
    left_join(parties, by = c('party' = paste0('pres', str_sub(sel_year, 3))))
  
  geo_df$party = forcats::fct_relevel(geo_df$party, order)
  
  p = ggplot(geo_df, aes(fill = color, alpha = vote_cat)) +
    geom_sf(size = 0.1) +
    scale_fill_identity() +
    facet_wrap(~party, ncol = 2, nrow = 2) +
    theme_void() +
    theme(legend.position = 'none')
  
  
  
  ggsave(paste0(export_dir, 'ZMB_pres', sel_year, '_party.pdf'))
}


# plot for each year ------------------------------------------------------


plot_votes(zmb16, 2016)
plot_votes(zmb15, 2015)
plot_votes(zmb11, 2011)
plot_votes(zmb08, 2008)
plot_votes(zmb06, 2006)

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


ggplot(lgnd, aes(x = vote_cat, y = 1, label = pct*100,
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
