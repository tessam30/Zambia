# ZMB_plot_2016pres
# Plot 2016 Presidential results

# source('ZMB_elections._analysis.r')

pct_breaks = c(seq(-5, 15, by = 5), seq(20, 100, by = 20))

vote_results = vote_results %>% 
  mutate(vote_cat = cut(vote_pct, breaks = pct_breaks))

pres = left_join(geo, vote_results, by = c('constituency' = 'constName'))

library(RColorBrewer)

p = ggplot(pres, aes(x = long, y = lat, fill = vote_cat, group = group, order = order)) +
  geom_polygon() +
  geom_path(size = 0.1, color = 'white') +
  scale_fill_brewer(palette = 'Blues') +
  facet_wrap(~Party) +
  coord_equal() +
  theme_void()

ggplot(vote_results %>% filter(vote_pct > 1), aes(x = vote_pct)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

ggsave('ZMB_pres2016_party.pdf')
