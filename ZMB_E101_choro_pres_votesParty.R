# Plot Zambian election data, by party and constituency ---------------------------------
# In previous files, election data from Zambian elections were imported
# cleaned, and merged.
# This file plots choropleths 
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017


# setup -------------------------------------------------------------------

# choropleth breaks
pct_breaks = c(seq(-5, 15, by = 5), seq(20, 100, by = 20))/100

# placement of annotations: nat'l voting pcts, by party
x_annot = -200000
y_annot = -900000

size_annot = 10


# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')


# filter just parties with larger vote counts -----------------------------------------------------------

# order for plotting the parties
party_order = c('PF', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')
# , 'FDD', 'ADD', 'UNIP')

year_order = c(2016, 2015, 2011, 2008, 2006)


# main choropleth plot function -------------------------------------------
# NOTE: can't simply facet over year and party, b/c can't join to a single shapefile.
# Since using two different shapefiles (2015 and before, and 2016), have to run twice

plot_votes = function(geo_df, sel_year, party_tot,
                      pty_order = party_order, yr_order = year_order,
                      width = 12, height = 12) {
  
  # geodata frame, complete with voting results by party/constituency merged in
  geo_df = geo_df %>%
    filter(party %in% party_order) %>% 
    mutate(vote_cat = cut(pct_votes, breaks = pct_breaks)) 
  
  # reorder levels
  geo_df$party = forcats::fct_relevel(geo_df$party, pty_order)
  geo_df$year = factor(geo_df$year, levels = yr_order)
  
  p = ggplot(geo_df) +
    # -- annotations -- total pct by party, across the country
    geom_text(aes(x = x_annot, y = y_annot, label = pct_lab, colour = color), 
              size = size_annot, alpha = 0.75,
              family = 'Lato Black',
              data = party_tot %>% filter(party %in% party_order)) +
    
    # -- choropleth --
    geom_sf(aes(fill = color, alpha = vote_cat), size = 0.1) +
    
    # -- scales -- 
    scale_color_identity() +
    scale_fill_identity() +
    
    # -- facets --
    facet_wrap(~year + party, ncol = 3) +
    
    # -- themes --
    theme_facet(facet_size = 16) 
  
  
  
  ggsave(paste0(export_dir, 'ZMB_pres', sel_year, '_party.pdf'),
         width = width, height = height)
  
  return(p)
}


# plot for each year ------------------------------------------------------


p16 = plot_votes(pres16, 2016, party_tot %>% filter(year == 2016))

p06_15 = plot_votes(pres_06_15, '2006-2015', party_tot %>% filter(year != 2016))

# plot legend -------------------------------------------------------------
colors = parties %>% select(party = party_name, color) %>% distinct() 

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
  theme_blank() +
  # coord_equal() +
  theme(legend.position = 'none')
