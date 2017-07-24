# Plot Zambian election data, by party and constituency ---------------------------------
# In previous files, election data from Zambian elections were imported
# cleaned, and merged.

# This file plots 4 figures:
# [1] choropleths of % each party won, by constituency, by year, for presidential elections
# [2] choropleths of % each party won, by constituency, by year, for parliamentary elections
# [3] choropleths of which party won, by constituency, by year, for parliamentary elections
# [4] bar graph of number of parliamentary winners
# [5] legend for choropleths

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 7 July 2017


# Structure ---------------------------------------------------------------
# 1. Setup key params: choropleth breakpoints, annotation locations, export dims, which parties to plot
# 2. Import data, including summary stats of pct won by each party 
# 3. Create function to plot choropleth of pct won by each party
# 4. Call that function for 2016 and pre-2015 (since they have separate shapefiles)
# 5. Plot legend to be appended to choros in Illustrator

# setup -------------------------------------------------------------------

# choropleth breaks
pct_breaks = c(seq(-5, 15, by = 5), seq(20, 100, by = 20))/100

# placement of annotations: nat'l voting pcts, by party
x_annot = -200000
y_annot = -900000
# font size for annotations
size_annot = 10

# export dimensions for plot
width = 8
height = 10

# filter just parties with larger vote counts
# order for plotting the parties
party_order = c('PF', 'Patriotic Front', 'UPND', 'UPND, FDD, UNIP (UDA)', 'MMD')
# , 'FDD', 'ADD', 'UNIP')

# import data -------------------------------------------------------------
# load and merge all years to geographic data
# source('ZMB_E00_mergeAll.R')

# calculate summary statistics: pct each party won nationally each year
source('ZMB_E101_calcpct_byparty.R')

# themes (for ggplot funcs)
source('ZMB_E200_themes.R')

# main choropleth plot function -------------------------------------------
# NOTE: can't simply facet over year and party, b/c can't join to a single shapefile.
# Since using two different shapefiles (2015 and before, and 2016), have to run twice

plot_votes = function(geo_df, sel_year, party_tot,
                      elec_type = 'pres',
                      pty_order = party_order, yr_order = year_order,
                      width = 12, height = 12) {
  
  # geodata frame, complete with voting results by party/constituency merged in
  geo_df = geo_df %>%
    # filter out 
    filter(party_name %in% party_order) %>% 
    mutate(vote_cat = cut(pct_cast, breaks = pct_breaks)) 
  
  # reorder levels: arrange by the most recent election results
  geo_df$party_name = forcats::fct_relevel(geo_df$party_name, pty_order)
  party_tot$party_name = forcats::fct_relevel(party_tot$party_name, pty_order)
  
  # sort years from most recent to least
  geo_df$year = fct_rev(factor(geo_df$year))
  party_tot$year = fct_rev(factor(party_tot$year))
  
  p = ggplot(geo_df) +
    # -- annotations -- total pct by party, across the country
    geom_text(aes(x = x_annot, y = y_annot, label = pct_lab, colour = color), 
              size = size_annot, alpha = 0.75,
              family = 'Lato Black',
              data = party_tot %>% filter(party_name %in% party_order)) +
    
    # -- choropleth --
    geom_sf(aes(fill = color, alpha = vote_cat), size = 0.1) +
    
    # -- scales -- 
    scale_color_identity() +
    scale_fill_identity() +
    
    # -- facets --
    facet_wrap(~year + party_name, ncol = 3) +
    
    # -- themes --
    theme_facet(facet_size = 16) 
  
  
  
  ggsave(paste0(export_dir, 'ZMB_', elec_type, sel_year, '_party.pdf'),
         width = width, height = height)
  
  return(p)
}

# [1] PRESIDENTIAL DATA ---------------------------------------------------
# plot for each year
p16 = plot_votes(pres16, 2016, pres_tot %>% filter(year == 2016))
p06_15 = plot_votes(pres_06_15, '2006-2015', pres_tot %>% filter(year != 2016))

# [2] PARLIAMENTARY DATA ---------------------------------------------------
# plot for each year
a16 = plot_votes(parl16, 2016, parl_tot %>% filter(year == 2016), elec_type = 'parl')
a06_11 = plot_votes(parl_06_11, '2006-2015', parl_tot %>% filter(year != 2016), elec_type = 'parl')


# [3] PARLIAMENTARY WINNERS -----------------------------------------------

plot_winners = function(geo_df, sel_year, 
                        yr_order = year_order,
                        width = 12, height = 12) {
  
  # geodata frame, complete with voting results by party/constituency merged in
  geo_df = geo_df %>%
    # filter out; include only winners (or -1 for constituencies w/o elections)
    filter(won %in% c(1, -1))
  
  
  # sort years from most recent to least
  geo_df$year = fct_rev(factor(geo_df$year))
  
  p = ggplot(geo_df) +
    
    # -- choropleth --
    geom_sf(aes(fill = color), size = 0.1, alpha = 0.7) +
    
    # -- scales -- 
    scale_color_identity() +
    scale_fill_identity(na.value = grey25K) +
    
    # -- facets --
    facet_wrap(~year, ncol = 3) +
    
    # -- themes --
    theme_facet(facet_size = 16) 
  
  
  
  ggsave(paste0(export_dir, 'ZMB_parl_winners', sel_year, '_party.pdf'),
         width = width, height = height)
  
  return(p)
}


w16 = plot_winners(parl16, 2016)
w06_11 = plot_winners(parl_06_11, '2006-2015')


# [4] Parliamentary winners bar graph -------------------------------------
pty_order = c('unknown', 'ADD', 'FDD', 'NDF', 'ULP', 'Independent', 'UPND', 'Patriotic Front', 'MMD')

ggplot(as_winners, aes(y = n, x = fct_relevel(party_name, pty_order),
                       color = color, fill = color)) +
  geom_bar(stat = 'identity', alpha = 0.5, size = 0.25) +
  facet_wrap(~ year) +
  coord_flip() + 
  scale_fill_identity() + 
  scale_y_reverse() +
  scale_color_identity() + 
  ylab('number of representatives') +
  theme_xgrid()

ggsave(paste0(export_dir, 'ZMB_parl_seats.pdf'), width = width, height = height/5)

# [5] plot legend -------------------------------------------------------------
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
  # -- scale bars: heatmap --
  geom_tile(color = grey60K, size = 0.1) +
  
  # -- labels --
  geom_text(nudge_x = 0.5, nudge_y = -0.65, vjust = 1, 
            size = 3, colour = grey60K, alpha = 1, family = 'Lato Light') + 
  
  # -- scales --
  scale_y_continuous(expand = c(0.25, 0.2)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_fill_identity() +
  
  # -- facets --
  facet_wrap(~party_name, ncol = 2) +
  
  # -- theme -- 
  theme_facet()


ggsave(paste0(export_dir, 'ZMB_pres_scales.pdf'), width = width, height = height)
