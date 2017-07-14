# Plot Zambian election data, by constituency ---------------------------------
# In previous files, election data from Zambian elections were imported
# cleaned, and merged.
# This file plots choropleths of the the Presidential election turnout by constituency
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 13 July 2017


# setup -------------------------------------------------------------------
turnout_pal = brewer.pal(9, 'YlGn')
rejected_pal = brewer.pal(9, 'OrRd')


# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')


# find limits -------------------------------------------------------------
# simple linear scale
turnout_lim = c(min(turnout16$turnout, turnout_06_15$turnout), max(turnout16$turnout, turnout_06_15$turnout))
rejected_lim = c(min(turnout16$pct_rejected, turnout_06_15$pct_rejected),
                 max(turnout16$pct_rejected, turnout_06_15$pct_rejected))


# relevel years -----------------------------------------------------------
turnout16 = turnout16 %>% 
  mutate(year_lab = fct_rev(as.factor(year)))

turnout_06_15 = turnout_06_15 %>% 
  mutate(year_lab = fct_rev(as.factor(year)))

# plot for each year ------------------------------------------------------
# plot_choro defined in `ZMB_E01_helper.R`

t06_15 = plot_choro(turnout_06_15, '2006-2015', 'turnout', ncol = 4, palette = turnout_pal,
                    fill_lim = turnout_lim, facet_var = 'year_lab')

t16 = plot_choro(turnout16, '2016', 'turnout', fill_lim = turnout_lim, 
                 ncol = 4, palette = turnout_pal,
                 facet_var = 'year_lab')

r06_15 = plot_choro(turnout_06_15, '2006-2015', 'pct_rejected', ncol = 4,
                    palette = rejected_pal, fill_lim = rejected_lim, facet_var = 'year_lab')

r16 = plot_choro(turnout16, '2016', 'pct_rejected', ncol = 4,
                 palette = rejected_pal, fill_lim = rejected_lim, facet_var = 'year_lab')


# plot turnout over time --------------------------------------------------
avg_turnout = pr_turnout %>% 
  group_by(year) %>% summarise(turnout = sum(cast)/sum(registered),
                               rejected = sum(rejected)/sum(cast)) %>% 
  mutate(year_lab = fct_rev(as.factor(year)))

line_avg = function(df, title, filename,
                    y_var = 'turnout',
                    width = 16, 
                    height = 1.75,
                    palette = brewer.pal(9, 'YlGn'), 
                    fill_lim = NA) {
  
  p = ggplot(df, aes_string(x = 'year_lab', group = 1, y = y_var, fill = y_var)) +
    
    geom_line(colour = grey60K, size = 0.2) +
    geom_point(size = 5.5, shape = 21, colour = 'white', stroke = 2) +
    
    # -- scales --
    scale_fill_gradientn(colours = palette, limits = fill_lim) +
    scale_y_continuous(name = NULL, labels = scales::percent) +
    
    # -- titles --
    ggtitle(title) +
    
    theme_ygrid()
  
  ggsave(paste0(export_dir, filename),     
         useDingbats = FALSE, 
         width = width, height = height)
  
  return(p)
}

# Avg. turnout over time
line_avg(avg_turnout, 'Average turnout in Presidential elections', 'ZMB_avg_turnout.pdf', palette = turnout_pal,
         fill_lim = turnout_lim)

# Avg. percent rejected over time 
line_avg(avg_turnout, 'Average percent of rejected ballots in Presidential elections', 
         palette = rejected_pal, 
         'ZMB_avg_rejected.pdf', fill_lim = rejected_lim, y_var = 'rejected')
