
# Plot time series data of voter turnout in ZMB ---------------------------
# Pulls data for the turnout in the national presidental elections in Zambia 
# between 2006 and 2016; calculates difference in voter turnout by constituency
# relative for the national average for the year.  Facetted by constituency, 
# and grouped by province.

# NOTE: Zambia geography is NOT consistent between years.
# Constituency is the common name, pulled from the most recent data (2016)
# However, Province is taken from the province, as it was specified during that year.
# Before the 2015 election, there were 9 provinces; 2015 and 2016 had 10.
# Additionally, in 2016 6 new constituencies were created.
# Therefore, there may be some inconsistencies within each constituency.

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017

# Import data -------------------------------------------------------------
# source('ZMB_E00_mergeAll.R')


# Create time series function ---------------------------------------------

plot_turnout = function(df, title, 
                        subtitle = 'Difference in voter turnout from the national average',
                        palette = brewer.pal(11, 'PiYG'),
                        time_var = 'year', 
                        turnout_var = 'turnout',
                        height = 10, width = 8, 
                        facet_var = 'constituency') {
  
  # calculate national average for turnout each year, 
  # and then deviation of each constituency from the avg.
  df = df %>% 
    group_by_(time_var) %>% 
    mutate_(avg = paste0('mean(', turnout_var, ')'),
            diff = paste0(turnout_var, '- avg'))
  
  # order by the last value in the series
  facet_order = df %>% 
    group_by_(facet_var) %>% 
    mutate_(year_rank = paste0('dense_rank(desc(', time_var, '))')) %>% 
    filter(year_rank == 1) %>% 
    ungroup() %>% 
    arrange(desc(diff)) %>% 
    pull(facet_var)
  
  # reorder
  df[[facet_var]] = fct_relevel(df[[facet_var]], facet_order)
  
  # make fill scale centered on 0
  fill_lim = max(abs(min(df$diff)), max(df$diff))
  
  # area outside the limits to shade the underlying boxes
  rect_xnudge = 0.5
  rect_ynudge = 0.03
  
  ggplot(df, aes_string(x = time_var, y = 'diff', 
                        group = facet_var, fill = 'diff')) +
    
    # -- Background rectangles -- : shade +/- 0
    annotate(geom = 'rect', xmin = min(df[[time_var]]) - rect_xnudge, 
             xmax = max(df[[time_var]]) + rect_xnudge,
             ymin = 0, ymax = max(df[['diff']]) + rect_ynudge, 
             fill = palette[11], alpha = 0.1) +
    
    annotate(geom = 'rect', xmin = min(df[[time_var]]) - rect_xnudge, 
             xmax = max(df[[time_var]]) + rect_xnudge,
             ymax = 0, ymin = min(df[['diff']]) - rect_ynudge, 
             fill = palette[1], alpha = 0.1) +
    
    # -- Zero baseline --
    geom_hline(yintercept = 0, size = 0.5, colour = grey70K) +
    
    # -- Line graph --
    geom_line(colour = grey75K) +
    
    # -- Dots on top of line --
    geom_point(shape = 21, color = 'black', size = 5, stroke = 0.2) +
    
    # -- Scales --
    scale_x_continuous(breaks = unique(df[[time_var]])) + 
    scale_y_continuous(labels = scales::percent, name = NULL) +
    scale_fill_gradientn(colours = palette, labels = scales::percent,
                         limits = c(-fill_lim, fill_lim)) +
    
    # -- Labels --
    ggtitle(title, subtitle = subtitle) + 
    
    # -- Facets --
    facet_wrap(~constituency) +
    
    # -- Themes --
    theme_ygrid(font_axis_label = 9)
  
  ggsave(paste0(export_dir, 'ZMB_presturnout_', title, '.pdf'), width = width, height = height)
}


# Loop over provinces to plot ---------------------------------------------

# Find all unqiue provinces
prov = unique(pr_turnout$province)

lapply(prov, function(x) plot_turnout(pr_turnout %>% filter(province == x), paste(x, "Province")))
