
# Plot time series data of voter turnout in ZMB ---------------------------

plot_turnout = function(df, title, 
                        subtitle = 'Difference in voter turnout from the national average',
                        palette = brewer.pal(11, 'PiYG'),
                        time_var = 'year', 
                        turnout_var = 'turnout',
                        facet_var = 'constituency') {
  
  # calculate national average for turnout each year, 
  # and then deviation of each constituency from the avg.
  df = df %>% 
    group_by_(time_var) %>% 
    mutate_(avg = paste0('mean(', turnout_var, ')'),
            diff = paste0(turnout_var, '- avg'))
  
  # order by the last value in the series
  facet_order = df %>% 
    filter_(paste0(time_var, ' == max(', df[[time_var]], ')')) %>% 
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
    geom_hline(yintercept = 0, size = 1, colour = 'grey') +
    
    # -- Line graph --
    geom_line() +
    
    # -- Dots on top of line --
    geom_point(shape = 21, color = 'black', size = 5, stroke = 0.2) +
    
    # -- Scales --
    scale_y_continuous(labels = scales::percent, name = NA) +
    scale_fill_gradientn(colours = palette, labels = scales::percent,
                         limits = c(-fill_lim, fill_lim)) +
    
    # -- Labels --
    ggtitle(title, subtitle = subtitle) + 
    
    # -- Facets --
    facet_wrap(~constituency) +
    
    # -- Themes --
    theme_bw()
}

# Find all unqiue provinces
prov = unique(df$province)

lapply(prov, function(x) plot_turnout(df %>% filter(province == x), x))
