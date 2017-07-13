
# Plot time series data of voter turnout in ZMB ---------------------------
library(ggplot2)
library(RColorBrewer)
library(extrafont)
loadfonts()

plot_turnout = function(df, 
                        palette = brewer.pal(11, 'PiYG'),
                        time_var = 'year', 
                        turnout_var = 'turnout',
                        facet_var = 'constituency') {
  
  rect_xnudge = 0.5
  rect_ynudge = 0.03
  
  ggplot(df, aes_string(x = time_var, y = turnout_var, 
                        group = facet_var, fill = turnout_var)) +
    
    # background: shade +/- 0
    annotate(geom = 'rect', xmin = min(df[[time_var]]) - rect_xnudge, 
             xmax = max(df[[time_var]]) + rect_xnudge,
             ymin = 0, ymax = max(df[[turnout_var]]) + rect_ynudge, 
             fill = palette[11], alpha = 0.1) +
    
    annotate(geom = 'rect', xmin = min(df[[time_var]]) - rect_xnudge, 
             xmax = max(df[[time_var]]) + rect_xnudge,
             ymax = 0, ymin = min(df[[turnout_var]]) - rect_ynudge, 
             fill = palette[1], alpha = 0.1) +
    
    
    geom_hline(yintercept = 0, size = 1, colour = 'grey') +
    
    geom_line() +
    geom_point(shape = 21, color = 'black', size = 5, stroke = 0.2) +
    
    scale_y_continuous(labels = scales::percent) +
    scale_fill_gradientn(colours = palette) +
    
    facet_wrap(~constituency) +
    theme_bw()
}

plot_turnout(df)
