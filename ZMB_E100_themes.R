
# colors ------------------------------------------------------------------
grey90K = '#414042'
grey80K = '#58595b'
grey75K = '#636466'
grey70K = '#6d6e71'
grey60K = '#808285'
grey50K = '#939598'
grey40K = '#a7a9ac'
grey25K = '#c7c8ca'
grey15K = '#dcddde'


# Llamar helpers ----------------------------------------------------------

#' @describeIn check_font Returns TRUE/FALSE if font is installed.
#' @export 
check_font = function(font_name){
  installed_fonts = extrafont::fonts()
  
  if (font_name %in% installed_fonts) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}

#' @describeIn check_font Replace font if it isn't found in the system
#' @param default_font string containg a font to use as a substituted if not found
#' @export
replace_font = function(font_name, default_font = 'sans') {
  if(check_font(font_name) == FALSE) {
    # font isn't installed
    return(default_font)
  } else{
    return(font_name)
  }
}

# ggplot themes, from llamar ----------------------------------------------

# > NULL
#' @param font_normal (optional) string containing font name for normal text
#' @param font_semi (optional) string containing font name for semilight text
#' @param font_light (optional) string containing font name for light text
#' @param font_axis_label (optional) value, in points, of the font in the axis labels. If changed, will change the other font sizes in the plot unless they are also adjusted.
#' @param font_axis_title (optional) value, in points, of the font in the axis titles
#' @param font_facet (optional) value, in points, of the font in the facet titles
#' @param font_legend_title (optional) value, in points, of the font in the legend title
#' @param font_legend_label (optional) value, in points, of the font in the legend values
#' @param font_title (optional) value, in points, of the font in the plot title
#' @param font_subtitle (optional) value, in points, of the font in the plot subtitle
#' @param legend.position (optional) legend position; takes either a ggplot legend position string like `'left'` or a tuple in percent of the x- and y-axes, like `c(0.3, 0.7)`
#' @param legend.direction (optional) legend direction; eitehr 'horizontal' or 'vertical'
#' @param panel_spacing (optional) number of lines between facet panels
#' @param grey_background (optional) if TRUE, make the background of the plot (but not the panel) background_colour
#' @param background_colour (optional) colour argument for the fill of the background, if grey_background == TRUE
#' @param projector (optional) if TRUE, make lines and text bolder for an LCD projector

#' 
#' 
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl)) + geom_point() + facet_wrap(~am) + ggtitle('Heavier cars have worse gas efficiency') + theme_blank()
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl)) + geom_point() + facet_wrap(~am) + ggtitle('Heavier cars have worse gas efficiency') + theme_stroke()
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl)) + geom_point() + facet_wrap(~am) + ggtitle('Heavier cars have worse gas efficiency') + theme_xgrid(legend.position = c(0.8, 0.9), grey_background = TRUE)
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl)) + geom_point() + facet_wrap(~am) + ggtitle('Heavier cars have worse gas efficiency') + theme_ygrid()
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl)) + geom_point() + facet_wrap(~am) + ggtitle('Heavier cars have worse gas efficiency', subtitle='data = mtcars') + theme_xygrid()



#' @describeIn  themes Internal function to set aesthetics.
set_aesthetics = function(font_normal, font_semi, font_light, projector) {
  # Decide whether to use projector mode or not.
  if(projector == FALSE) {
    # Define standard colours for grid lines and text
    normal_grid_stroke = 0.1
    
    normal_grid = grey60K
    
    text_colour = grey60K
    subtitle_colour = grey75K
    title_colour = grey90K
    
    grid_stroke = normal_grid_stroke
    grid_colour = normal_grid
    
    # Check if fonts are defined.
    font_normal = replace_font(font_name = font_normal)
    font_semi = replace_font(font_name = font_semi)
    font_light = replace_font(font_name = font_light)
    
  } else {
    # Define standard colours for grid lines and text
    projector_grid_stroke = 0.25
    
    projector_grid = grey75K
    
    text_colour = grey75K
    subtitle_colour = grey90K
    title_colour = grey90K
    
    grid_stroke = projector_grid_stroke
    grid_colour = projector_grid
    
    # Check if fonts are defined; use darker font.
    font_normal = replace_font(font_name = font_normal)
    font_semi = replace_font(font_name = font_normal)
    font_light = replace_font(font_name = font_normal)
  }
  
  return(list(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
              grid_stroke = grid_stroke, grid_colour = grid_colour, 
              text_colour = text_colour, subtitle_colour = subtitle_colour, title_colour = title_colour))
}

#' @describeIn  themes completely blank theme; similar to theme_void but without legend or margins.
#' @export
theme_blank <- function(legend.position = 'none',
                        legend.direction = 'horizontal',
                        font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        font_legend_title = 12, 
                        font_legend_label = font_legend_title * 0.8,
                        projector = FALSE,
                        background_colour = NA
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  # -- theme --
  theme(title = element_blank(), 
        
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), 
        
        strip.text = element_blank(),
        strip.background = element_blank(),
        
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
        
        
        legend.position = legend.position,
        legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi, angle = 0),
        legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi, angle = 0),
        legend.direction = legend.direction)
}

#' @describeIn  themes similar to blank theme, but with facet titles.
#' @export
theme_facet <- function(legend.position = 'none',
                        legend.direction = 'horizontal',
                        font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        font_facet = 'Lato Light',
                        facet_size = 14,
                        font_legend_title = 12, 
                        font_legend_label = font_legend_title * 0.8,
                        projector = FALSE,
                        background_colour = NA
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  # -- theme --
  theme(title = element_blank(), 
        
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), 
        
        strip.text = element_text(family = font_facet, size = facet_size, colour = subtitle_colour, hjust = 0.025), 
        strip.background = element_blank(),
        
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
        
        
        legend.position = legend.position,
        legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi, angle = 0),
        legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi, angle = 0),
        legend.direction = legend.direction)
}



#' @describeIn  themes Theme with thin grey border around edge
#' @export
#' 
theme_stroke = function(stroke_size = 0.25,
                        stroke_colour = grey90K) {
  theme(plot.background = element_rect(colour = stroke_colour, size = stroke_size, linetype = 1),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
}


#' @describeIn  themes Theme with x labels, x title, and x-axis line; no gridlines
#' @export
theme_xaxis <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_subtitle = font_axis_label * 1.2,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_line(size = grid_stroke * 2, colour = grey90K), 
    axis.ticks.x = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}


#' @describeIn  themes Theme with y labels, titles, and y-axis line; no gridlines
#' @export
theme_yaxis <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_subtitle = font_axis_label * 1.2,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_line(size = grid_stroke * 2, colour = grey90K), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}

#' @describeIn themes Theme with x-axis labels; no gridlines.
#' @export
theme_xlab <- function(font_normal = 'Lato',
                       font_semi = 'Lato',
                       font_light = 'Lato Light',
                       legend.position = 'none',
                       legend.direction = 'horizontal',
                       panel_spacing = 3, # panel spacing, in lines
                       font_axis_label = 12,
                       font_axis_title = font_axis_label * 1.15,
                       font_facet = font_axis_label * 1.15,
                       font_legend_title = font_axis_label, 
                       font_legend_label = font_axis_label * 0.8,
                       font_subtitle = font_axis_label * 1.2,
                       font_title = font_axis_label * 1.3,
                       grey_background = FALSE,
                       background_colour = grey10K,
                       projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}


#' @describeIn themes Theme with y-axis labels; no gridlines.
#' @export
theme_ylab <- function(font_normal = 'Lato',
                       font_semi = 'Lato',
                       font_light = 'Lato Light',
                       legend.position = 'none',
                       legend.direction = 'horizontal',
                       panel_spacing = 3, # panel spacing, in lines
                       font_axis_label = 12,
                       font_axis_title = font_axis_label * 1.15,
                       font_facet = font_axis_label * 1.15,
                       font_legend_title = font_axis_label, 
                       font_legend_label = font_axis_label * 0.8,
                       font_subtitle = font_axis_label * 1.2,
                       font_title = font_axis_label * 1.3,
                       grey_background = FALSE,
                       background_colour = grey10K,
                       projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}


#' @describeIn themes Theme with x and y axis labels; no gridlines.
#' @export
theme_xylab <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_subtitle = font_axis_label * 1.2,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_blank(), 
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}


#' @describeIn  themes Theme with light x-grid lines, x and y axis labels, and x-axis title.
#' @export
theme_xgrid <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_subtitle = font_axis_label * 1.2,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_line(size = grid_stroke, colour = grid_colour), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}



#' @describeIn  themes Theme with light y-grid lines, x and y axis labels, and y-axis title.
#' @export
theme_ygrid <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_title = font_axis_label * 1.3,
                        font_subtitle = font_axis_label * 1.2,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_line(size = grid_stroke, colour = grid_colour), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}



#' @describeIn  themes Theme with x and y labels, titles, and gridlines
#' @export
theme_xygrid <- function(font_normal = 'Lato',
                         font_semi = 'Lato',
                         font_light = 'Lato Light',
                         legend.position = 'none',
                         legend.direction = 'horizontal',
                         panel_spacing = 3, # panel spacing, in lines
                         font_axis_label = 12,
                         font_axis_title = font_axis_label * 1.15,
                         font_facet = font_axis_label * 1.15,
                         font_legend_title = font_axis_label, 
                         font_legend_label = font_axis_label * 0.8,
                         font_subtitle = font_axis_label * 1.2,
                         font_title = font_axis_label * 1.3,
                         grey_background = FALSE,
                         background_colour = grey10K,
                         projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_line(size = grid_stroke, colour = grid_colour), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_line(size = grid_stroke, colour = grid_colour), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}


#' @describeIn  themes Theme with axis labels, titles, grid lines, axes, and legend.
#' @export
theme_basic <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = c(0.85, 0.85),
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_subtitle = font_axis_label * 1.2,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_line(size = grid_stroke, colour = grid_colour), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_line(size = grid_stroke, colour = grid_colour), 
    panel.spacing = unit(panel_spacing, "lines"), 
    plot.margin = plot_margin, 
    
    panel.border = element_rect(size = grid_stroke * 4, colour = grey90K, fill = NA),
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}
