# Plot Zambian election data, by constituency ---------------------------------
# In previous files, election data from Zambian elections were imported
# cleaned, and merged.
# This file plots choropleths of the the Presidential election turnout by constituency
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 13 July 2017


# setup -------------------------------------------------------------------

# choropleth breaks
pct_breaks = c(seq(-5, 15, by = 5), seq(20, 100, by = 20))/100


# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')



# plot for each year ------------------------------------------------------
# plot_choro defined in `ZMB_E01_helper.R`

t06_15 = plot_choro(turnout_06_15, '2006-2015', 'turnout')
r06_15 = plot_choro(turnout_06_15, '2006-2015', 'pct_rejected', 
                    palette = brewer.pal(9, 'OrRd'), fill_lim = c(0, 0.05))
