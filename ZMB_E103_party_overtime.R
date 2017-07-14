
# Area graph over time ----------------------------------------------------
# Change in share of electoral vote over time, by party
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 14 July 2017
# In the end, made with RAW graphics and imported into AI.



# import data -------------------------------------------------------------
# source('ZMB_pres_mergeAll.R')
# source('ZMB_E201_pct_byparty.R')

party_order = unique(party_tot$party_grp)

party_tot$party_grp = factor(party_tot$party_grp, levels = party_order)


ggplot(party_tot, aes(x = year, y = pct, fill = color)) +
  geom_area(stat = 'identity', alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_identity() +
  # facet_wrap(~party_grp) +
  theme_ygrid()

ggplot(party_tot, aes(x = year, y = pct, fill = party_grp)) +
  geom_bar(stat = 'identity', alpha = 1) +
  scale_y_continuous(labels = scales::percent) + 
  theme_bw()

