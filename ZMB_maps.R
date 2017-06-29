library(geocenter)

  geo = shp2df(baseDir = '~/Documents/Zambia/ElectionData/constituencies/', layerName = 'Zambia_Const_156', getCentroids = FALSE)

  geo = geo %>% 
    mutate(constituency = str_to_title(ConstName1))

  
  # merge geo
  winners = left_join(geo, winners, by = 'constituency')
  
  
  
  

# plot --------------------------------------------------------------------
p = ggplot(winners, aes(x = long, y = lat, fill = Party, group = group, order = order)) +
    geom_polygon() +
    geom_path(size = 0.1, color = 'white') +
    coord_equal() +
    theme_void()
  

  
  ggsave('test2.pdf')


df %>% 
  group_by(constituency) %>% 
  mutate(pct_votes = votes/sum(votes)) %>% 
  arrange(constituency) %>% 
  select(constituency, party, pct_votes) %>% 
  mutate(class_votes = cut(pct_votes, breaks = pct_breaks)) %>% 
  gather()
    # spread(party, pct_votes)
