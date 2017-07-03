library(geocenter)

  zmb156 = shp2df(baseDir = '~/Documents/Zambia/ElectionData/constituencies/', layerName = 'Zambia_Const_156', getCentroids = FALSE)
  
  zmb150 = shp2df(baseDir = '~/Documents/Zambia/ElectionData/constituencies_pre2016/', layerName = 'GRED_Zambia_2006_beta2', getCentroids = FALSE)
  
  
  zmb156 = zmb156 %>% 
    mutate(constituency = str_to_title(ConstName1))

  zmb150 = zmb150 %>% 
    mutate(constituency = str_to_title(CST_N))
  
  
cst156 = zmb156 %>% select(constituency, DistName1) %>% distinct()
cst150 = zmb150 %>% select(constituency, CST_N) %>% distinct() 

# Merging, crosswalks -----------------------------------------------------

cst156_website = read.csv('~/Documents/GitHub/Zambia/processeddata/ZMB_constituencies.csv')
# from ZMB_pres2015_clean.R
cst150_website = pres15 %>% select(constName) %>% distinct()

cst156_crosswalk = full_join(cst156, cst156_website %>% mutate(x = 'website_2016'), by = c("constituency" = "Constituency"))
write.csv(cst156_crosswalk, '~/Documents/GitHub/Zambia/processeddata/cst156_crosswalk.csv')


cst150_crosswalk = full_join(cst150, cst150_website %>% mutate(y = 'website_2015'), by = c("constituency" = "constName"))
write.csv(cst150_crosswalk, '~/Documents/GitHub/Zambia/processeddata/cst156_crosswalk.csv')

crosswalk = full_join(cst150_crosswalk, cst156_crosswalk, by = c("constituency"))

crosswalk = crosswalk %>% mutate(merge_flag = ifelse(is.na(x) | is.na(y) | is.na(DistName1) | is.na(CST_N), 1, 0))
  
write.csv(crosswalk, '~/Documents/GitHub/Zambia/processeddata/cstcrosswalk.csv')


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
