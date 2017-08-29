
# Look into the gender breakdown of candidates by party, constitue --------



# Merge together all the candidates ---------------------------------------


pr_candid = pr_votes %>% distinct(province, district, constituency, candidate, first_name, last_name, year, won, pct_margin, party) %>% 
 mutate(election = 'presidential')

as_candid = as_votes %>% distinct(province, district, constituency, candidate, first_name, last_name, year, won, pct_margin, party) %>% 
  mutate(election = 'assembly')

# candid = bind_rows(pr_candid, as_candid) %>% 
candid = as_candid %>% 
  ungroup() %>% 
  arrange(first_name, last_name, party) %>% 
  separate(first_name, into = c('first_name', 'middle_name'), sep = ' ') %>% 
  mutate(first_name = str_trim(first_name))

unique_names = candid %>% 
  distinct(first_name)

library(babynames)
pct_thresh = 0.75

usa_names = babynames %>% 
  group_by(name, sex) %>% 
  summarise(n = sum(n)) %>% 
  mutate(origin = "US_Census") %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(total = sum(n),
         sex_ratio = n/total,
         rank = dense_rank(desc(sex_ratio)),
         # If proportion of people named that name falls between 25% - 75%, identify as "unisex" name
         sex = ifelse(sex_ratio < pct_thresh & sex_ratio >= (1 - pct_thresh), 'U', sex)) %>% 
  # select only the more prevalent name
  filter(rank == 1) %>%
  distinct(name, sex, sex_ratio, origin)

unique_names = unique_names %>% 
  left_join(usa_names, by = c('first_name' = 'name'))

write.csv(candid, paste0(data_dir, 'ZMB_unique_candidates.csv'))

candid = candid %>% left_join(unique_names)


candid %>% filter(year == 2016, !is.na(sex), won == 1) %>%
  count(sex) %>% 
  mutate(pct = n/sum(n))
  
candid %>% filter(!is.na(sex)) %>% 
  count(constituency, sex) %>% 
  ungroup() %>% 
  group_by(constituency) %>% 
  mutate(pct = n/sum(n)) %>% 
  filter(sex == 'F') %>% 
  arrange(desc(pct))


# avg margin of victory ---------------------------------------------------
candid %>% 
  ungroup() %>% 
  group_by(constituency, year) %>% 
  summarise(margin = mean(pct_margin, na.rm = T)) %>% 
  ungroup() %>% 
  count(margin > 0.1) %>% 
  mutate(n/sum(n))
  # ggplot(., aes(x = margin)) +
  # geom_histogram(binwidth = 0.025)



# look at margins of victory ----------------------------------------------
margin_thresh = 0.1
candid = candid %>% group_by(constituency, year) %>% 
  arrange(constituency, year, won) %>% 
  mutate(female_ran = any(sex == 'F'),
         female_won = won == 1 & sex == 'F',
         close_election = pct_margin < margin_thresh)

candid %>% group_by(constituency, year, female_won, female_ran) %>% 
  summarise(margin = mean(pct_margin), chk = sd(pct_margin), num_candid = n()) %>% 
  ungroup() %>% 
  group_by(female_ran, female_won) %>% 
  summarise(margin = mean(margin, na.rm = TRUE), num_candid = mean(num_candid, na.rm = TRUE), ct = n())

candid %>% group_by(constituency, year, female_won, female_ran) %>% 
  summarise(close_elec = mean(close_election), num_candid = n()) %>% 
  ungroup() %>% 
  group_by(female_ran, female_won) %>% 
  summarise(close_elec = mean(close_elec, na.rm = TRUE), num_candid = mean(num_candid, na.rm = TRUE), ct = n())

