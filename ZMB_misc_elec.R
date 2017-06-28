percent = function(x, ndigits) {
  paste0(round(x*100, ndigits), '%')
}

# Difference b/w UPND and PF
votes %>% 
  group_by(province, Party) %>% 
  summarise(votes = sum(votes)) %>%
  ungroup() %>% 
  group_by(province) %>% 
  mutate(pct = votes / sum(votes)) %>% 
  arrange(province) %>% 
  filter(Party %in% c('UPND', 'PF'))

# PF and UPND roughly equivalent in the number of votes received
party_total = votes %>% 
  group_by(Party) %>% 
  summarise(votes = sum(votes)) %>%
  ungroup() %>% 
  mutate(pct = votes / sum(votes),
         pct_votes = percent(pct)) %>% 
  arrange(desc(votes)) %>% 
  mutate(exp_seats = round(156 * pct))

# ... but PF won more seats.
party_won = votes %>% 
  filter(won == 1) %>% 
  count(Party) %>% 
  ungroup() %>% 
  mutate(pct_seats = percent(n/sum(n))) %>% 
  arrange(desc(n)) %>% 
  rename(num_seats = n)

# differential:
party_diff =
  left_join(party_total, party_won, by = 'Party') %>% 
  select(party = Party, pct_votes, pct_seats, contains('seats')) %>% 
  mutate(num_seats = coalesce(num_seats, 0L),
         diff = num_seats - exp_seats) %>% 
  arrange(desc(abs(diff)))


# margins of victory
library(ggplot2)
qplot(data = votes, x = pct_margin, binwidth = 0.025)

# Find close races
votes %>% 
  filter(pct_margin < 0.2, rank < 3) %>% 
  select(constituency, Party, rank) %>% 
  spread(rank, Party) %>% 
  count(`1`, `2`) %>% 
  arrange(desc(n))


winners = votes %>% 
  filter(rank == 1)
