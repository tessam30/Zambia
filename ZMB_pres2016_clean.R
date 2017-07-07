# Scrape 2016 Zambia Election results -------------------------------------
# Build off of Tim Essam's code (tessam@usaid.gov)
# Validated and modified by Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 6 July 2017

library(rvest)
library(tidyverse)
library(readxl)
library(stringr)
library(stringi)

base_dir = '~/Documents/GitHub/Zambia/'

# Scrape Constituency-level totals ----------------------------------------

# Url information
cand_prof <- c("lungu,edgar,pf", "hichilema,hakainde,upnd", "nawakwi,edith,fdd", 
               "banda,andyford,pac", "kabimba,wynter,rainbow", "chishimba,saviour,upp",
               "kaunda,tilyenji,unip", "sinkamba,peter,greens", "mwamba,maxwell,da")

base_url <- c("https://www.elections.org.zm/results/2016_presidential_election/candidate/")
url_list <- str_c(base_url, cand_prof)

# Steps:
# 1. for each name in cand_prof, append the string to the end of the base_url line
# 2. hit the url and extrac the "td" table into a matrix
# 3. convert the matrix to a dataframe that is reshaped into 3 vectors
# 4. Add a new column with candidates name, add in a new columm with party

votes <- function(x) {
  
  # specify the url + the stub of candidate
  link <- read_html(str_c(base_url, x))
  
  # use the rvest functions to extract the table
  tmp <- link %>% 
    html_nodes("td") %>%
    html_text() %>% 
    matrix()
  
  # Reshape the dataframe into 3 columns, clean up some of the variables    
  df <- as.data.frame(matrix(tmp, nrow = (dim(tmp)[1] / 3), byrow = TRUE)) %>% 
    mutate(vote_count = parse_number(V2),
           pct_cast = parse_number(V3), 
           link = x, 
           constName = parse_character(V1))
  
  return(df)
}


# Call function and store in a new dataframe
pr16_raw = lapply(cand_prof, votes) %>% 
  bind_rows() %>% 
  separate(link, c("last_name", "first_name", "Party")) %>%
  mutate(constituency = str_to_title(str_replace_all(str_trim(constName), "[\r\n]" , "")),
         last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         pct_cast = pct_cast/100,
         party = str_to_upper(Party),
         candidate = paste(first_name, last_name)) %>% 
  group_by(constituency) %>% 
  mutate(year = 2016,
         pct_votes = vote_count / sum(vote_count),
         rank = min_rank(desc(vote_count)),
         won = ifelse(rank == 1, 1, 0),
         margin_victory = ifelse(rank == 1, vote_count - lead(vote_count), NA),
         pct_margin = ifelse(rank == 1, pct_votes - lead(pct_votes), NA)) %>% 
  # fill margin for the entire constituency
  fill(pct_margin, margin_victory)  %>% 
  ungroup() %>% 
  group_by(party) %>% 
  mutate(natl_votes = sum(vote_count)) %>% 
  ungroup() %>% 
  mutate(party_natl_pct = natl_votes/sum(vote_count))



#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt


# merge w/ lookup table ---------------------------------------------------
# Creates a crosswalk between the shapefile names and those used on election website (with vote count)
# Also connects the 150 constituencies from pre-2016 to the 156 afterwards.
# Note that while the names may be the same, the boundaries have shifted and in some cases are quite different.

geo_base = read_excel(paste0(base_dir, 'ZMB_admin_crosswalk.xlsx'), sheet = 2)

pr16_raw = left_join(pr16_raw, geo_base, by = c("constituency" = "website2016"))

# pull out just the relevant vars -----------------------------------------

pr16 = pr16_raw %>% 
  select(year, province, district, constituency, 
         party, candidate, first_name, last_name,
         vote_count, rank, won, 
         pct_cast, pct_votes, margin_victory, pct_margin, party_natl_pct)
