
# Scrape 2015 Zambia Election results -------------------------------------
# Build off of Tim Essam's code (tessam@usaid.gov)
# Validated and modified by Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 3 July 2017

library(rvest)
library(tidyverse)
library(stringr)
library(tidyr)


# Url information
cand_prof <- c("lungu,edgar,pf", "hichilema,hakainde,upnd", "nawakwi,edith,fdd", 
               "mumba,nevers,mmd","kaunda,tilyenji,unip", "chanda,eric,4r",
               "chipimo,elias,narep", "miyanda,godfrey,heritage", "pule,daniel,cdp",
               "sondashi,ludwig,fda", "sinkamba,peter,greens")

base_url <- c("https://www.elections.org.zm/results/2015_presidential_election/candidate/")
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
pres15 <- lapply(cand_prof, votes) %>% 
  bind_rows() %>% 
  separate(link, c("last_name", "first_name", "Party")) %>%
  mutate(constName = str_to_title(str_replace_all(str_trim(constName), "[\r\n]" , "")),
         last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         pct_cast = pct_cast/100,
         Party = str_to_upper(Party)) %>% 
  group_by(constName) %>% 
  mutate(pct_votes = vote_count / sum(vote_count),
         rank = min_rank(desc(vote_count)),
         won = ifelse(rank == 1, 1, 0),
         margin_victory = ifelse(rank == 1, vote_count - lead(vote_count), NA),
         pct_margin = ifelse(rank == 1, pct_votes - lead(pct_votes), NA)) %>% 
  # fill margin for the entire consituency
  fill(pct_margin, margin_victory)


# Also pull the stats on number rejected/cast per district ----------------




#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt
