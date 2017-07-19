# Scrape 2016 Zambia Election results -------------------------------------
# Breakdown by candidate-level data built off of Tim Essam's code (tessam@usaid.gov)
# Validated and modified by Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 6 July 2017


# goals -------------------------------------------------------------------
# 1. Pull the candidate-level constituency breakdowns for presidential election
# 2. Pull the constituency-level turnout numbers for presidential election
# 3. clean those two; calculate stats and merge to geographic data

# setup -------------------------------------------------------------------

# taken care of in ZMB_E01_helpers.R
# library(rvest)
# library(tidyverse)
# library(readxl)
# library(stringr)
# library(stringi)

# base_dir = '~/Documents/GitHub/Zambia/'

# [1] Scrape Constituency-level totals ----------------------------------------

# Url information
cand_prof = c("lungu,edgar,pf", "hichilema,hakainde,upnd", "nawakwi,edith,fdd", 
              "banda,andyford,pac", "kabimba,wynter,rainbow", "chishimba,saviour,upp",
              "kaunda,tilyenji,unip", "sinkamba,peter,greens", "mwamba,maxwell,da")

base_url = c("https://www.elections.org.zm/results/2016_presidential_election/candidate/")
url_list = str_c(base_url, cand_prof)

# Steps:
# 1. for each name in cand_prof, append the string to the end of the base_url line
# 2. hit the url and extrac the "td" table into a matrix
# 3. convert the matrix to a dataframe that is reshaped into 3 vectors
# 4. Add a new column with candidates name, add in a new columm with party

votes = function(x) {
  
  # specify the url + the stub of candidate
  link = read_html(str_c(base_url, x))
  
  # use the rvest functions to extract the table
  tmp = link %>% 
    html_nodes("td") %>%
    html_text() %>% 
    matrix()
  
  # Reshape the dataframe into 3 columns, clean up some of the variables    
  df = as.data.frame(matrix(tmp, nrow = (dim(tmp)[1] / 3), byrow = TRUE)) %>% 
    mutate(vote_count = parse_number(V2),
           pct_cast_web = parse_number(V3), 
           link = x, 
           constName = parse_character(V1))
  
  return(df)
}


# Call function and store in a new dataframe
pr16 = lapply(cand_prof, votes) %>% 
  bind_rows() %>% 
  separate(link, c("last_name", "first_name", "Party")) %>%
  mutate(constituency = str_to_title(str_replace_all(str_trim(constName), "[\r\n]" , "")),
         last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         pct_cast_web = pct_cast_web/100,
         party = str_to_upper(Party),
         candidate = paste(first_name, last_name)) %>% 
  group_by(constituency) %>% 
  mutate(year = 2016) %>% 
  left_join(geo_base %>% select(province, district2016, constituency), by = c('constituency')) %>% 
  rename(district = district2016) %>% 
  calc_stats()



#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt


# [2] Pull turnout data ---------------------------------------------------
base_url = c("https://www.elections.org.zm/results/2016_presidential_election/constituency/")

# get unique constituencies, convert into url-compatible format
constits = pr16 %>% 
  select(constituency) %>% 
  distinct() %>% 
  pull(constituency)

# pull_turnout defined in `ZMB_E01_helpers.R`
# reads in website and pulls out the number of registered, cast, and rejected votes / constituency
pr16_total = lapply(constits, function(x) pull_turnout(base_url, x, 2016)) %>% 
  bind_rows() %>% 
  # merge geo data
  left_join(geo_base %>% select(province, district2016, constituency), by = c('constituency')) %>% 
  rename(district = district2016)


# merge # cast votes into candidate level info ------------------------------------
pr16 = merge_turnout(pr16, pr16_total)


# checks ------------------------------------------------------------------
# check website numbers agree w/ calcs for candidates
check_pct(pr16, incl_registered = FALSE)


# check website numbers agree w/ calcs for turnout
check_turnout(pr16_total, incl_rejected = FALSE)

# turnout and candidate info agree
check_constit(pr16, pr16_total)


# cleanup -----------------------------------------------------------------
pr16 = filter_candid(pr16)
pr16_total = filter_turnout(pr16_total)
