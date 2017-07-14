
# Scrape 2015 Zambia Election results -------------------------------------
# Build off of Tim Essam's code (tessam@usaid.gov)
# Validated and modified by Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 3 July 2017


# setup -------------------------------------------------------------------

# taken care of in ZMB_E01_helpers.R
# library(rvest)
# library(tidyverse)
# library(stringr)
# library(tidyr)


# Goals -------------------------------------------------------------------
# 1. Pull all the districts, provinces, and constituencies from website
# 2. Pull the voting totals by candidate
# 3. Pull the turnout by constituency
# 4. Calc stats, cleanup


# [1] Get the province-district-constituencies ----------------------------
base_url = c("https://www.elections.org.zm/results/2015_presidential_election/constituency/")
constits = geo_base %>% filter(!is.na(website2015)) %>%  pull(website2015)

geo15 = lapply(constits, function(x) pull_dists(base_url, x)) %>% 
  bind_rows() %>% 
  rename(website2015 = constituency)

# merge with the base, to get the "official" 2016 constituency names
geo15 = full_join( geo_base %>% select(constituency, website2015), geo15, by = 'website2015')

# write.csv(geo15, '2015_geonames.csv')

# [2] Set up candidates -------------------------------------------------------

# Url information
cand_prof = c("lungu,edgar,pf", "hichilema,hakainde,upnd", "nawakwi,edith,fdd", 
               "mumba,nevers,mmd","kaunda,tilyenji,unip", "chanda,eric,4r",
               "chipimo,elias,narep", "miyanda,godfrey,heritage", "pule,daniel,cdp",
               "sondashi,ludwig,fda", "sinkamba,peter,greens")

base_url = c("https://www.elections.org.zm/results/2015_presidential_election/candidate/")
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
pr15 = lapply(cand_prof, votes) %>% 
  bind_rows() %>% 
  separate(link, c("last_name", "first_name", "Party")) %>%
  mutate(year = 2015,
         website2015 = str_to_title(str_replace_all(str_trim(constName), "[\r\n]" , "")),
         last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         candidate = paste(first_name, last_name),
         pct_cast_web = pct_cast_web/100,
         party = str_to_upper(Party)) %>% 
  left_join(geo15, by = c('website2015')) %>%
  calc_stats()



#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt




# [2] Pull turnout data ---------------------------------------------------
base_url = c("https://www.elections.org.zm/results/2015_presidential_election/constituency/")


# pull_turnout defined in `ZMB_E01_helpers.R`
pr15_total = lapply(constits, function(x) pull_turnout(base_url, x, 2015)) %>% 
  bind_rows() %>% 
  rename(website2015 = constituency) %>% 
  # merge geo data
  left_join(geo15, by = c('website2015'))


# merge cast into candidate level info ------------------------------------
pr15 = merge_turnout(pr15, pr15_total)


# checks ------------------------------------------------------------------
# check website numbers agree w/ calcs for candidates
check_pct(pr15, incl_registered = FALSE, ndigits = 3)


# check website numbers agree w/ calcs for turnout
check_turnout(pr15_total, incl_rejected = FALSE)

# turnout and candidate info agree
check_constit(pr15, pr15_total)


# cleanup -----------------------------------------------------------------
pr15 = filter_candid(pr15)
pr15_total = filter_turnout(pr15_total)

#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt
