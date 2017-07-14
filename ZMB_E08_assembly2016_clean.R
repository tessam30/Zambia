
# import Zambia 2016 NATIONAL ASSEMBLY ELECTIONS --------------------------
# Pull out three pieces of info from the consituency elections from 2016:
# https://www.elections.org.zm/results/2016_national_assembly_elections/
# 1. candidate/election results
# 2. overall poll stats (how many people voted, registered)
# 3. relationship b/w prov, district, constituency

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 28 June 2017


# setup -------------------------------------------------------------------
# # taken care of in ZMB_E01_helpers.R
# library(rvest)
# library(stringr)
# library(dplyr)
# library(tidyr)


# goals -------------------------------------------------------------------
# [0] Scrape assembly data from website
# [1] Pull out the votes by candidate by constituency --> as16
# [2] Get the relationship between the province, districts, constituencies
# [3] Pull the turnout by constituency --> as16_total



# Import constituencies ---------------------------------------------------

base_url = "https://www.elections.org.zm/results/2016_national_assembly_elections/constituency/"

constit = read.csv('~/Documents/GitHub/Zambia/processeddata/ZMB_constituencies.csv')

# convert to URLs to rvest:
constit = constit %>% 
  rowwise() %>% 
  mutate(cons = url_format(Constituency),
         url = paste0(base_url, cons))


# [0] Scrape data from website: Import data -------------------------------------------------------------
as16_raw = NULL
as16_total_raw = NULL
dists = NULL


# Note: more efficient to use lapply and function written in ZMB_E01_helpers.R
for (i in 1:nrow(constit)) {
  region = constit$Constituency[i]
  url = constit$url[i]
  
  print(as.character(region))
  
  html = read_html(url)
  
  # (1) get the voting totals ([[1]])
  vote = html %>% 
    # access table in html
    html_node('#table .table') %>% 
    # convert to a table
    html_table() %>% 
    # add in constituency name
    mutate(constituency = region)
  
  as16_raw = bind_rows(as16_raw, vote)
  
  # (2) get the turnout/registered
  reg = html %>% 
    html_node('#ptable .table') %>% 
    html_table(header = FALSE)
  
  reg = reg %>% mutate(constituency = region)
  
  as16_total_raw = bind_rows(as16_total_raw, reg)
  
  # (3) get the relationship b/w prov/dist/constituency
  dist = html_nodes(html, '.breadcrumb') %>% html_text()
  dists = bind_rows(dists, data.frame(dist))
}


# [2] clean up districts ----------------------------------------------------------
# unnest districts, provinces
dists = dists %>%
  separate(col = dist, sep = '\\n', into = c('natl', 'province', 'district', 'constituency', 'gunk')) %>% 
  select(-natl, -gunk)

dists = dists %>% 
  mutate(province = str_trim(str_to_title(str_replace_all(province, 'Province : ', ''))),
         district = str_trim(str_to_title(str_replace_all(district, 'District : ', ''))),
         constituency = str_trim(str_to_title(str_replace_all(constituency, 'Constituency : ', '')))
  )

# Export "official" list of constituencies with district and province names
write.csv(dists, '2016_geonames.csv')


# [3] clean turnout numbers ---------------------------------------------------


as16_total = as16_total_raw %>% 
  # swing registered long
  spread(X1, X2) %>% 
  mutate(year = 2016,
    registered = str2num(`Total Registered Voters`),
         cast = str2num(`Total Votes Cast`),
         rejected = str2num(`Total Votes Rejected`),
         turnout_web = str2pct(Turnout)) %>% 
  calc_turnout() %>% 
  # merge registered w/ district and province name
  left_join(dists, by = "constituency")

if(sum(is.na(as16_total$district)) > 0) {
  warning('merge of district names w/ registered votes did not work')
}



# [1] CALC VOTES PCT BY constit -------------------------------------------


# clean votes
as16 =  as16_raw %>% 
  split_candid('Candidate Name', sep = ', ') %>% 
  # merge votes w/ district and province name
  left_join(dists, by = "constituency") %>% 
  rename(party = Party) %>% 
  mutate(year = 2016,
         vote_count = str2num(Votes)) %>% 
  # calc percentages
  calc_stats() %>% 
  # calc pct_turnout
  merge_turnout(as16_total)



  if(sum(is.na(as16$district)) > 0) {
    warning('merge of district names w/ registered votes did not work')
  }



# checks ------------------------------------------------------------------
# check website totals agree
# Note: no percentages were calculated in the candidate-level breakdown
check_turnout(as16_total, incl_rejected = FALSE)

# collapse votes down to constituency level -------------------------------
# Check to make sure that the constitency-level individual vote tallies (in `votes`) 
# match with the summaries provided in `registered`

check_constit(as16, as16_total)


# filter out just the good stuff ------------------------------------------
as16 = filter_candid(as16)
as16_total = filter_turnout(as16_total)

rm(vote, reg, dists, constit, as16_total_raw, as16_raw)




