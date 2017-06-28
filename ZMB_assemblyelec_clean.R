
# import Zambia 2016 NATIONAL ASSEMBLY ELECTIONS --------------------------
# Pull out three pieces of info from the consituency elections from 2016:
# https://www.elections.org.zm/results/2016_national_assembly_elections/
# 1. candidate/election results
# 2. overall poll stats (how many people voted, registered)
# 3. relationship b/w prov, district, constituency

# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 28 June 2017


# setup -------------------------------------------------------------------
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)


# Import constituencies ---------------------------------------------------

base_url = "https://www.elections.org.zm/results/2016_national_assembly_elections/constituency/"

constit = read.csv('~/Documents/Zambia/rawdata/ZMB_constituencies.csv')

# convert to URLs to rvest:
constit = constit %>% 
  rowwise() %>% 
  mutate(cons = str_replace_all(str_to_lower(Constituency),  ' ', '_'),
         cons = str_replace_all(cons, "'", '%2527'),
         url = paste0(base_url, cons))


# Import data -------------------------------------------------------------
votes = NULL
registered = NULL
dists = NULL


for (i in 1:nrow(constit)) {
  region = constit$Constituency[i]
  url = constit$url[i]
  
  print(region)

  html = read_html(url)
  
  # (1) get the voting totals ([[1]])
  vote = html %>% 
    html_node('#table .table') %>% 
    html_table()
  
  vote = vote %>% mutate(constituency = region)
  
  votes = bind_rows(votes, vote)
  
  # (2) get the registered
  reg = html %>% 
    html_node('#ptable .table') %>% 
    html_table(header = FALSE)
  
  reg = reg %>% mutate(constituency = region)
  
  registered = bind_rows(registered, reg)
  
  # (3) get the relationship b/w prov/dist/constituency
  dist = html_nodes(html, '.breadcrumb') %>% html_text()
  dists = bind_rows(dists, data.frame(dist))
}


# clean up stuff ----------------------------------------------------------
# unnest districts, provinces
dists = dists %>%
  separate(col = dist, sep = '\\n', into = c('natl', 'province', 'district', 'constituency', 'gunk')) %>% 
  select(-natl, -gunk)

dists = dists %>% 
  mutate(province = str_trim(str_to_title(str_replace_all(province, 'Province : ', ''))),
         district = str_trim(str_to_title(str_replace_all(district, 'District : ', ''))),
         constituency = str_trim(str_to_title(str_replace_all(constituency, 'Constituency : ', '')))
         )

# swing registered long
registered = registered %>% 
  spread(X1, X2)

registered = registered %>% mutate(registered = as.numeric(str_replace_all(`Total Registered Voters`, ',', '')),
                      cast = as.numeric(str_replace_all(`Total Votes Cast`, ',', '')),
                      rejected = as.numeric(str_replace_all(`Total Votes Rejected`, ',', '')),
                      valid_votes = cast - rejected,
                      turnout = cast / registered,
                      pct_rejected = rejected/cast)

# merge registered w/ district and province name
registered = left_join(registered, dists, by = "constituency")

if(sum(is.na(registered$district)) > 0) {
  warning('merge of district names w/ registered votes did not work')
}


# clean votes
votes = votes %>% 
  separate(`Candidate Name`, sep = ', ', into = c('lastname', 'firstname')) %>% 
  mutate(candidate = str_to_title(paste(firstname, lastname, sep = ' ')),
         votes = as.numeric(str_replace_all(Votes, ',', '')))

# merge votes w/ district and province name

votes = left_join(votes, dists, by = "constituency")

if(sum(is.na(votes$district)) > 0) {
  warning('merge of district names w/ registered votes did not work')
}



# collapse votes down to constituency level -------------------------------
# Check to make sure that the constitency-level individual vote tallies (in `votes`) 
# match with the summaries provided in `registered`

vote_sum = votes %>% group_by(province, district, constituency) %>% summarise(votes_indiv = sum(votes))

vote_check = full_join(vote_sum, registered, by = c("province", "district", "constituency"))

vote_check = vote_check %>% 
  select(province, district, constituency, cast, rejected, valid_votes, votes_indiv) %>% 
  mutate(notEqual = valid_votes != votes_indiv)

if(sum(vote_check$notEqual) > 0){
  warning('vote tallies do not match for some constituencies')
}
