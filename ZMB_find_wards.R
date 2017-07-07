
# Find all wards within Zambia --------------------------------------------
# Shape files of wards in Zambia are a bit messy, so constructing a lookup table of province, district, 
# constituencies, wards, and polling places
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 7 July 2017



# procedure ---------------------------------------------------------------
# 1. Previously: in ZMB_assembly2016_clean.R, scraped all the constituencies to back out their distr/prov
# 2. Using these constituency names, pull up the constituency-level data
# 3. Within this website, the "Polling Stations in <Constituency>" drop-down menu contains all the ward names.  Scrape it!
# 4. Clean and pull out just the pertinent data

# setup -------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(rvest)

base_dir = '~/Documents/GitHub/Zambia/'

# previous lookup table: prov, dist, constit ------------------------------
constit = read_csv( paste0(base_dir, 'processeddata/ZMB_2016_adminnames.csv')) %>% select(-X1)


# Polling-level website ---------------------------------------------------


# URL for getting the polling station level data:
# example: https://www.elections.org.zm/results/2016_presidential_election/poll/ambidzi,chadiza
# end is /poll/ward,district
# base_poll_url = "https://www.elections.org.zm/results/2016_presidential_election/poll/"
base_poll_url =  "/results/2016_presidential_election/poll/"

# Pull tables of wards per constituency -----------------------------------
base_url = 'https://www.elections.org.zm/results/2016_presidential_election/constituency/'

constit = constit %>% 
  mutate(url = paste0(base_url, str_to_lower(constituency)),
         url = str_replace_all(url, ' ', '_'),
         url = str_replace_all(url,"'", '%2527'))

wards = NULL

# Note: would be faster to create function and lapply it across the list.
for (i in 1:nrow(constit)) {
  print(paste0(i, ': ', round(i/nrow(constit), 2) * 100, '%'))
  
  url = constit$url[i]
  
  cons = constit$constituency[i]
  dist = constit$district[i]
  prov = constit$province[i]
  
  html = read_html(url)
  
  # HTML text from the form i
  # Use html_form to pull out the form data, which will get both the 
  # 1) URL like   "/results/2016_presidential_election/poll/lukundushi,mansa"
  # 2) ward name / polling place like "MANO/Mano Primary School"
  
  messy_wards = html %>%
    html_node('#districts-select') %>% 
    html_form()
  
  # pull out just the options from the form
  messy_wards = data.frame(messy_wards$fields$`NULL`$options)
  
  # clean up the wards
  
  ward = messy_wards %>% 
    rename(ward_url = messy_wards.fields..NULL..options) %>% 
    mutate(ward_polling = row.names(messy_wards),
           constituency = cons,
           province = prov,
           district = dist,
           dist_fromurl = str_replace_all(ward_url, base_poll_url, '')) %>% 
    # split ward / polling location into two columns
    separate(ward_polling, sep = "\\/", into = c('ward', 'polling_station'), remove = FALSE) %>%
    # split URL into ward and district
    separate(dist_fromurl, sep = '\\,', into = c('ward_fromurl', 'dist_fromurl')) %>% 
    mutate(ward = str_to_title(ward),
           ward_fromurl = str_to_title(ward_fromurl),
           dist_fromurl = str_to_title(dist_fromurl))
  
  wards = bind_rows(wards, ward)
}


wards = wards %>% filter(ward != "")


# Merge with previous crosswalk -------------------------------------------
crswlk = read_excel('ZMB_admin_crosswalk.xlsx', sheet = 2)

crswlk = left_join(wards, crswlk, by = c("constituency", "province", "district"))

write_csv(crswlk, 'ZMB_admin_wards_crosswalk.csv')
