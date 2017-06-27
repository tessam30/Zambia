library(rvest)
library(tidyverse)
library(stringr)
library(foreign)
library(stringi)


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
             vote_pct = parse_number(V3), 
             link = x, 
             constName = parse_character(V1))
    
    return(df)
  }

# Call functino and store in a new dataframe
  vote_results <- lapply(cand_prof, votes) %>% 
    bind_rows() %>% 
    separate(link, c("last_name", "first_name", "Party")) %>%
    mutate(constName = stri_trans_totitle(str_replace_all(str_trim(constName), "[\r\n]" , "")))
  
    geo_const <-  vote_results %>% 
      group_by(constName) %>% 
      count()  

  geo <- read.dbf("~/Zambia/ZMB_Constituency_2010/Zambia_ConstituenciesAndWards_2006to2010_UTM35S.dbf")

  # Read in constituency crosswalk
  cw <- read_csv("~/Zambia/ZMB_constituency_crosswalk_new_old.csv")
  
  tmp = geo_const2 %>% 
    left_join(cw, c("constName" = "old")) %>% 
    mutate(constName = ifelse(is.na(new), constName, new))
  
  
  
  
  
# Count the number of constituencies in shapefile
  geo_const2 <- geo %>% 
    group_by(constName) %>% 
    count()

# Check the constituencies in the election data (150 according to sources)
  setdiff(geo_const$constName, geo_const2$constName)  
  intersect(geo_const$constName, geo_const2$constName)

  full_dist <- full_join(geo_const, geo_const2, by = "constName")
  
  # Check another shapefile with constituencies
  geo2 <- read.dbf("~/Zambia/GRED_beta2_20170530_Zambia/shapefile/GRED_Zambia_2006_beta2.dbf") %>% 
    rename(constName = CST_N)
  
  setdiff(geo_const$constName, geo2$constName)
  tmp <-  full_join(geo_const, geo2, by = "constName")
  





#http://lightonphiri.org/blog/visualising-the-zambia-2015-presidential-by-election-results
#http://documents.worldbank.org/curated/en/766931468137977527/text/952760WP0Mappi0mbia0Report00PUBLIC0.txt
