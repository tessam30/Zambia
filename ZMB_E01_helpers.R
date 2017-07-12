
# Helper functions to clean up Zambia election data -----------------------
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 12 July 2017


# convert strings to numbers ----------------------------------------------
str2num = function(value){
  as.numeric(str_replace_all(value, ',', ''))
}


# summarise and calc pct of total votes -----------------------------------
calc_pctVotes = function(df, group1 = 'party', group2 = 'year') {
  summ = df %>% 
    group_by_(group1, group2) %>% 
    summarise(tot_votes = sum(vote_count)) %>% 
    ungroup() %>% 
    group_by_(group2) %>%
    mutate(pct = tot_votes / sum(tot_votes)) %>% 
    arrange(desc(pct))
    
  return(summ)
}


# convert any strings to nice title case sans spaces ----------------------
pretty_strings = function(string) {
  # df = df %>% 
  #   mutate_(.dots = setNames(list(paste0('stringr::str_to_title(stringr::str_trim(', column, "))")), column))
  # 
  # return(df)
  
  stringr::str_to_title(stringr::str_trim(string))
}



# split candidate name into first name, last name, middle, and rejoin --------
# candidate name given LAST FIRST MIDDLE
split_candid = function(df, column = 'candid', sep = ' ') {
  df %>% 
    separate_(col = column, into = c('last_name', 'first_name', 'middle_name'), sep = sep) %>% 
    mutate(first_name = paste(pretty_strings(first_name), pretty_strings(middle_name)),
           last_name = pretty_strings(last_name),
           candidate = paste(first_name, last_name)) %>% 
    select(-middle_name)
}

