library(rvest)
library(stringr)
base_url = 'http://www.parliament.gov.zm/node/'

first node: http://www.parliament.gov.zm/node/3
last node: http://www.parliament.gov.zm/node/5944

x = 3

pull_img = function(num) {
  if(num %% 50 == 0) {
  print(num)
  }
  
  # specify the url + the stub of candidate
  img = tryCatch(read_html(str_c(base_url, num)) %>% 
                   
                   
                   # get the image
                   # img = link  %>% 
                   html_nodes(xpath = '//*/img') %>% html_attr('src'),  error = function(e){NA})
  
  if(length(img) == 1){
    if(!is.na(img) & str_detect(str_to_lower(img), 'jpg')){
      download.file(img, destfile = paste0('~/Documents/Zambia/img/', num, '.jpg'))
    }
  }
}


link = read_html('http://www.parliament.gov.zm/members/previous-assembly')

link %>% html_nodes('.panel-col-last .inside , .views-field-title') %>% html_text()

lapply(3:5944, pull_img)

# get the name of the person
name = link %>% html_node('#page-title') %>% html_text()

# DOB
dob = link %>% html_node('.date-display-single') %>% html_text()

link %>% html_node('.view-content , .field-content') %>% html_text()



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
