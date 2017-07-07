
# Import Zambia DRG Fellows file ----------------------------
# Prepares the data to be mapped by geocoding the cities
# Tim Essam, USAID | GeoCenter, 6 July 2017, tessam@usaid.gov

# setup -------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(foreign)
library(readxl)
library(knitr)
library(data.table)
library(ggmap)
library(leaflet)
library(htmlwidgets)

# Set Yali file target and get names in the Excel file
  base_dir = '~/Zambia/'
  yali = str_c(base_dir, 'ZMB_DRG_Yali_fellows.xls')
  excel_sheets(path = yali)
  
# Read in first file to location of participants
  df = read_excel(yali, 'RLC')
  names(df)
  
  df2 = df %>% 
    mutate(location = str_c(City, ", ", Country)) %>% 
    group_by(location) %>%
    summarise(n = n()) %>% 
    na.omit()
  
# Geocode the cities, bind them back to location data and plot for accuracy
  loc = geocode(df2$location, source = "google")
  loc_df = cbind(df2, loc) %>% 
    separate(location, c("City", "Country"), ", ")
  
  leaflet(loc_df) %>%
      addCircles(lng = ~lon, lat = ~lat) %>% 
    addProviderTiles(providers$CartoDB.Positron)

# Merge back in with dataframe
  rlc = left_join(df, loc_df, by = c("City" = "City"))
  write_csv(rlc, str_c(base_dir, "RLC_geocoded.csv"))
  
# ------------------------------------------------------------  
  
# Repeat with MFW_Alums but we need universities now
  df3 = read_excel(yali, "MWF_alumns") 
  
  df4 = left_join(df3, loc_df, by = c("Mailing_city" = "City"))

# Get a listing of locations to be geocoded  
univ = df3 %>% 
  mutate(univ_location = str_c(Host_university_std, ", " , "United States")) %>% 
  group_by(univ_location) %>% 
  summarise(n = n()) %>% 
  na.omit() 

  # Geocode locations
  univ_loc = geocode(univ$univ_location, source = "google") 
  
  # Bind the results back to the original list, splay out the location field
  univ_df = cbind(univ, univ_loc) %>% 
    separate(univ_location, c("University", "Country"), ", ") %>% 
    rename(lon_univ = lon, lat_univ = lat)
  
  # Check geocoding
  leaflet(univ_loc) %>% 
    addCircles(lng = ~lon, lat = ~lat) %>% 
    addProviderTiles(providers$CartoDB.Positron)
  
  # Merge everything back together and save as a .csv
  mwf_alumns = left_join(df4, univ_df, by = c("Host_university_std" = "University"))
  write_csv(mwf_alumns, str_c(base_dir, "mwf_alumns.csv"))

  # clean up unused objects
  rm(df3)

  
  