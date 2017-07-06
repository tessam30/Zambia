
# Import Zambia World Bank Poverty Mapping Results ----------------------------
# Imports shape with 1421 Wards and creates a crosswalk and join
# Tim Essam, USAID | GeoCenter, 5 July 2017, tessam@usaid.gov


# Sources -----------------------------------------------------------------

# 1) Zambia 2016 Constituencies source: https://github.com/lightonphiri/data-zambia-shapefiles/tree/master/cso-shapefiles
# Based on: http://lightonphiri.org/blog/mapping-the-zambia-2016-presidential-election-results
# 2) World Bank data have been scraped from PDF report.


# setup -------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(foreign)
library(readxl)
library(knitr)
library(data.table)

base_dir = '~/Zambia/'

# Import the .dbf with the Ward names and the corresponding Excel file; turn off factors in read.dbf
  dbf = str_c(base_dir, 'data-zambia-shapefiles-master/cso-shapefiles/wards/pop_Ward_Exported.dbf')
  df1 <- read.dbf(dbf, as.is = TRUE) 
  
  df1 %>% group_by(WARD_NAME) %>% 
    summarise(n = n()) %>% 
    filter(n >1)
  
  # Repeat on poverty mapping dataset and corresponding crosswalk
  exldf = str_c(base_dir, "ZMB_PovertyMapping_Data.xls")
  df2 = read_excel(exldf, sheet = "Ward_poverty")
  
  # Many of the Wards in the Zambezi region are mislabled in the District Field. 
  # Working on a remedy to standardize the District -- Ward relationship so the merge is proper
  
  df2 = df2 %>% 
    mutate(Province =  ifelse(Province == "North Western", "North-Western", Province),
           District2 = case_when(df2$District %like% 'Kapiri-Mposhi' ~ 'Kapiri Mposhi', 
                                TRUE ~ NA_character_)) %>% 
    mutate(District2 = ifelse(is.na(District2), District, District2)) 
           
  cw = read_excel(exldf, sheet = "Crosswalk")
  
      
  # Join in crosswalk and fix up mismatched wards
  df2 = left_join(df2, cw, by = c("Ward" = "PovWard")) %>% 
    mutate(WARD_NAME = ifelse(is.na(ShapeWard), Ward, ShapeWard)) %>% 
    rename(PROVINCENA = Province.x, DISTRICTNA = District2) %>% 
    select(-(ShapeWard:Reference))


  povdf = left_join(df1, df2) 
  
  
# What wards do not match across the two data sets?
  kable(setdiff(df1$DISTRICTNA, df2$DISTRICTNA), col.names = "df1 not in df2")
  kable(setdiff(df2$DISTRICTNA, df1$DISTRICTNA), col.names = "df2 not in df1")
  
  
  

  
