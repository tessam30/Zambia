library(readxl)
library(dplyr)
library(stringr)
library(tidyr)


# import data -------------------------------------------------------------
# data pulled from https://www.elections.org.zm/media/2006_presidential_results.pdf
# on 29 June 2017
# Exported from Adobe Acrobat into Excel to begin the cleaning process

pres06_raw = read_excel('rawdata/2006_presidential_results.xlsx', skip = 7)

# check if there's data in each column
pres06_raw %>% summarise_all(funs(sum(!is.na(.))))
t(pres06_raw %>% summarise_all(funs(length(unique(.)))))

# Pull out the stats for the votes per district
pres06 = pres06_raw %>% mutate(province = PROVINCE,
                  district = COUNCIL, 
                  constituency = `CONSTITUENCY NO./NAME`) %>% 
  # back fill province/district locations
  fill(province) %>% 
  fill(district) %>% 
  # remove "total" rows
  filter(!is.na(PARTY)) %>% 
  select(party = PARTY)
