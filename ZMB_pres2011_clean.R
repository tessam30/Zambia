library(readxl)
library(dplyr)
library(stringr)
library(tidyr)


# import data -------------------------------------------------------------
# data pulled from http://www.elections.org.zm/media/28092011_public_notice_-_2011_presidential_election_results.pdf
# on 29 June 2017
# Exported from Adobe Acrobat into Excel to begin the cleaning process

pres2011 = read_excel('rawdata/2011_presidential_election_results.xlsx', skip = 15)

