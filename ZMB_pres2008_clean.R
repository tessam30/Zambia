library(readxl)
library(dplyr)
library(stringr)
library(tidyr)


# import data -------------------------------------------------------------
# data pulled from https://www.elections.org.zm/media/2008_presidential_election_results.pdf
https://www.elections.org.zm/media/2006_presidential_results.pdf
# on 29 June 2017
# Exported from 

pres2011 = read_excel('rawdata/2011_presidential_election_results.xlsx', skip = 15)