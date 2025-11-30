#---------------New Building Alterations and Letters of Intent--------

#Purpose:
# 485x will be new buildings that filed with Department of Buildings most likely after 2022 
# They may have already sent in letters of intent to Housing Preservation and Development
# But, it's possible that they have not sent in the letter of intent
# Most new buildings will probably use 485x, so we can also use all new buildings 
# and be reasonably confident the majority will use 485x. 

#----------------------Libraries-----------------

library(tidyverse)
library(dplyr)
library(janitor) 
library(stringr)
library(purrr)
library(httr2)

#--------------------Datasources---------------------------

# DOB Job Applications
# DOB used to use an older software but switch to DOB Now by 2022
# Most new building permits will be in DOB Now

dob_now_job_applications <- request("https://data.cityofnewyork.us/resource/w9ak-ipjd.json") %>%
  req_url_query(
    `$select` = "bbl, job_filing_number, job_type, filing_date, approved_date, proposed_dwelling_units",
    `$limit` = 5000000
  ) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)


dob_now_job_applications <- request("https://data.cityofnewyork.us/resource/ic3t-wcy2.json") %>%
  req_url_query(
    `$select` = "borough, block, lot, job_, doc_, job_type, proposed_dwelling_units",
    `$limit` = 5000000
  ) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

