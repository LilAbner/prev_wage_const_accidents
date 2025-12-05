#---------------New Building Alterations and Letters of Intent--------

#Purpose:
# 485x/421a(16) will be new buildings that filed the job application to build a new building 
# with Department of Buildings most likely after 2022 
# They may have already sent in letters of intent to Housing Preservation and Development
# But, it's possible that they have not sent in the letter of intent
# Most new buildings will probably use 485x or 421a(16), so we can also use all new buildings 
# and be reasonably confident the majority will use 485x or 421a(16). 


#----------------------Libraries-----------------------------

library(tidyverse)
library(dplyr)
library(janitor) 
library(stringr)
library(purrr)
library(httr2)

#--------------------Datasources-----------------------------------

# DOB Job Applications
# DOB used to use an older software but switch to DOB Now by 2022
# Most new building permits will be in DOB Now

dob_now_job_applications <- request("https://data.cityofnewyork.us/resource/w9ak-ipjd.json") %>%
  req_url_query(
    `$select` = "bbl, job_filing_number, job_type, filing_date, approved_date, proposed_dwelling_units",
    `$limit` = 5000000,
    `$where` = "job_type  in ('New Building', 'ALT-CO - New Building with Existing Elements to Remain')"
  ) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

''
dob_job_applications <- request("https://data.cityofnewyork.us/resource/ic3t-wcy2.json") %>%
  req_url_query(
    `$select` = "borough, block, lot, job__, doc__, job_type, proposed_dwelling_units, pre__filing_date, fully_permitted, withdrawal_flag",
    `$limit` = 5000000,
    `$where` = "job_type in ('NB')"
  ) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

# Letters of Intent
# these are optional and can be filed after the building is built and eligible
four85x_letters_of_intent <- request("https://data.cityofnewyork.us/resource/rrtd-iyd7.json") %>%
  req_url_query(
  `$select` = "presumed_bbl, reported_units, reported_restricted_units, reported_affordability_option,  duplicate_count, form_submission_date",
  `$limit` = 1000
) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

four21a_letters_of_intent <- request("https://data.cityofnewyork.us/resource/pq4c-wbq4.json") %>%
  req_url_query(
    `$select` = "bbl, reported_units, reported_affordale_units, reported_affordability_option, form_submission_date",
    `$limit` = 1000
  ) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

#get buildings with active 421a tax exemptions (for buildings that completed construction)

four21a_codes <- c("5110","5110-C","5113","5113-C","5114","5114-C",
"5116","5116-C","5117","5117-C","5118","5118-C",
"5119","5120","5121","5122","5123")
where_clause <- paste0("exmp_code in ('", paste(four21a_codes, collapse="','"), "')")


active_421a_tax_exemptions <- request("https://data.cityofnewyork.us/resource/muvi-b6kx.json") %>%
  req_url_query(`$select` = "parid",
                `$limit` = 1000000,
                `$where` = where_clause) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)
  



#-------------------------Combining and Cleaning Permits------------

# Most new buildings that will likely use 485X will file the job application in DOB Now
# it's possible they started the initial planning way before 2022, especially after covid
# so use both apps


dob_job_applications_clean <- dob_job_applications %>%
  mutate(filing_date_clean = mdy(pre__filing_date),
         filing_date_clean = if_else(is.na(filing_date_clean),
                                     ymd(pre__filing_date),filing_date_clean ),
         doc__ = as.numeric(doc__),
         boro = case_when(borough == 'MANHATTAN' ~ 1,
                          borough == "BRONX" ~ 2,
                          borough == "BROOKLYN" ~ 3, 
                          borough == "QUEENS" ~ 4,
                          T ~ 5),
         block = as.numeric(block),
         lot = as.numeric(lot),
         bbl = boro*1000000000 + block*10000 + lot
         ) %>%
  filter(filing_date_clean >= "2014-01-01" #interested in buildings built after 421a(16 passed), assume permits filed ~ two years below passage
         & doc__ == 1  #take only first job application for each project
         ) %>% 
  select(bbl, filing_date_clean, proposed_dwelling_units, fully_permitted, withdrawal_flag, job_number = job__)


dob_now_job_applications_clean <-dob_now_job_applications %>%
  mutate(filing_date_clean = as_date(ymd_hms(filing_date)),
         doc__ = str_sub(job_filing_number, start = -2), #get the document number
         bbl = as.numeric(bbl)) %>%
  filter(doc__ == "I1"  # get the main document instead of secondary applications
           ) %>% 
  select(bbl, filing_date_clean, proposed_dwelling_units, approved_date, job_number = job_filing_number)

# Combine Both Systems
job_applications <- bind_rows(dob_now_job_applications_clean, dob_job_applications_clean) %>%
  filter(!is.na(bbl) & !is.na(filing_date_clean))

#-----------------------485x and 421a-----------

active_421a_tax_exemptions <- active_421a_tax_exemptions %>%
  mutate(active_exemption = 1,
         parid = as.numeric(parid)) %>%
  rename(bbl = parid)
active_421a_tax_exemptions <- active_421a_tax_exemptions %>%
  distinct()


four21a_letters_of_intent <- four21a_letters_of_intent %>%
  filter(!is.na(bbl)) %>%
  group_by(bbl) %>%
  slice(1) %>%
  rename(reported_restricted_units = reported_affordale_units ) %>%
  mutate(type = "421a(16)")


four85x_letters_of_intent <- four85x_letters_of_intent %>%
  mutate(type = "485x",
         bbl = presumed_bbl) %>%
  filter(!is.na(bbl)) %>%
  group_by(bbl) %>%
  slice(1) %>%
  select(-presumed_bbl, -duplicate_count)

letters_of_intent <- bind_rows(four85x_letters_of_intent, four21a_letters_of_intent) %>%
  mutate(bbl = as.numeric(bbl)) %>%
  group_by(bbl) %>%
  mutate(four85x_supersed = #8 bbls have applied for both a 485x and a 421a(16)
           # mark those
           # and then keep the 485x letters
           if_else(any(type == "485x") & any(type == "421a(16)"), 1, 0, missing = 0),
         count = ifelse(type == "485x", 1, 0)) %>%
  slice_max(count, n = 1) %>%
  select(-count)
         

test <- letters_of_intent %>%
  group_by(bbl) %>%
  filter(n() >1)
#---------------------Combine Job Applications with 421a and 485x data--------

job_applications <- job_applications %>%
  left_join(active_421a_tax_exemptions) %>%
  left_join(letters_of_intent)

# Check Joins

 job_applications %>%
  summarise(active_exemption = sum(active_exemption, na.rm = T))


 job_applications %>%
   group_by(reported_affordability_option) %>%
   summarise(n())
 
 job_applications %>%
   filter(active_exemption == 1 & !is.na(type))
 
#-----------------------Export Dataset-----------------
 
 job_applications %>%
   write_csv("job_applications_loi.csv")
 
 