#Purpose: Get construction accident dataset
# Combine job applications with affordability areas,
# and with construction accidents,
# and analyze potential differences


#----------------------Libraries-----------------------------

library(tidyverse)
library(dplyr)
library(janitor) 
library(stringr)
library(purrr)
library(httr2)


#-----------------Get Construction Accident Data------------------

data_dictionary <- readxl::read_xlsx("C:/Users/kayli/Downloads/Construction-Related_Incidents_Data_Dictionary (1).xlsx",
                                     sheet = 2)


accident_data <- request("https://data.cityofnewyork.us/resource/bf97-mjsy.json") %>%
  req_url_query(
    `$select` = "address_bbl, record_type_description, check2_description, incident_date_mm_dd_yyyy, fatality, injury",
    `$limit` = 3000
  ) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE) %>%
  mutate(bbl = as.double(address_bbl)) %>%
  filter(!is.na(bbl))


# Check DOB Incident Data
accident_data %>%
  group_by(record_type_description, check2_description) %>%
  summarise(n = n())

# To expand project further: 
# self reported data goes back to 2017 on DOB website
# in form of xlsxs for each year and month
# could see about setting up scrape to grab XLSXs
# could also see how correlated Stop Work Orders are with workplace accidents and use that as a proxy



#-----------------------Get Affordability Area and Job Application Data-------

job_applications <- read_csv('job_applications_loi.csv') %>%
  filter(as.double(proposed_dwelling_units) > 0) 
# slim down dataset to just residential units

affordability_areas <- read_csv("bbls_affordability_zones.csv")

#-----------------------Combine Files---------------

# Join Geographic Data to Job Application Data
affordability_areas <- left_join(affordability_areas, 
                                 job_applications, 
                                 by = join_by(BBL == bbl))


# Check join
affordability_areas %>%
  group_by(ifelse(!is.na(filing_date_clean), 1, 0)) %>%
  summarise(n()) 

# Join Accident Data
affordability_areas <- affordability_areas %>%
  left_join(accident_data, by = join_by(BBL == bbl)) %>%
  distinct()

# Check accident Data
affordability_areas %>%
  group_by(record_type_description) %>%
  summarise(n()) 

##--------------------Prevailing Wage Requirements-------

#previaling wage levels vary upon size of building

affordability_areas <- affordability_areas %>%
  mutate( proposed_dwelling_units = as.numeric(proposed_dwelling_units),
    four85x_100_unit = if_else(proposed_dwelling_units > 99, 1, 0, missing = 0), 
         four85x_zone_a = if_else(proposed_dwelling_units > 149 & zone_a == 1, 1, 0, missing =  0),
         four85x_zone_b = if_else(proposed_dwelling_units > 149 & zone_b == 1, 1, 0, missing =  0),
         four21a_enhanced = if_else(proposed_dwelling_units > 299 & 
                                      (man_zone_a == 1 | queens_enhanced == 1 | brooklyn_enhanced == 1), 1, 0, missing = 0) 
                                    )
#----------------Data Checks---------------------------

# Count of projects/units in affordability zone

affordability_areas_count <- affordability_areas %>%
  mutate(proposed_dwelling_units  = as.double(proposed_dwelling_units),
         job_app = ifelse(is.na(proposed_dwelling_units) | proposed_dwelling_units == 1, 0, 1 )) %>%
  group_by(prev_wage, man_zone_a, zone_a, zone_b, brooklyn_enhanced, queens_enhanced) %>%
  summarise(n = n(), 
            sum_apps = sum(job_app, na.rm = T),
            sum_units = sum(proposed_dwelling_units, na.rm = T)) %>%
  mutate(percent = sum_apps / n) #calculate the projects out of potential bbls

# Results: 
# Queens has the highest % of applications per bbl, 
# but, Queens has bbls that were industrial factories turned into buildings, so not suprising
# zone B and zone a has tons of production, which makes sense, since Brooklyn has led in the most creation of units

is.Date(affordability_areas$incident_date_mm_dd_yyyy)

accident_data %>%
  mutate(incident_date_mm_dd_yyyy = ymd_hms(incident_date_mm_dd_yyyy)) %>%
  arrange(incident_date_mm_dd_yyyy) %>%
  select(incident_date_mm_dd_yyyy)

cons_job_apps <- affordability_areas %>%
  filter(!is.na(incident_date_mm_dd_yyyy) | !is.na(job_number) ) %>%
  mutate(no_job_app = ifelse(!is.na(incident_date_mm_dd_yyyy) & is.na(job_number), 1, 0), # check if an accident has no job app - probably was on an office building 
         no_accident = ifelse(is.na(incident_date_mm_dd_yyyy) & !is.na(job_number), 1, 0))  # most job apps don't affilated accidents

cons_job_apps %>%
  summarise(no_accident = mean(no_accident), # 97% of job applications have no affiliated accidents 
            no_job_app = sum(no_job_app)) #

# check by year
cons_job_apps %>%
  mutate(accident = ifelse(!is.na(incident_date_mm_dd_yyyy), 1, 0)) %>%
 # filter(filing_date_clean >= "2020-01-01") %>% # assume it takes at least two years for a building to get built from filing permits
  group_by(year(filing_date_clean)) %>% # buildings with permits later than 2014 still have some accidents
  summarise(no_accident = mean(no_accident), 
            accident = sum(accident)) 

#buildings with 2014 job applications still have accidents, check!
test <- cons_job_apps %>%
  filter(year(filing_date_clean) == 2014 & !is.na(incident_date_mm_dd_yyyy))
# manually checked certificate of occupancy available on DOB Now 
# year built or certificate of occupancy line up with active construction accidents
# so it's most likely fine! forge ahead

#also noticed these are buildings with multiple accidents within a few days of each other
# will be interesting to flag

#check types of construction accidents
affordability_areas$injury
type <- affordability_areas %>%
  mutate(across(c(injury, fatality), as.double)) %>%
  filter(!is.na(incident_date_mm_dd_yyyy) & !is.na(job_number)) %>% #get only accidents affilated with residential building construction
  group_by(record_type_description, check2_description) %>%
  summarise(n = n(),
            injury = sum(injury),
            fatality = sum(fatality))

# i can't tell the difference between a notification, accident, or injury, and all have associated injuries
# undefined in data dictionary or DOB websites
# so I'm going to use them interchangably 

check <- affordability_areas %>%
  filter(!is.na(incident_date_mm_dd_yyyy) & !is.na(job_number)) %>%
  filter(check2_description == "Worker Fell") # looks like injuries have multiple per accident, check if that makes sense

#-----------------------------Initial Analysis ---------



##------------------------------Counts--------------------
affordability_areas <- affordability_areas %>%
  mutate(across(c(injury, fatality), as.double),
         incident_date_mm_dd_yyyy = ymd_hms(incident_date_mm_dd_yyyy)) 


four21a_accidents <- affordability_areas %>%
  # filter if it's currently a 421a building or intending to use 421a/485x
  filter(!is.na(job_number) & (active_exemption == 1 | !is.na(reported_affordability_option))) %>%
  group_by(check2_description) %>%
  summarise(n = n(),
            injury = sum(injury),
            fatality = sum(fatality))

unique(affordability_areas$type)
four21a_accidents_detailed <- affordability_areas %>%
  # filter if it's currently a 421a building or intending to use 421a/485x
  filter(!is.na(job_number) & (active_exemption == 1 | !is.na(type))) %>%
  mutate(type = ifelse(is.na(type), "Active 421a", type)) %>%
  group_by(check2_description, type) %>%
  summarise(n = n(),
            injury = sum(injury),
            fatality = sum(fatality))

four21a_accidents_detailed_prev<- affordability_areas %>%
  # filter if it's currently a 421a building or intending to use 421a/485x
  filter(!is.na(job_number) & (active_exemption == 1 | !is.na(type))) %>%
  mutate(type = ifelse(is.na(type), "Active 421a", type)) %>%
  group_by(prev_wage, check2_description, type) %>%
  summarise(n = n(),
            injury = sum(injury),
            fatality = sum(fatality)) 
#looks like prevailing wage areas have more accidents than non-prevailing wage buildings 
# could be a reporting issue? in that prevailing wage buildings are more likely to report injuries

##-----------Correlation and Linear Regressions---------------
# check correlations 
affordability_areas$type
correlation <- affordability_areas %>%
  filter(!is.na(job_number) & (active_exemption == 1 | !is.na(type))) %>%
  mutate( across(c(active_exemption, fatality, injury), ~ifelse(is.na(.), 0, .)),
          four85x_letter = if_else(type == "485x", 1, 0, missing = 0),
          four21a_letter = if_else(type == "421a(16)", 1, 0, missing = 0))%>%
  select(man_zone_a, zone_a:prev_wage, four85x_100_unit:four21a_enhanced, active_exemption, four85x_letter, four21a_letter, fatality, injury) 

correlation <- cor(correlation)
 # interestingly, prevailing wage sometimes negatively correlated with fatalities
# but slightly postively correlated with injuries
# could be proof of reporting issue

# again, most residential construction uses the 421a tax exemption
# so most buildings probably will
# let's see if that makes a differ
correlation_no_letters <- affordability_areas %>%
  filter(!is.na(job_number)) %>%
  mutate( across(c(active_exemption, fatality, injury), ~ifelse(is.na(.), 0, .)),
          four85x_letter = if_else(type == "485x", 1, 0, missing = 0),
          four21a_letter = if_else(type == "421a(16)", 1, 0, missing = 0))%>%
  select(man_zone_a, zone_a:prev_wage, active_exemption, four85x_100_unit:four21a_enhanced,  four85x_letter, four21a_letter, fatality, injury)  %>%
  cor()

# mixed results again
# still potential reporting issue
# most likely not statistically signifigant

# check if statistically significant differences

# narrow to active 485x applications
affordability_areas_485_x <- affordability_areas %>%
  filter(type == "485x") 
# no buildings that have submitted a letter of intent are subject to the prevailing wage requirements
# about 6 injuries
# not going to run regression

#for registered 421a(16) buildings
affordability_areas_421a <- affordability_areas %>%
  filter(type == "421a(16)")  
  
# not statistically significant
summary(lm(data = affordability_areas_421a,
           injury ~ four21a_enhanced + prev_wage))

# not statistically significant either
summary(lm(data = affordability_areas_421a,
           fatality ~ four21a_enhanced + prev_wage))

# for 421a buildings

affordability_areas_421a_active <- affordability_areas %>%
  filter(active_exemption == 1)

summary(lm(data = affordability_areas_421a_active,
           injury ~ four21a_enhanced + prev_wage))

# for buildings that are still in process and have not filed a letter of intent
# assume use of 485x 

affordability_areas_no_letter <- affordability_areas %>%
  filter((active_exemption == 0 | is.na(active_exemption)) & 
           is.na(type))

summary(lm(data = affordability_areas_421a_active,
           injury ~ four85x_100_unit + four85x_zone_a + four85x_zone_b)
)
#not statistically signifigant, 100+ unit buildings have a positive sign but meaningless coefficient