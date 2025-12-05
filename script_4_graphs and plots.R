

#------------Slidedeck Visualizations------------

library(tidyverse)
library(ggplot2)


library(ggspatial)
library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(arcgis)

#-----------------Data from "combine and anaylze.r" file

source("combine and anaylze.r")


# Borough Map (for looks)
nyc_borough_map <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0"
nyc_borough <- arcgislayers::arc_open(nyc_borough_map) 

#--------------------Job Applications-----


# look at job applications of building applying for 421 
# and that could apply for 421a

potential_485_x <- affordability_areas %>%
  filter(filing_date_clean > "2020-01-01" & (active_exemption == 0 | is.na(active_exemption)) & (type != "421a" | is.na(type)
           )) %>%
  mutate(wage_type = case_when(prev_wage == 0 & four85x_100_unit == 0 ~ "No Prevailing Wage",
                          four85x_zone_a == 1 | four85x_zone_b == 1  ~ "Higher Prevailing Wages, based on Geography + Unit Size",
                          four85x_100_unit == 1 ~ "Prevailing Wage based on Unit Size",
                          T ~ "No Prevailing Wage"
  )) %>%
  group_by(wage_type, prev_wage, four85x_100_unit, four85x_zone_a, four85x_zone_b) %>%
  summarise(n = n_distinct(BBL))

wage_levels = c("No Prevailing Wage", "Prevailing Wage based on Unit Size", "Higher Prevailing Wages, based on Geography + Unit Size")


potential_485_x_bar_plot <- potential_485_x %>%
  mutate(wage_type = factor(wage_type, levels = wage_levels)) %>%
  arrange(wage_type) %>%
  mutate(sub_type = case_when(four85x_zone_a == 1 ~ "Zone A",
                              four85x_zone_b == 1 ~ "Zone B", 
                              prev_wage == 1 ~ "In Geographic Area",
                              T ~ "Not In Geographic Area") ) %>%
    ggplot(aes(x = wage_type, y = n, fill = sub_type)) + 
  geom_col() + 
  theme_minimal()
# takeaway: regardless of geographic area, most not using prevailing wage



potential_485_x_boro <- affordability_areas %>%
  filter(filing_date_clean > "2020-01-01" & (active_exemption == 0 | is.na(active_exemption)) & (type != "421a" | is.na(type)
  )) %>%
  mutate(wage_type = case_when(prev_wage == 0 & four85x_100_unit == 0 ~ "No Prevailing Wage",
                               four85x_zone_a == 1 | four85x_zone_b == 1  ~ "Higher Prevailing Wages, based on Geography + Unit Size",
                               four85x_100_unit == 1 ~ "Prevailing Wage based on Unit Size",
                               T ~ "No Prevailing Wage"
  )) %>%
  group_by(Borough, wage_type) %>%
  summarise(n = n_distinct(BBL))


# Prevailing Wage 

split_wage <- affordability_areas %>%
  filter(filing_date_clean > "2020-01-01" & (active_exemption == 0 | is.na(active_exemption)) & (type != "421a" | is.na(type)
  )) %>%
  mutate(split = case_when(proposed_dwelling_units == 99 ~ "99 Units",
                           proposed_dwelling_units < 99 & proposed_dwelling_units > 94 ~ "95-98 Units",
                           proposed_dwelling_units == 149 & prev_wage == 1 ~ "149 Units",
                           proposed_dwelling_units < 149 & proposed_dwelling_units > 144 & prev_wage == 1 ~ "145-148 Units",
                           T ~ NA))

split_wage <- split_wage %>%
  group_by(split) %>%
  summarise(n = n_distinct(BBL)) %>%
  filter(!is.na(split))


split_wage_plot <- split_wage %>%
  ggplot(aes(x = split, y = n)) + 
  geom_col() +
  theme_minimal()
         
#-------------------------------Injuries----------



potential_485_x_injuries <- affordability_areas %>%
  filter(filing_date_clean > "2020-01-01" & (active_exemption == 0 | is.na(active_exemption)) & (type != "421a" | is.na(type)
  )) %>%
  mutate(wage_type = case_when(prev_wage == 0 & four85x_100_unit == 0 ~ "No Prevailing Wage",
                               four85x_zone_a == 1 | four85x_zone_b == 1  ~ "Higher Prevailing Wages, based on Geography + Unit Size",
                               four85x_100_unit == 1 ~ "Prevailing Wage based on Unit Size",
                               T ~ "No Prevailing Wage"
  ),
  wage_type = factor(wage_type, levels = wage_levels)) %>%
  arrange(wage_type) 
  
potential_485_x_injuries_plot <- potential_485_x_injuries %>%
  mutate(injury = ifelse(is.na(injury), 0, injury)) %>%
  ggplot(aes(x = injury, group = wage_type, fill = wage_type)) + 
  geom_density(adjust=1.5) +     theme_ipsum()


         
         
