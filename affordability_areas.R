
#-------------Affordability Areas----------

#---------------Packages-----------------
library(ggspatial)
library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(arcgis)
#will also need arcgislayers



#--------------Upload Maps of NYC -----------


# BBLs (Borough Base Lots)
nyc_map <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/MAPPLUTO/FeatureServer/0"
nyc_map <- arcgislayers::arc_open(nyc_map) 

# 2020 NTAs (Needed to demarcate Zone A and B, and NTAs are not in Pluto)
nta_map <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2020/FeatureServer/0" 
nta_map <- arcgislayers::arc_open(nta_map)

# Borough Map (needed to determine the Zone A in Manhttan)
nyc_borough_map <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0"
nyc_borough <- arcgislayers::arc_open(nyc_borough_map) 

# as an alternative to ArcGis Layers, download from DCP/OpenData

#------------------------------Create  Base Map of BBLs using Pluto---------------------

nyc_map <- nyc_map %>%
  arc_select(fields = c("Borough", "Council", "BBL", "XCoord", "YCoord", #geographic variables
                        "YearBuilt", "YearAlter1", "YearAlter2") #YearBuilt can show us the year the building was built, but it's not always correct
  )

nyc_map <- nyc_map %>%
  st_transform("NAD83")

#----------------Create 485X zone A and B areas (requires prevailing wage)-------

##-----------------------------Manhattan Zone A--------


# all of manhattan below 96th street is in Zone A
# demarcate the border of 96th street by creating a box around manhattan below 96th street
# there's a way to use a linestring to "cut" a polygon.
# but that requires a separate (older package)
# so instead we're making a box to designate Zone A 

nyc_borough <- nyc_borough %>% #get borough boundaries of Manhattan
  arc_select(fields = c("BoroName"),
             where = "BoroCode = 1" )

nyc_borough <- nyc_borough %>%
  st_transform("NAD83")

#create a polygon with the 96 border
ninety_sixth_border <- st_as_sfc("LINESTRING(-73.984255 40.799289, -73.926955 40.777986,
                                  -74.003067 40.665508,
                                  -74.040684 40.682965,
                                  -74.053161  40.689745,
                                  -73.984255 40.799289

)") %>%
  st_cast("POLYGON") %>%
  st_set_crs("NAD83") %>%
  st_as_sf()

# check work
nyc_borough %>%
  ggplot() +
  geom_sf(fill = "white", colour = "grey50") +
  geom_sf(data = ninety_sixth_border, color = "blue")

sf_use_s2(T)

# get all of manhattan below 96th street 
manhattan_zone_a <-  nyc_borough %>%
  st_intersection(ninety_sixth_border) %>%
  mutate(man_zone_a = 1) #create indicator variable

manhattan_zone_a %>%
  ggplot() +
  geom_sf(fill = "white", colour = "grey50")

##------------------------------Import select NTAs---------------


# the rest of Zone A and Zone B use NTAS to determine prevailing wage levels


zone_a <- c("BK0101","BK0102", "BK0103" , "BK0104", "QN0201")
zone_b <- c("BK0201","BK0202","BK0203","BK0204","BK0601","BK0602","BK0801", "QN0105", "QN010")
combined <- append(zone_a, zone_b)

nta_map <- nta_map %>% # get applicable ntas
  arc_select(fields = c("NTAName", "NTA2020"),
             where = "NTA2020 IN ('BK0101', 'BK0102', 'BK0103', 'BK0104', 
             'QN0201', 'BK0201', 'BK0202', 'BK0203', 'BK0204',
             'BK0601', 'BK0602', 'BK0801', 'QN0105', 'QN0102')" ) #note: could not figure out how to call the combined value but that would be ideal

nta_map <- nta_map %>%
  st_transform("NAD83") %>%
  #create relevant dummys
  mutate(zone_a = ifelse(NTA2020 %in% zone_a, 1, 0),
         zone_b = ifelse(NTA2020 %in% zone_b, 1, 0),
         prev_wage = ifelse(NTA2020 %in% combined, 1, 0)) 

#----------------Combine Affordability Areas with BBL File-------------
# switch off sperical geometry
sf_use_s2(F)

nyc_map_combined <- nyc_map %>%
  st_join(manhattan_zone_a, left = T)  %>%
  st_join(nta_map, left = T) %>%
  mutate(across(c(man_zone_a:prev_wage), ~ ifelse(is.na(.), 0, .)),
         zone_a = if_else(zone_a == 1 | man_zone_a == 1, 1,0,  missing  = 0),
         prev_wage = if_else(zone_a == 1 | prev_wage == 1, 1, 0, missing = 0)
  )


#-----------------------------Export Map and Export BBLs--------


st_write





