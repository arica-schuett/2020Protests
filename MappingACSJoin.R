library(lubridate)
library(tidyverse)
library(tidycensus)
library(sf)

### This code gathers and cleans the data for making the tables

######## Access Census Data #####
census_api_key("47674665e11654113f17b2b82d1a791a88f289b4", overwrite=TRUE)
acs_vars <- load_variables(2019, "acs5", cache = TRUE)


# Get ACS data at tract level
acs_data <- get_acs(geography = "place", variables= c(population = "B01003_001", 
                                                     BlackPop = "B02001_003",
                                                     BlackPov = "B17020B_001",
                                                     Bachelors = "B15003_022", 
                                                     Pop25Plus = "B15003_001",
                                                     Masters = "B15003_023", 
                                                     ProfDegree = "B15003_024", 
                                                     Doctorate = "B15003_025",
                                                     CollegeStudents = "B14001_008"),
                    year = 2019, #2015-2019 5-year ACS
                    survey = "acs5", 
                    geometry = T)

# geometry=TRUE automatically downloads shapefiles and computes centroids:
#acs_data <- acs_data %>% mutate(
#  longitude = st_coordinates(st_centroid(geometry))[,1],
#  latitude = st_coordinates(st_centroid(geometry))[,2]
#)

acs_data <- acs_data %>% dplyr::select(-moe)
#acs_data$geometry <- NULL
################

acs_wide <- acs_data %>%
  pivot_wider(names_from = "variable", values_from = "estimate")

acs_wide <- acs_wide %>%
  filter( population > 500)


any(is.na(acs_wide$population))
any(is.na(acs_wide$BlackPop))
sum(acs_wide$population) # 242323357
sum(acs_wide$BlackPop) # 34557802

# Filter out PR
acs_wide <- filter(acs_wide, GEOID < 7200186)

# policing, race #starting keywords not patriotism Not Pro-police not anti-mask or anti
mapping <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping_spatial_join.csv", colClasses = c("PLACEFIPS" = "character"))
mapping <- mapping %>%
  filter(!grepl("Vehicle", cause_of_death))

####  threat
mapping$wapo_threat_level[ mapping$wapo_threat_level=="Attack"|
                             mapping$wapo_threat_level=="Brandished Weapon" |
                             mapping$wapo_threat_level=="Sudden Threatening Movement" |
                             mapping$wapo_threat_level=="Used Weapon"]<-"High"   ### unclear if including brandishing or sudden threatening movement as HIGH threat

mapping$date <- mdy(mapping$date)

mapping <- mapping %>%
  mutate(date = ymd(date),  # Convert to Date type
         year = year(date))      # Extract year
hist(mapping$year) 



mapping <- mapping %>%
  mutate(mappingPreGF = ifelse(date >= "2017-01-01" & date <= "2020-05-20", 1, 0),
         mappingPostGF = ifelse(date>= "2020-05-20" & date <= "2020-09-01", 1, 0),
         mappingBlackOnlyPreGF = ifelse(race == "Black" & date >= "2017-01-01" & date <= "2020-05-20", 1, 0),
         mappingBlackOnlyPostGF = ifelse(race == "Black" & date >= "2020-05-20" & date <= "2020-09-01", 1, 0))



acs_mapping <- left_join(mapping, acs_wide, by = c("PLACEFIPS" = "GEOID"))




write.csv(acs_mapping, "/Users/aricaschuett/Documents/protest/Shea + Arica/ACS_Mapping.csv")


#ccc_spatial_join <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc_spatial_join.csv", colClasses = c("PLACEFIPS" = "character"))

#ccc_spatial_join2 <- left_join(acs_wide, ccc_spatial_join, by = c("GEOID"= "PLACEFIPS" ))


#write.csv(ccc_spatial_join2, "/Users/aricaschuett/Documents/protest/Shea + Arica/ccc_spatial_join2.csv")










