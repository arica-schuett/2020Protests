library(tidyverse)
library(lubridate)

#ccc.acs_complete <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc.acs_complete.csv")
#ccc.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc-acsTotal.csv")
#mapping.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping.acs.csv")
#mapping <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping.csv")
#acs_wide <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/acs_wide.csv")

### This data will contain every ACS observation, the count of police killings jan 2017-before GF, count of police killings after GF (but before Sep 2020), 
### Count of general left-wing protests (Valence in CCC, check code book) jan 2014-pre GF, 
### Democratic vote share 2016 MITESL, FIPS 

ccc_spatial_join <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc_spatial_join.csv", colClasses = c("PLACEFIPS" = "character"))

#mapping_acs_join <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ACS_Mapping.csv", header=FALSE)

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



#mapping <- mapping_acs_join
ccc <- ccc_spatial_join

#mapping <- mapping %>%
#  mutate(date = mdy(date))

ccc <- ccc %>%
  mutate(date = mdy(date))

#mapping <- mapping %>%
#  mutate(
#    date = ymd(date),  # Convert to Date type
#    year = year(date  ))      # Extract year


ccc <- ccc %>%
  mutate(
    date = ymd(date),  # Convert to Date type. # Begins Jan 1, 2017
    year = ymd(date  ))      # Extract year

mapping <- filter(mapping, date < as.Date("2020-09-01" ))
mapping <- filter(mapping, date > as.Date("2017-01-01" ))
summary(mapping$date)
mapping <- mapping %>%
  mutate(mappingPreGF = ifelse(date >= as.Date("2017-01-01") & date < as.Date("2020-05-25"), 1, 0),
         mappingPostGF = ifelse(date >= as.Date("2020-05-25") & date <= as.Date("2020-09-01"), 1, 0),
         mappingBlackOnlyPreGF = ifelse(race == "Black" & date >= as.Date("2017-01-01") & date < as.Date("2020-05-25"), 1, 0),
         mappingBlackOnlyPostGF = ifelse(race == "Black" & date >= as.Date("2020-05-25") & date <= as.Date("2020-09-01"), 1, 0))

#mapping <- mapping %>%
#  dplyr::select(name, age, gender, race, date, city, state, zip, county, agency_responsible, 
#         cause_of_death, officer_charged, allegedly_armed, wapo_armed, wapo_threat_level, wapo_flee, 
#         wapo_body_camera, wapo_id, off_duty_killing, officer_known_past_shootings, mappingPreGF, 
#         mappingPostGF, mappingBlackOnlyPostGF, mappingBlackOnlyPreGF, POPULATION, POP_SQMI, year, 
#         BlackPop, BlackPov, Bachelors, Masters, ProfDegree, Pop25Plus, ProfDegree, 
#         Doctorate, CollegeStudents, PLACEFIPS)

ccc <- ccc %>%
  dplyr::select(date, locality, state, type, actors, claims, valence, issues, size_mean, arrests_any, 
         injuries_crowd_any, injuries_police_any, property_damage_any, PLACEFIPS )


ccc <- filter(ccc, date < as.Date("2020-09-01" ))
ccc <- filter(ccc, date > as.Date("2017-01-01" ))
ccc <- ccc %>%
  mutate(cccPreGF = ifelse(date >= "2017-01-01" & date < "2020-05-25", 1, 0),
         cccPostGF = ifelse(date >= "2020-05-25" & date <= "2020-09-01", 1, 0))



## Descriptive tables-- Victims by type by city
## Victims per city Total
VictimCount <- mapping %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimsCount = n())
#VC <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCount.csv")
write.csv(VictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountFull.csv")

##Victims per city Black 
BlackVictimCount <- mapping %>%
  filter(race == "Black") %>%
  group_by(PLACEFIPS) %>%
  summarize(BlackVictimCount = n()) 
write.csv(BlackVictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountFull.csv")

## Pre-GF Victims per city  
VictimCountPreGF <- mapping %>%
  filter(mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimCountPreGF = n()) 
write.csv(VictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountPreGFFull.csv")

## Pre-GF Victims per city Black 
BlackVictimCountPreGF <- mapping %>%
  filter(mappingBlackOnlyPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(BlackVictimCountPreGF = n())
write.csv(BlackVictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountPreGFFull.csv")

## Pre-GF Victims per city 2020
VictimsPreGF2020 <- mapping %>%
  filter( year == 2020 & mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimsPreGF2020 = n()) 
write.csv(VictimsPreGF2020, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimsPreGF2020Full.csv")

## Pre-GF Victims per city 2020 Black 
VictimsPreGF2020Blk <- mapping %>%
  filter(race == "Black" & year == 2020 & mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimsPreGF2020Blk = n()) 
write.csv(VictimsPreGF2020Blk, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimsPreGF2020BlkFull.csv")

##High Threat Victims Per City  Pre GF add armed vs unarmed
VictimHighThreat <- mapping %>%
  filter(wapo_threat_level == "High" & mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimHighThreat = n()) 
write.csv(VictimHighThreat, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimHighThreatFull.csv")

## Descriptive tables-- Victims by type by city
##Protest Totals Per city Pre GF and left wing valence protest totals per city
ProtestTotal <- ccc %>%
  group_by(PLACEFIPS) %>%
  summarize(ProtestTotal= n()) 
head(ProtestTotal)
write.csv(ProtestTotal, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestTotalFull.csv")

AntiTrumpProtestPreGF <- ccc %>%
  filter(date < "2020-05-25" & valence == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(AntiTrumpProtestPreGFCount = n()) 
write.csv(AntiTrumpProtestPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/AntiTrumpProtestPreGF.csv")

ProtestTotalPreGF <- ccc %>%
  group_by(PLACEFIPS) %>%
  summarize(ProtestTotalPreGF= n()) 
head(ProtestTotalPreGF)
write.csv(ProtestTotalPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestTotalPreGF.csv")

PostGFProtestCount <- ccc %>%
  filter(date >= ymd("2020-05-25")) %>%
  group_by(PLACEFIPS) %>%
  summarize(PostGFProtestCount = n()) 
head(PostGFProtestCount)
write.csv(PostGFProtestCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/PostGFProtestCount.csv")

NoProtestCitiesCCC <- PostGFProtestCount %>%
  filter(PostGFProtestCount == 0)

######## Access Census Data #####
census_api_key("47674665e11654113f17b2b82d1a791a88f289b4", overwrite=TRUE)
acs_vars <- load_variables(2019, "acs5", cache = TRUE)

acs_data <- get_acs(geography = "place", 
                    variables = c(population = "B01003_001", 
                                  BlackPop = "B02001_003",
                                  BlackPov = "B17020B_001",
                                  Bachelors = "B15003_022", 
                                  Pop25Plus = "B15003_001",
                                  Masters = "B15003_023", 
                                  ProfDegree = "B15003_024", 
                                  Doctorate = "B15003_025",
                                  
                                  CollegeStudents = "B14001_008"),
                    year = 2019, #2015-2019 5-year ACS
                    survey = "acs5")
acs_data <- acs_data %>% dplyr::select(-moe)

################

acs_wide <- acs_data %>%
  pivot_wider(names_from = variable, values_from = estimate)

acs_wide <- acs_wide %>%
  filter( population > 500)

any(is.na(acs_wide$population))
any(is.na(acs_wide$BlackPop))
sum(acs_wide$population) 
sum(acs_wide$BlackPop) 

head(ProtestTotal)
head(mapping)
acs_wide$PLACEFIPS <- acs_wide$GEOID


# Store original column names from acs_wide
original_cols <- names(acs_wide)

# Perform all joins in a clean pipeline
ProtestByCity <- acs_wide %>%
  left_join(ProtestTotal, by = "PLACEFIPS") %>%
  left_join(PostGFProtestCount, by = "PLACEFIPS") %>%
  left_join(VictimCount, by = "PLACEFIPS") %>%
  left_join(BlackVictimCount, by = "PLACEFIPS") %>%
  left_join(VictimCountPreGF, by = "PLACEFIPS") %>%
  left_join(BlackVictimCountPreGF, by = "PLACEFIPS") %>%
  left_join(VictimsPreGF2020, by = "PLACEFIPS") %>%
  left_join(VictimHighThreat, by = "PLACEFIPS") %>%
  left_join(AntiTrumpProtestPreGF, by = "PLACEFIPS") %>%
  left_join(VictimsPreGF2020Blk, by = "PLACEFIPS") %>%
  left_join(ProtestTotalPreGF, by = "PLACEFIPS") %>%
  mutate(across(-all_of(original_cols), ~ replace_na(.x, 0)))



#ProtestByCity <- left_join(acs_wide, ProtestTotal, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, PostGFProtestCount, by= "PLACEFIPS", all = T) 
#ProtestByCity <- left_join(ProtestByCity, VictimCount, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, BlackVictimCount, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, VictimCountPreGF, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, BlackVictimCountPreGF, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, VictimsPreGF2020, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, VictimHighThreat, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, AntiTrumpProtestPreGF, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, VictimsPreGF2020Blk, by= "PLACEFIPS", all = T)
#ProtestByCity <- left_join(ProtestByCity, ProtestTotalPreGF, by= "PLACEFIPS", all = T)






ProtestByCity <- ProtestByCity %>%
  mutate(CollegeEdTotal = Bachelors + Masters + ProfDegree + Doctorate)

ProtestByCity$EduRate <- ProtestByCity$CollegeEdTotal/ProtestByCity$Pop25Plus
ProtestByCity$BlackPopPct <- ProtestByCity$BlackPop / ProtestByCity$population
ProtestByCity$BlackPovRate <- ProtestByCity$BlackPov / ProtestByCity$BlackPop


write.csv(ProtestByCity, "/Users/aricaschuett/Documents/protest/ProtestByCity3-25.csv")
