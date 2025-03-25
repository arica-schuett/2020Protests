library(ggplot2)

ProtestByCity <- read.csv("/Users/aricaschuett/Documents/protest/ProtestByCity3-25.csv", row.names = NULL)


#ProtestByCity$AntiTrumpProtestPreGFCount[is.na(ProtestByCity$AntiTrumpProtestPreGFCount)] <- 0
#ProtestByCity$ProtestTotal[is.na(ProtestByCity$ProtestTotal)] <- 0
#ProtestByCity$BlackVictimCount[is.na(ProtestByCity$BlackVictimCount)] <- 0
#ProtestByCity$VictimCountPreGF[is.na(ProtestByCity$VictimCountPreGF)] <- 0 
#ProtestByCity$PostGFProtestCount[is.na(ProtestByCity$PostGFProtestCount)] <- 0
#ProtestByCity$VictimsCount[is.na(ProtestByCity$VictimsCount)] <- 0
#ProtestByCity$VictimsPreGF2020[is.na(ProtestByCity$VictimsPreGF2020)] <- 0
#ProtestByCity$BlackVictimCountPreGF[is.na(ProtestByCity$BlackVictimCountPreGF)] <- 0 
#ProtestByCity$ProtestTotalPreGF[is.na(ProtestByCity$ProtestTotalPreGF)] <- 0
#ProtestByCity$VictimHighThreat[is.na(ProtestByCity$VictimHighThreat)] <- 0
#ProtestByCity$VictimsPreGF2020Blk[is.na(ProtestByCity$VictimsPreGF2020Blk)] <- 0

# summary(ProtestByCity$BlackPovRate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   0.946   1.000   0.893   1.000   1.000    4358 
# table(ProtestByCity$BlackPop == 0)
# FALSE  TRUE 
# 15391  4358 


### Create Summary Tables for Cities with Protest and those without
NoProtestCities <- ProtestByCity %>%
  filter(PostGFProtestCount == 0)
write.csv(NoProtestCities, "/Users/aricaschuett/Documents/protest/NoProtestCities3-25.csv")


summary(NoProtestCities$population)
summary(NoProtestCities$BlackPopPct)
summary(NoProtestCities$EduRate)
summary(NoProtestCities$CollegeStudents)
summary(NoProtestCities$AntiTrumpProtestPreGFCount)
summary(NoProtestCities$VictimsCount)

ProtestCities <- ProtestByCity %>%
  filter(PostGFProtestCount > 0)
write.csv(ProtestCities, "/Users/aricaschuett/Documents/protest/ProtestCities3-25.csv")

summary(ProtestCities$population)
summary(ProtestCities$BlackPopPct)
summary(ProtestCities$EduRate)
summary(ProtestCities$CollegeStudents)
summary(ProtestCities$AntiTrumpProtestPreGFCount)
summary(ProtestCities$VictimsCount)
#####




m1 <- lm(PostGFProtestCount ~ population , data = ProtestByCity)
nobs(m1) #19749
summary(m1)

m2 <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020, data = ProtestByCity)
nobs(m2) #19749
used_data <- model.frame(m2)
dropped_obs <- anti_join(ProtestByCity, used_data, by = c("PostGFProtestCount","VictimsPreGF2020"))
summary(m2)

m3 <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020, data = ProtestByCity)
nobs(m3) #19749
used_data <- model.frame(m3)
dropped_obs <- anti_join(ProtestByCity, used_data, by = c("PostGFProtestCount","VictimsPreGF2020"))summary(m3)

m4 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsPreGF2020, data = ProtestByCity)
nobs(m4) #15388 # drops places with no Black People
used_data <- model.frame(m4)
dropped_obs <- anti_join(ProtestByCity, used_data, by = c("PostGFProtestCount","population"))
summary(m4)

m5 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimCountPreGF, data = ProtestByCity)
nobs(m5) #15388
summary(m5)

m6 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimCountPreGF, data = ProtestByCity)
nobs(m6) #15388
summary(m6)

m7 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + AntiTrumpProtestPreGFCount + BlackVictimCountPreGF, data = ProtestByCity)
nobs(m7) #15388
summary(m7) #controling for anti-trump protests makes the education rate go negative. 

p1 <- ggplot(ProtestByCity, aes(x = BlackVictimCountPreGF, y = PostGFProtestCount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot of Post-GF Protest Count vs. Police Deaths Rate",
       x = "Black Victims of Police Violence ",
       y = "Post-GF Protest Count") +
  theme_minimal()
p1

p2 <- ggplot(ProtestByCity, aes(x = VictimsCount, y = PostGFProtestCount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot of Post-GF Protest Count vs. Police Deaths Rate",
       x = "Black Victims of Police Violence ",
       y = "Post-GF Protest Count") +
  theme_minimal()
p2

p3 <- ggplot(ProtestByCity, aes(x = AntiTrumpProtestPreGFCount, y = PostGFProtestCount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot of Post-GF Protest Count vs. Police Deaths Rate",
       x = "Victim Count",
       y = "Post-GF Protest Count") +
  theme_minimal()
p3

m7a <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + AntiTrumpProtestPreGFCount + BlackVictimCountPreGF, data = ProtestCities)
nobs(m7a) #3720
used_data <- model.frame(m7a)
dropped_obs <- anti_join(ProtestCities, used_data, by = c("PostGFProtestCount","population")) #drops 219 for having no Black poverty rate
summary(m7a)

ProtestByCity$PoliceDeathsRate <- ProtestByCity$VictimCountPreGF/ProtestByCity$population
ProtestByCity$BlackVictimsRate <- ProtestByCity$VictimCountPreGF/ProtestByCity$population

m8 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + AntiTrumpProtestPreGFCount+ PoliceDeathsRate, data = ProtestByCity)
nobs(m8) #15388
summary(m8)


m9 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimsRate, data = ProtestByCity)
nobs(m9) #15388
summary(m9)

ProtestByCity$VictimRaceRatio <- ProtestByCity$BlackVictimCountPreGF/ ProtestByCity$VictimCountPreGF
m10 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimRaceRatio, data = ProtestByCity)
nobs(m10) #1871
used_data <- model.frame(m10)
dropped_obs <- anti_join(ProtestByCity, used_data, by = c("PostGFProtestCount","population")) #drops 16799 for not being able to calculate the victim Race Ratio. 
summary(m10)


### Post GF Protest as DV
m11 <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimCountPreGF, data = ProtestByCity)
nobs(m11) #19749
summary(m11)

m12 <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020, data = ProtestByCity)
nobs(m12) #19749
summary(m12)


m13 <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020Blk, data = ProtestByCity)
nobs(m13) #19749
summary(m13)


m14 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimCountPreGF, data = ProtestByCity)
nobs(m14) #19749
summary(m14)

m15 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimCountPreGF, data = ProtestByCity)
nobs(m15) #15388
summary(m15)


ProtestByCity$PoliceDeathsRate <- ProtestByCity$VictimCountPreGF/ProtestByCity$population
ProtestByCity$BlackVictimsRate <- ProtestByCity$BlackVictimCountPreGF/ProtestByCity$population

m16 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + PoliceDeathsRate, data = ProtestByCity)
nobs(m16) #15388
summary(m16)



m17 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimsRate, data = ProtestByCity)
nobs(m17) #19749
summary(m17)

ProtestByCity$VictimRaceRatio <- ProtestByCity$BlackVictimCountPreGF/ ProtestByCity$VictimCountPreGF

m18 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimRaceRatio, data = ProtestByCity)
nobs(m18) #1871
summary(m18)

m19 <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate +
                                            AntiTrumpProtestPreGFCount + VictimRaceRatio, data = ProtestByCity)
nobs(m19) #1871
summary(m19)

p1 <- ggplot(ProtestByCity, aes(x = PoliceDeathsRate, y = PostGFProtestCount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot of Post-GF Protest Count vs. Police Deaths Rate",
       x = "Police Deaths Rate (Victims per capita)",
       y = "Post-GF Protest Count") +
  theme_minimal()
p1
