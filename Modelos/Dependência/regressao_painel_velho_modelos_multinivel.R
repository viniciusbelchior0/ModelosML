# Maryland Crime Scene
library(readr)
library(tidyverse)
library(lubridate)

crime_raw <- read_csv("C:/Users/NOTEBOOK CASA/Downloads/Cursos - DataCamp/Cursos - DataCamp/Projetos/Trends in Maryland Crime Rates (Mixed Effects and Hierarquical)/Trends in Maryland Crime Rates/datasets/Violent_Crime_by_County_1975_to_2016.csv")

crime_use <- crime_raw %>%
  select(JURISDICTION, YEAR, POPULATION, crime_rate = 'VIOLENT CRIME RATE PER 100,000 PEOPLE') %>%
  mutate(YEAR_2 = year(mdy_hms(YEAR)))

head(crime_use)

#2. Raw data and tren lines
ggplot(crime_use, aes(x = YEAR_2, y = crime_rate, group = JURISDICTION)) +
  geom_line() +
  stat_smooth(method = 'lm', se = FALSE, size = 0.5)

#3. Re-scale the data
crime_use <-
  crime_use %>%
  mutate(YEAR_3 = YEAR_2 - min(YEAR_2))

#4. Build a LMER
library(lmerTest)

lmer_crime <- lmer(crime_rate ~ YEAR_3 + (YEAR_3|JURISDICTION), crime_use)
lmer_crime


#5. Model Outputs
# Examine the model outputs using summary
summary(lmer_crime)
# This is for readability
noquote("**** Fixed-effects ****")
# Use fixef() to view fixed-effects
fixef(lmer_crime)
# This is for readability
noquote("**** Random-effects ****")
# Use ranef() to view random-effects
ranef(lmer_crime)

#6. Format model coefficients

# Add the fixed-effect to the random-effect and save as county_slopes
county_slopes <- fixef(lmer_crime)["YEAR_3"] + ranef(lmer_crime)$JURISDICTION["YEAR_3"]
# Add a new column with county names
county_slopes <-
  county_slopes %>%
  rownames_to_column("county")


