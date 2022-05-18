
# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")


# Define year groups:

all_patients <- all_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))



all_patients$dead <- ifelse(is.na(all_patients$deathdate)==TRUE,0,1)
table(all_patients$dead )


# Mortality ---------------------------------------------------------------


deaths <- all_patients %>% group_by(study_year) %>% select(pt, dead) %>% unique()  %>% summarise(deaths = sum(dead))


pt_count <- all_patients %>% group_by(study_year) %>% select(pt) %>% unique() %>% 
  summarise(`Number of Patients` = n())

pt_count$mortality_rate_per1k <- (deaths$deaths/(pt_count$`Number of Patients`+1000))*100

pt_count$mortality_rate <- (deaths$deaths / pt_count$`Number of Patients`)*100



deaths1 <- deaths %>% left_join(pt_count)






deaths <- all_patients %>% group_by(year_group) %>% select(pt, dead) %>% unique()  %>% summarise(deaths = sum(dead))


pt_count <- all_patients %>% group_by(year_group) %>% select(pt) %>% unique() %>% 
  summarise(`Number of Patients` = n())

pt_count$mortality_rate_per1k <- (deaths$deaths/(pt_count$`Number of Patients`+1000))*100

pt_count$mortality_rate <- (deaths$deaths / pt_count$`Number of Patients`)*100



deaths2 <- deaths %>% left_join(pt_count)

names(deaths1)[1] <- "Year"
names(deaths2)[1] <- "Year"

deaths <- rbind(deaths1,deaths2)

write_csv(deaths,"results/all_patients/6.25/6.25.csv")
