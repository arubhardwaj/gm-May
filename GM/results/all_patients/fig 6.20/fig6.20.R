
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



deaths <- deaths %>% left_join(pt_count)


deaths %>% select(mortality_rate_per1k, study_year) %>% 
  ggplot()+
  geom_line(aes(x=study_year,y=mortality_rate_per1k))

# with trend
deaths %>% select(mortality_rate_per1k, study_year) %>% 
  ggplot(aes(x=study_year,y=mortality_rate_per1k))+
  geom_line(col = "blue", lwd = 0.8)+
  geom_smooth(method = "lm", lwd = 0.3, col= "red")
