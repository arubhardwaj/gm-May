
# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

new_patients <- read_csv("dt_new_pt.csv")


# Define year groups:

#new_patients$last_month <- lubridate::mdy(new_patients$last_month)
new_patients <- new_patients %>% mutate(year_group = ifelse(last_month>='2000-01-31' & last_month <= '2002-10-01',"2000-2002",
                                                            ifelse(last_month>='2002-10-01' & last_month <= '2008-05-01',"2002-2008",
                                                                   ifelse(last_month>='2008-05-01' & last_month <= '2012-07-01',"2008-2012",ifelse(last_month>='2012-07-01' & last_month <= '2014-03-31',"2012-2014","NA")))))






new_patients$dead <- ifelse(is.na(new_patients$deathdate)==TRUE,0,1)
table(new_patients$dead )


# Mortality ---------------------------------------------------------------


deaths <- new_patients %>% group_by(study_year) %>% select(pt, dead) %>% unique()  %>% summarise(deaths = sum(dead))


pt_count <- new_patients %>% group_by(study_year) %>% select(pt) %>% unique() %>% 
  summarise(`Number of Patients` = n())

pt_count$mortality_rate_per1k <- (deaths$deaths/(pt_count$`Number of Patients`+1000))*100

pt_count$mortality_rate <- (deaths$deaths / pt_count$`Number of Patients`)*100



deaths1 <- deaths %>% left_join(pt_count)






deaths <- new_patients %>% group_by(year_group) %>% select(pt, dead) %>% unique()  %>% summarise(deaths = sum(dead))


pt_count <- new_patients %>% group_by(year_group) %>% select(pt) %>% unique() %>% 
  summarise(`Number of Patients` = n())

pt_count$mortality_rate_per1k <- (deaths$deaths/(pt_count$`Number of Patients`+1000))*100

pt_count$mortality_rate <- (deaths$deaths / pt_count$`Number of Patients`)*100



deaths2 <- deaths %>% left_join(pt_count)

names(deaths1)[1] <- "Year"
names(deaths2)[1] <- "Year"

deaths <- rbind(deaths1,deaths2)

write_csv(deaths,"results/new_patients/6.25/6.25.csv")
