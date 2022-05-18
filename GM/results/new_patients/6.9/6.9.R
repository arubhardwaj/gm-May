
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




total <- all_patients %>% group_by(year_group) %>% 
  summarise(rate_1000_pop = sum(current_lvd)/1000,
            `standard deviation` = sd(current_lvd))


# males



total_m <- all_patients %>% filter(gender==1) %>% group_by(year_group) %>% 
  summarise(rate_1000_pop_Males = sum(current_lvd)/1000,
            `standard deviation Males` = sd(current_lvd))



# females


total_f <- all_patients %>% filter(gender==2) %>% group_by(year_group) %>% 
  summarise(rate_1000_pop_Females = sum(current_lvd)/1000,
            `standard deviation Females` = sd(current_lvd))

all_years <- all_patients %>% 
  summarise(rate_1000_pop = sum(current_lvd)/1000,
            `standard deviation` = sd(current_lvd))

all_years <- cbind("year_group"="All Years",all_years)


all_years_m <- all_patients %>% filter(gender==1) %>% 
  summarise(rate_1000_pop_Males = sum(current_lvd)/1000,
            `standard deviation Males` = sd(current_lvd))

all_years_m <- cbind("year_group"="All Years",all_years_m)


all_years_f <- all_patients %>% filter(gender==2) %>% 
  summarise(rate_1000_pop_Females = sum(current_lvd)/1000,
            `standard deviation Females` = sd(current_lvd))

all_years_f <- cbind("year_group"="All Years",all_years_f)

all_years <- cbind(all_years,all_years_m[,2:3], all_years_f[,2:3])
total <- cbind(total, total_m[,2:3], total_f[,2:3])

total <- rbind(total,all_years)

write_csv(total, "results/all_patients/6.9/6.9.csv")

