
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




total <- new_patients %>% group_by(year_group) %>% 
  summarise(rate_1000_pop = sum(current_mi)/1000,
            `standard deviation` = sd(current_mi)) %>% na.omit()


# males



total_m <- new_patients %>% filter(gender==1) %>% group_by(year_group) %>% 
  summarise(rate_1000_pop_Males = sum(current_mi)/1000,
            `standard deviation Males` = sd(current_mi))



# females


total_f <- new_patients %>% filter(gender==2) %>% group_by(year_group) %>% 
  summarise(rate_1000_pop_Females = sum(current_mi)/1000,
            `standard deviation Females` = sd(current_mi))

all_years <- new_patients %>% 
  summarise(rate_1000_pop = sum(current_mi,na.rm=T)/1000,
            `standard deviation` = sd(current_mi,na.rm=T))

all_years <- cbind("year_group"="All Years",all_years)


all_years_m <- new_patients %>% filter(gender==1) %>% 
  summarise(rate_1000_pop_Males = sum(current_mi)/1000,
            `standard deviation Males` = sd(current_mi))

all_years_m <- cbind("year_group"="All Years",all_years_m)


all_years_f <- new_patients %>% filter(gender==2) %>% 
  summarise(rate_1000_pop_Females = sum(current_mi)/1000,
            `standard deviation Females` = sd(current_mi))

all_years_f <- cbind("year_group"="All Years",all_years_f)

all_years <- cbind(all_years,all_years_m[,2:3], all_years_f[,2:3])
total <- cbind(total, total_m[,2:3], total_f[,2:3])

total <- rbind(total,all_years)

write_csv(total, "results/new_patients/6.10/6.10.csv")

