

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



# Visits 
total_visits <- new_patients %>% group_by(study_year) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T))


male_visits <- new_patients %>% filter(gender==1) %>% group_by(study_year) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T))


female_visits <- new_patients %>% filter(gender==2) %>% group_by(study_year) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T))


total_visits <- total_visits %>% left_join(male_visits) %>% 
  left_join(female_visits)

## By year groups

total_visits_g <- new_patients %>% group_by(year_group) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T))


male_visits_g <- new_patients %>% filter(gender==1) %>% group_by(year_group) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T))


female_visits_g <- new_patients %>% filter(gender==2) %>% group_by(year_group) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T))


total_visits_g <- total_visits_g %>% left_join(male_visits_g) %>% 
  left_join(female_visits_g)

names(total_visits_g)[1] <- "Year"
names(total_visits)[1] <- "Year"


total_visits <- rbind(total_visits,total_visits_g)
total_visits1 <- total_visits


# Average Per Month -------------------------------------------------------
# Visits 

per_patient <- new_patients[unique(new_patients$pt),] %>% nrow()
per_patient_m <- new_patients %>% filter(gender==1) %>% select(pt) %>% unique() %>% nrow()
per_patient_f <- new_patients %>% filter(gender==2) %>% select(pt) %>% unique() %>% nrow()

total_visits <- new_patients %>% group_by(study_year) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T)/per_patient)


male_visits <- new_patients %>% filter(gender==1) %>% group_by(study_year) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T)/per_patient_m)


female_visits <- new_patients %>% filter(gender==2) %>% group_by(study_year) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T)/per_patient_f)


total_visits <- total_visits %>% left_join(male_visits) %>% 
  left_join(female_visits)

## By year groups

total_visits_g <- new_patients %>% group_by(year_group) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T)/per_patient)


male_visits_g <- new_patients %>% filter(gender==1) %>% group_by(year_group) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T)/per_patient_m)


female_visits_g <- new_patients %>% filter(gender==2) %>% group_by(year_group) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T)/per_patient_f)


total_visits_g <- total_visits_g %>% left_join(male_visits_g) %>% 
  left_join(female_visits_g)

names(total_visits_g)[1] <- "Year"
names(total_visits)[1] <- "Year"


total_visits <- rbind(total_visits,total_visits_g)

total_visits2 <- total_visits

names(total_visits2)[2:4] <- c("Total Visits Per Patent","Total Visits Per Patent (Males)","Total Visits Per Patent (Females)")


table6.6 <- cbind(total_visits1,total_visits2)

table6.6 <- table6.6[,-5]
table6.6 <- data.frame(table6.6)



write_csv(table6.6,"results/new_patients/6.6/6.6.csv")

