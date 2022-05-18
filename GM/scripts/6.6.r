

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

# Visits 
total_visits <- all_patients %>% group_by(study_year) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T))


male_visits <- all_patients %>% filter(gender==1) %>% group_by(study_year) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T))


female_visits <- all_patients %>% filter(gender==2) %>% group_by(study_year) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T))


total_visits <- total_visits %>% left_join(male_visits) %>% 
  left_join(female_visits)

## By year groups

total_visits_g <- all_patients %>% group_by(year_group) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T))


male_visits_g <- all_patients %>% filter(gender==1) %>% group_by(year_group) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T))


female_visits_g <- all_patients %>% filter(gender==2) %>% group_by(year_group) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T))


total_visits_g <- total_visits_g %>% left_join(male_visits_g) %>% 
  left_join(female_visits_g)

names(total_visits_g)[1] <- "Year"
names(total_visits)[1] <- "Year"


total_visits <- rbind(total_visits,total_visits_g)
total_visits1 <- total_visits


# Average Per Month -------------------------------------------------------
# Visits 

per_patient <- all_patients[unique(all_patients$pt),] %>% nrow()
per_patient_m <- all_patients %>% filter(gender==1) %>% select(pt) %>% unique() %>% nrow()
per_patient_f <- all_patients %>% filter(gender==2) %>% select(pt) %>% unique() %>% nrow()

total_visits <- all_patients %>% group_by(study_year) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T)/per_patient)


male_visits <- all_patients %>% filter(gender==1) %>% group_by(study_year) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T)/per_patient_m)


female_visits <- all_patients %>% filter(gender==2) %>% group_by(study_year) %>% 
  summarise(Females = sum(GP_contacts_this_month, na.rm =T)/per_patient_f)


total_visits <- total_visits %>% left_join(male_visits) %>% 
  left_join(female_visits)

## By year groups

total_visits_g <- all_patients %>% group_by(year_group) %>% 
  summarise(Total_visits = sum(GP_contacts_this_month, na.rm =T)/per_patient)


male_visits_g <- all_patients %>% filter(gender==1) %>% group_by(year_group) %>% 
  summarise(Males = sum(GP_contacts_this_month, na.rm =T)/per_patient_m)


female_visits_g <- all_patients %>% filter(gender==2) %>% group_by(year_group) %>% 
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



write_csv(table6.6,"results/all_patients/6.6/6.6.csv")
