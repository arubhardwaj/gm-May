
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


patients_hes <- all_patients %>% mutate(MI_HES = ifelse(is.na(current_mi_HES_event)==TRUE,0,1),
                        LVD_HES = ifelse(is.na(current_lvd_HES_event)==TRUE,0,1),
                        angina_HES = ifelse(is.na(current_angina_HES_event)==TRUE,0,1),
                        cancer_hes = ifelse(is.na(current_cancer_HES_event)==TRUE,0,1),
                        stroke_HES = ifelse(is.na(current_stroke_HES_event)==TRUE,0,1),
                        vision_HES = ifelse(is.na(current_vision_HES_event)==TRUE,0,1),
                        microvasc_HES = ifelse(is.na(current_microvasc_HES_event)==TRUE,0,1),
                        
                        ) 




attach(patients_hes)
patients_hes$Hosp <- (LVD_HES+angina_HES+cancer_hes+stroke_HES+vision_HES+microvasc_HES)

patients_hes$Hosp <- ifelse(patients_hes$Hosp>0,1,0)


# Total -------------------------------------------------------------------


# Total

total <- patients_hes %>% group_by(year_group) %>%  
  summarise(rate_1000_pop = sum(Hosp)/1000,
            `standard deviation` = sd(Hosp))

# Male Total


total_m <- patients_hes %>% filter(gender==1) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Males = sum(Hosp)/1000,
            `standard deviation Males` = sd(Hosp))

# Female

total_f <- patients_hes %>% filter(gender==2) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Females = sum(Hosp)/1000,
            `standard deviation Females` = sd(Hosp))


any_reason <- data.frame(total, total_m[,2:3], total_f[,2:3])


a <- data.frame(lapply(any_reason[,2:7], mean))
a <- data.frame(year_group="All Years",a)

any_reason <- rbind(any_reason,a)


# LVD ---------------------------------------------------------------------

# Total

total <- patients_hes %>% group_by(year_group) %>%  
  summarise(rate_1000_pop = sum(LVD_HES)/1000,
            `standard deviation` = sd(LVD_HES))

# Male Total


total_m <- patients_hes %>% filter(gender==1) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Males = sum(LVD_HES)/1000,
            `standard deviation Males` = sd(LVD_HES))

# Female

total_f <- patients_hes %>% filter(gender==2) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Females = sum(LVD_HES)/1000,
            `standard deviation Females` = sd(LVD_HES))


LVD_Hospitalisation <- data.frame(total, total_m[,2:3], total_f[,2:3])


a <- data.frame(lapply(LVD_Hospitalisation[,2:7], mean))
a <- data.frame(year_group="All Years",a)

LVD_Hospitalisation <- rbind(LVD_Hospitalisation,a)



# Microvascular -----------------------------------------------------------

# Total

total <- patients_hes %>% group_by(year_group) %>%  
  summarise(rate_1000_pop = sum(microvasc_HES)/1000,
            `standard deviation` = sd(microvasc_HES))

# Male Total


total_m <- patients_hes %>% filter(gender==1) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Males = sum(microvasc_HES)/1000,
            `standard deviation Males` = sd(microvasc_HES))

# Female

total_f <- patients_hes %>% filter(gender==2) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Females = sum(microvasc_HES)/1000,
            `standard deviation Females` = sd(microvasc_HES))


MI_Hospitalisation <- data.frame(total, total_m[,2:3], total_f[,2:3])

MI_Hospitalisation <- data.frame(total, total_m[,2:3], total_f[,2:3])

a <- data.frame(lapply(MI_Hospitalisation[,2:7], mean))
a <- data.frame(year_group="All Years",a)

MI_Hospitalisation <- rbind(MI_Hospitalisation,a)


# Cancer ------------------------------------------------------------------

# Total

total <- patients_hes %>% group_by(year_group) %>%  
  summarise(rate_1000_pop = sum(cancer_hes)/1000,
            `standard deviation` = sd(cancer_hes))

# Male Total


total_m <- patients_hes %>% filter(gender==1) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Males = sum(cancer_hes)/1000,
            `standard deviation Males` = sd(cancer_hes))

# Female

total_f <- patients_hes %>% filter(gender==2) %>% group_by(year_group) %>%  
  summarise(rate_1000_pop_Females = sum(cancer_hes)/1000,
            `standard deviation Females` = sd(cancer_hes))


cancer_Hospitalisation <- data.frame(total, total_m[,2:3], total_f[,2:3])

a <- data.frame(lapply(cancer_Hospitalisation[,2:7], mean))
a <- data.frame(year_group="All Years",a)

cancer_Hospitalisation <- rbind(cancer_Hospitalisation,a)


table_6.22 <- rbind(any_reason, LVD_Hospitalisation,MI_Hospitalisation,cancer_Hospitalisation)

write_csv(table_6.22, "results/all_patients/6.13/6.22.csv")



























