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

total <- all_patients %>% group_by(study_year) %>% summarise(Mean = round(mean(CharlsonIndex, na.rm=T),2),
                                                    SD = round(sd(CharlsonIndex, na.rm=T),2))


total_m <- all_patients %>% filter(gender==1) %>%  group_by(study_year) %>% summarise(Mean_Male = round(mean(CharlsonIndex, na.rm=T),2),
                                                             SD_Male = round(sd(CharlsonIndex, na.rm=T),2))

total_f <- all_patients %>% filter(gender==2) %>%  group_by(study_year) %>% summarise(Mean_Female = round(mean(CharlsonIndex, na.rm=T),2),
                                                                                      SD_Female = round(sd(CharlsonIndex, na.rm=T),2))

table_1 <- total %>% left_join(total_m) %>% left_join(total_f)

names(table_1)[1] <- "Years"

# study year groups
total <- all_patients %>% group_by(year_group) %>% summarise(Mean = round(mean(CharlsonIndex, na.rm=T),2),
                                                             SD = round(sd(CharlsonIndex, na.rm=T),2))


total_m <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(Mean_Male = round(mean(CharlsonIndex, na.rm=T),2),
                                                                                      SD_Male = round(sd(CharlsonIndex, na.rm=T),2))

total_f <- all_patients %>% filter(gender==2) %>%  group_by(year_group) %>% summarise(Mean_Female = round(mean(CharlsonIndex, na.rm=T),2),
                                                                                      SD_Female = round(sd(CharlsonIndex, na.rm=T),2))

table_2 <- total %>% left_join(total_m) %>% left_join(total_f)
names(table_2)[1] <- "Years"

table_6.4 <- rbind(table_1,table_2)

write_csv(table_6.4,"results/all_patients/6.4/6.4.csv")




sample(all_patients$pt,10)

