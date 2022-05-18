

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


table_6.3 <- all_patients %>% group_by(study_year) %>% summarise(mean_gulucose = round(mean(HbA1c_DCCT,na.rm=T),2),
                                                    sd_gulucose = round(sd(HbA1c_DCCT,na.rm=T),2))

table_6.3_yg <- all_patients %>% group_by(year_group) %>% summarise(mean_gulucose = round(mean(HbA1c_DCCT,na.rm=T),2),
                                                                 sd_gulucose = round(sd(HbA1c_DCCT,na.rm=T),2))

names(table_6.3_yg) <- c("Years","Mean","Standard Deviation")
names(table_6.3) <- c("Years","Mean","Standard Deviation")

table_6.3 <- rbind(table_6.3,table_6.3_yg)

write_csv(table_6.3, "results/all_patients/6.3/6.3.csv")
