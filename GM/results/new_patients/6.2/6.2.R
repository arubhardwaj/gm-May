

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



# 6.2 ---------------------------------------------------------------------
library(tidytidbits)
library(reshape)

count <- all_patients %>% group_by(study_year) %>% count_by(gender)

percentage <- cast(count, study_year~gender)


count2 <- all_patients %>% group_by(study_year) %>% count(gender)
n <- cast(count2, study_year~gender)



names(percentage) <- c("study_year","male percentage","female percentage")
names(n) <- c("study_year","male count","female count")

count_data <- n %>% left_join(percentage)

count_data$total <- count_data$`male count`+count_data$`female count`
count_data_1 <- count_data


# Year group --------------------------------------------------------------

count <- all_patients %>% group_by(year_group) %>% count_by(gender)

percentage <- cast(count, year_group~gender)


count2 <- all_patients %>% group_by(year_group) %>% count(gender)
n <- cast(count2, year_group~gender)



names(percentage) <- c("study_year","male percentage","female percentage")
names(n) <- c("study_year","male count","female count")

count_data <- n %>% left_join(percentage)

count_data$total <- count_data$`male count`+count_data$`female count`
count_data_2 <- count_data

table_6.2 <- rbind(count_data_1,count_data_2)

write_csv(table_6.2,"results/all_patients/6.2/count_data.csv")



