

# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

new_patients <- read_csv("dt_new_pt.csv")


# Define year groups:

new_patients <- new_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))



# 6.2 ---------------------------------------------------------------------
library(tidytidbits)
library(reshape)

count <- new_patients %>% group_by(study_year) %>% count_by(gender)

percentage <- cast(count, study_year~gender)


count2 <- new_patients %>% group_by(study_year) %>% count(gender)
n <- cast(count2, study_year~gender)



names(percentage) <- c("study_year","male percentage","female percentage")
names(n) <- c("study_year","male count","female count")

count_data <- n[1:15,1:3] %>% left_join(percentage[1:15,1:3])

count_data$total <- count_data$`male count`+count_data$`female count`
count_data_1 <- count_data


# Year group --------------------------------------------------------------

count <- new_patients %>% group_by(year_group) %>% count_by(gender)

percentage <- cast(count, year_group~gender)


count2 <- new_patients %>% group_by(year_group) %>% count(gender)
n <- cast(count2, year_group~gender)



names(percentage) <- c("study_year","male percentage","female percentage")
names(n) <- c("study_year","male count","female count")

count_data <- n[1:4,1:3] %>% left_join(percentage[1:4,1:3])

count_data$total <- count_data$`male count`+count_data$`female count`
count_data_2 <- count_data

table_6.2 <- rbind(count_data_1,count_data_2)

write_csv(table_6.2,"results/new_patients/6.2/count_data.csv")



