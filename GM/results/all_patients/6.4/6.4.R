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




total <- new_patients %>% group_by(study_year) %>% summarise(Mean = round(mean(CharlsonIndex, na.rm=T),2),
                                                    SD = round(sd(CharlsonIndex, na.rm=T),2))


total_m <- new_patients %>% filter(gender==1) %>%  group_by(study_year) %>% summarise(Mean_Male = round(mean(CharlsonIndex, na.rm=T),2),
                                                             SD_Male = round(sd(CharlsonIndex, na.rm=T),2))

total_f <- new_patients %>% filter(gender==2) %>%  group_by(study_year) %>% summarise(Mean_Female = round(mean(CharlsonIndex, na.rm=T),2),
                                                                                      SD_Female = round(sd(CharlsonIndex, na.rm=T),2))

table_1 <- total %>% left_join(total_m) %>% left_join(total_f)

names(table_1)[1] <- "Years"

# study year groups
total <- new_patients %>% group_by(year_group) %>% summarise(Mean = round(mean(CharlsonIndex, na.rm=T),2),
                                                             SD = round(sd(CharlsonIndex, na.rm=T),2))


total_m <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(Mean_Male = round(mean(CharlsonIndex, na.rm=T),2),
                                                                                      SD_Male = round(sd(CharlsonIndex, na.rm=T),2))

total_f <- new_patients %>% filter(gender==2) %>%  group_by(year_group) %>% summarise(Mean_Female = round(mean(CharlsonIndex, na.rm=T),2),
                                                                                      SD_Female = round(sd(CharlsonIndex, na.rm=T),2))

table_2 <- total %>% left_join(total_m) %>% left_join(total_f)
names(table_2)[1] <- "Years"

table_6.4 <- rbind(table_1,table_2)

write_csv(table_6.4,"results/new_patients/6.4/6.4.csv")




sample(new_patients$pt,10)

