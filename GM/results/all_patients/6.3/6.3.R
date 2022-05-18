

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



table_6.3 <- new_patients %>% group_by(study_year) %>% summarise(mean_gulucose = round(mean(HbA1c_DCCT,na.rm=T),2),
                                                    sd_gulucose = round(sd(HbA1c_DCCT,na.rm=T),2))

table_6.3_yg <- new_patients %>% group_by(year_group) %>% summarise(mean_gulucose = round(mean(HbA1c_DCCT,na.rm=T),2),
                                                                 sd_gulucose = round(sd(HbA1c_DCCT,na.rm=T),2))

names(table_6.3_yg) <- c("Years","Mean","Standard Deviation")
names(table_6.3) <- c("Years","Mean","Standard Deviation")

table_6.3 <- rbind(table_6.3,table_6.3_yg)

write_csv(table_6.3, "results/new_patients/6.3/6.3.csv")

