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


# 6.4 fig -----------------------------------------------------------------


all_patients %>% group_by(study_year) %>% summarise(Mean_weight = round(mean(weight, na.rm=T),2)) %>% 
  ggplot()+
  geom_point(aes(x=study_year,y=Mean_weight))+
  xlab("Study Years")+
  ylab("Mean Weight")
