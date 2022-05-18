library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")



all_patients %>% group_by(study_year) %>% summarise(HbA1c_DCCT=mean(HbA1c_DCCT,na.rm=T)) %>% 
  ggplot(aes(x=study_year, y=HbA1c_DCCT))+
  geom_point()+
  xlab("Study Year")