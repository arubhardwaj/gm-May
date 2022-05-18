

# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")


all_patients %>% group_by(study_year) %>% summarise(Average_Age=mean(age_this_month,na.rm = T)) %>% 
  ggplot(aes(x=study_year,y=Average_Age))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Averae Age")+
  xlab("Averae Age")
