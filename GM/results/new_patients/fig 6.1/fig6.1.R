

# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

new_patients <- read_csv("dt_new_pt.csv")


new_patients %>% group_by(study_year) %>% summarise(Average_Age=mean(age_this_month,na.rm = T)) %>% 
  ggplot(aes(x=study_year,y=Average_Age))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Averae Age")+
  xlab("Averae Age")
