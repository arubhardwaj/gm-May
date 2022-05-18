library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

new_patients <- read_csv("dt_new_pt.csv")



new_patients %>% group_by(study_year) %>% summarise(HbA1c_DCCT=mean(HbA1c_DCCT,na.rm=T)) %>% 
  ggplot(aes(x=study_year, y=HbA1c_DCCT))+
  geom_point()+
  xlab("Study Year")
