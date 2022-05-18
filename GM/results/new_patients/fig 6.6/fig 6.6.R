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





# 6.4 fig -----------------------------------------------------------------


new_patients %>% group_by(study_year) %>% summarise(Mean_weight = round(mean(weight, na.rm=T),2)) %>% 
  ggplot()+
  geom_point(aes(x=study_year,y=Mean_weight))+
  xlab("Study Years")+
  ylab("Mean Weight")
