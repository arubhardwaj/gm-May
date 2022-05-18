
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




# Met
p1 <- new_patients %>% group_by(study_year) %>% mutate(log_met = log(sum(count_current_met_rx)/length(count_current_met_rx)*100)) %>% 
  select(study_year, log_met) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_met))+
  ggtitle('Met')



p2 <- new_patients %>% group_by(study_year) %>% mutate(log_sul = log(sum(count_current_sul_rx)/length(count_current_sul_rx)*100)) %>% 
  select(study_year, log_sul) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_sul))+
  ggtitle('Sul')



p3 <- new_patients %>% group_by(study_year) %>% mutate(log_ins = log(sum(count_current_ins_rx)/length(count_current_ins_rx)*100)) %>% 
  select(study_year, log_ins) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_ins))+
  ggtitle("Ins")



p4 <- new_patients %>% group_by(study_year) %>% mutate(log_sitagliptin = log(sum(count_current_sitagliptin_rx)/length(count_current_sitagliptin_rx)*100)) %>% 
  select(study_year, log_sitagliptin) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_sitagliptin))+
  ggtitle("sitagliptin")



ggpubr::ggarrange(p1,p2,p3,p4)
