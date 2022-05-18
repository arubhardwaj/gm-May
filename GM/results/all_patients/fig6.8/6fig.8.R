
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


# Met
p1 <- all_patients %>% group_by(study_year) %>% mutate(log_met = log(sum(count_current_met_rx)/length(count_current_met_rx)*100)) %>% 
  select(study_year, log_met) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_met))+
  ggtitle('Met')



p2 <- all_patients %>% group_by(study_year) %>% mutate(log_sul = log(sum(count_current_sul_rx)/length(count_current_sul_rx)*100)) %>% 
  select(study_year, log_sul) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_sul))+
  ggtitle('Sul')



p3 <- all_patients %>% group_by(study_year) %>% mutate(log_ins = log(sum(count_current_ins_rx)/length(count_current_ins_rx)*100)) %>% 
  select(study_year, log_ins) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_ins))+
  ggtitle("Ins")



p4 <- all_patients %>% group_by(study_year) %>% mutate(log_sitagliptin = log(sum(count_current_sitagliptin_rx)/length(count_current_sitagliptin_rx)*100)) %>% 
  select(study_year, log_sitagliptin) %>% unique() %>% 
  ggplot()+
  geom_line(aes(x=study_year, y=log_sitagliptin))+
  ggtitle("sitagliptin")



ggpubr::ggarrange(p1,p2,p3,p4)
