
# Packages ----------------------------------------------------------------
library(tidyverse)
library(survival)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(survminer)

getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")


# Define year groups:

all_patients <- all_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))






all_patients$dead <- ifelse(is.na(all_patients$deathdate)==T, 0,1)
all_patients$time <- (lubridate::mdy(all_patients$deathdate) - lubridate::mdy(all_patients$dm_presentation))/12


fit <- survfit(Surv(time,dead)~year_group,data= all_patients)
fit

ggsurvplot(
  fit = survfit(Surv(time,dead)~year_group,data= all_patients), 
  xlab = "Days", 
  ylab = "Overall survival probability")
