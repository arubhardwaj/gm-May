# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")

wt.sd <- function(x,wt) { 
  return( sqrt(wt.var(x,wt)) ) #return the standard deviation
} 

wt.var <- function(x,wt) {
  s = which(is.finite(x + wt)); wt = wt[s]; x = x[s] #remove NA info
  xbar = wt.mean(x,wt) #get the weighted mean
  return( sum(wt *(x-xbar)^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))) )
}

# Define year groups:

all_patients <- all_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))





model <- lm(data=all_patients, HbA1c_DCCT~as.factor(study_year)) 
stargazer::stargazer(model, type = "text")

smod <- tidy(model) 
smod

write_csv(smod,"results/all_patients/6.26/6.26.csv")



model <- lm(data=all_patients, HbA1c_DCCT~as.factor(year_group)) 
stargazer::stargazer(model, type = "text")

smod <- tidy(model) 
smod

write_csv(smod,"results/all_patients/6.26/6.26_grouped.csv")
