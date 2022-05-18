# Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)

getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD


new_patients <- read_csv("dt_new_pt.csv")


# Define year groups:

#new_patients$last_month <- lubridate::mdy(new_patients$last_month)
new_patients <- new_patients %>% mutate(year_group = ifelse(last_month>='2000-01-31' & last_month <= '2002-10-01',"2000-2002",
                                                            ifelse(last_month>='2002-10-01' & last_month <= '2008-05-01',"2002-2008",
                                                                   ifelse(last_month>='2008-05-01' & last_month <= '2012-07-01',"2008-2012",ifelse(last_month>='2012-07-01' & last_month <= '2014-03-31',"2012-2014","NA")))))





wt.sd <- function(x,wt) { 
  return( sqrt(wt.var(x,wt)) ) #return the standard deviation
} 

wt.var <- function(x,wt) {
  s = which(is.finite(x + wt)); wt = wt[s]; x = x[s] #remove NA info
  xbar = wt.mean(x,wt) #get the weighted mean
  return( sum(wt *(x-xbar)^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))) )
}

model <- lm(data=new_patients, HbA1c_DCCT~as.factor(study_year)) 
stargazer::stargazer(model, type = "text")

smod <- tidy(model) 
smod

write_csv(smod,"results/new_patients/6.26/6.26.csv")



model <- lm(data=new_patients, HbA1c_DCCT~as.factor(year_group)) 
stargazer::stargazer(model, type = "text")

smod <- tidy(model) 
smod

write_csv(smod,"results/new_patients/6.26/6.26_grouped.csv")

