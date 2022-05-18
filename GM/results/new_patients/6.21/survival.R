
# Packages ----------------------------------------------------------------
library(tidyverse)
library(survival)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(survminer)

getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

new_patients <- read_csv("dt_new_pt.csv")


# Define year groups:

#new_patients$last_month <- lubridate::mdy(new_patients$last_month)
new_patients <- new_patients %>% mutate(guideline = ifelse(last_month>='2000-01-31' & last_month <= '2002-10-01',"2000-2002",
                                                            ifelse(last_month>='2002-10-01' & last_month <= '2008-05-01',"2002-2008",
                                                                   ifelse(last_month>='2008-05-01' & last_month <= '2012-07-01',"2008-2012",ifelse(last_month>='2012-07-01' & last_month <= '2014-03-31',"2012-2014","NA")))))









new_patients$dead <- ifelse(is.na(new_patients$deathdate)==T, 0,1)
new_patients$time <- (lubridate::mdy(new_patients$deathdate) - lubridate::mdy(new_patients$dm_presentation))

new_patients$time <- new_patients$time /360

new_patients_d <- new_patients %>% group_by(pt) %>% mutate(death_cum = cumsum(dead))


Surv(new_patients$time,new_patients$dead)

fit <- survfit(Surv(time,dead)~as.factor(guideline),data= new_patients_d)
fit


ggsurvplot(fit)



knitr::stitch("results/new_patients/6.21/survival.R")

latexpdf::tex2pdf('survival.tex')
