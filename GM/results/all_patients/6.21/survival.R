
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

all_patients <- all_patients %>% mutate(guideline = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))

ggsurvplot_combine()




all_patients$dead <- ifelse(is.na(all_patients$deathdate)==T, 0,1)
all_patients$time <- (lubridate::mdy(all_patients$deathdate) - lubridate::mdy(all_patients$dm_presentation))

all_patients$time <- all_patients$time /360

all_patients_d <- all_patients %>% group_by(pt) %>% mutate(death_cum = cumsum(dead))


Surv(all_patients$time,all_patients$dead)

fit <- survfit(Surv(time,dead)~guideline,data= all_patients_d)
fit


ggsurvplot(fit,
           conf.int=TRUE, # add confidence intervals
           pval=TRUE, # show the p-value for the log-rank test
           risk.table=FALSE, # show a risk table below the plot
           palette=c("dodgerblue4", "orchid2","red","blue"), # change colors of the groups
           title="Kaplan-Meier Curve for Lung Cancer Survival",
           pval.method=1,
           ggtheme = theme_survminer(),
           cumevents = T,  test.for.trend = FALSE)



knitr::stitch("results/all_patients/6.21/survival.R")

latexpdf::tex2pdf('survival.tex')
