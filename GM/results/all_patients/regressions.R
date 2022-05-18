
# Packages ----------------------------------------------------------------
library(tidyverse)
library("scales")        # variable transforms, formatting tables
library("stargazer")     # formatting regression output
library("dplyr")         # data wrangling
library("pander")        # nice tables

getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")



# Define year groups:

all_patients <- all_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))



# Table 6.26 --------------------------------------------------------------

data_its <- all_patients %>% select(study_year,HbA1c_DCCT, year_group)

model <- lm(HbA1c_DCCT ~ year_group, data = data_its)
stargazer(model, type = "text")

summary(model)

ggplot(data_its,aes(x=study_year,y=HbA1c_DCCT,shape = year_group))+
  geom_point()+
  geom_smooth(method = "lm")


# Table 6.27 Mean HbA1c by guideline periods (all patients) ---------------

all_patients




















