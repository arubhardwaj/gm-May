

# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

new_patients <- read_csv("dt_new_pt.csv")

# All patients



# total 
total_patients <- new_patients$pt %>% unique() %>% length()

s <- new_patients %>%filter(gender==2) %>%  select(pt,year_group)
s <- s[unique(s$pt),]

s  %>% count(year_group)


male_patients <- new_patients %>% select(pt, gender) %>% filter(gender==1) %>% unique() %>% nrow()
female_patients <- new_patients %>% select(pt, gender) %>% filter(gender==2) %>% unique() %>% nrow()

total <- cbind(total_patients,male_patients,female_patients) %>% data.frame()

write_csv(total, "results/new_patients/6.1/total.csv")

# Define year groups:

new_patients <- new_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))



# Mean Age
total_mean_age <- new_patients %>% group_by(year_group) %>% summarise(mean_age = mean(age_this_month))


new_patients %>% summarise(mean_age = mean(age_this_month,na.rm=T))
new_patients %>% summarise(sd_age = sd(age_this_month,na.rm=T))

new_patients %>% group_by(year_group) %>% summarise(sdn_age = sd(age_this_month,na.rm=T))

new_patients %>% filter(gender==1)%>% summarise(mean_age = mean(age_this_month,na.rm=T))
new_patients %>%  filter(gender==2)%>% summarise(mean_age = mean(age_this_month,na.rm=T))

male_mean_age <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(mean_age = mean(age_this_month))
female_mean_age <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(mean_age = mean(age_this_month))
total_mean_age <- na.omit(total_mean_age)
mean_age <- cbind(total_mean_age,male_mean_age=male_mean_age$mean_age,female_mean_age=female_mean_age$mean_age) %>% data.frame()

write_csv(mean_age, "results/new_patients/6.1/mean_age.csv")


# HbA1c_DCCT
total_mean_HbA1c_DCCT <- new_patients %>% group_by(year_group) %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))

new_patients %>% group_by(year_group) %>% summarise(sd_HbA1c_DCCT = sd(HbA1c_DCCT, na.rm=T))
new_patients  %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
new_patients %>% filter(gender==1)  %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
new_patients  %>% filter(gender==2)   %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))


male_mean_HbA1c_DCCT <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
female_mean_HbA1c_DCCT <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
total_mean_HbA1c_DCCT <- na.omit(total_mean_HbA1c_DCCT)
mean_HbA1c_DCCT <- cbind(total_mean_HbA1c_DCCT,male_mean_HbA1c_DCCT=male_mean_HbA1c_DCCT$male_HbA1c_DCCT,female_mean_HbA1c_DCCT=female_mean_HbA1c_DCCT$female_HbA1c_DCCT) %>% data.frame()

write_csv(mean_HbA1c_DCCT, "results/new_patients/6.1/mean_HbA1c_DCCT.csv")


# HbA1c_IFCC
total_mean_HbA1c_IFCC <- new_patients %>% group_by(year_group) %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))


new_patients %>% summarise(mean_HbA1c_IFCCT = mean(HbA1c_IFCC, na.rm=T))
new_patients %>% group_by(year_group) %>%  summarise(sd_HbA1c_IFCCT = sd(HbA1c_IFCC, na.rm=T))
new_patients %>%  summarise(sd_HbA1c_IFCCT = sd(HbA1c_IFCC, na.rm=T))

new_patients %>% filter(gender==1) %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))
new_patients %>% filter(gender==2) %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))


male_mean_HbA1c_IFCC <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))
female_mean_HbA1c_IFCC <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))
total_mean_HbA1c_IFCC<-na.omit(total_mean_HbA1c_IFCC)
mean_HbA1c_IFCC <- cbind(total_mean_HbA1c_IFCC,male_HbA1c_IFCC=male_mean_HbA1c_IFCC$male_HbA1c_IFCC,female_HbA1c_IFCC=female_mean_HbA1c_IFCC$female_HbA1c_IFCC) %>% data.frame()


write_csv(mean_HbA1c_IFCC, "results/new_patients/6.1/mean_HbA1c_IFCC.csv")


# weight
total_mean_weight <- new_patients %>% group_by(year_group) %>% summarise(mean_weight = mean(weight, na.rm=T))

new_patients %>% group_by(year_group) %>% summarise(sd_weight = sd(weight, na.rm=T))
new_patients %>% summarise(sd_weight = sd(weight, na.rm=T))
new_patients %>% summarise(mean_weight = mean(weight, na.rm=T))
new_patients %>% filter(gender==1)%>% summarise(mean_weight = mean(weight, na.rm=T))
new_patients %>% filter(gender==2) %>% summarise(mean_weight = mean(weight, na.rm=T))

male_mean_weight <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_weight = mean(weight, na.rm=T))
female_mean_weight <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_weight = mean(weight, na.rm=T))

total_mean_weight <- na.omit(total_mean_weight)

mean_weight <- cbind(total_mean_weight,male_weight=male_mean_weight$male_weight,female_weight=female_mean_weight$female_weight) %>% data.frame()

write_csv(mean_weight, "results/new_patients/6.1/mean_weight.csv")




# smoking
total_percentage_smoking <- new_patients %>% group_by(year_group) %>% summarise(percentage_smoking = sum(smoking, na.rm=T)/length(pt)*100)


new_patients  %>% summarise(percentage_smoking = sum(smoking, na.rm=T)/length(pt)*100)
new_patients %>% filter(gender==1)  %>% summarise(percentage_smoking = sum(smoking, na.rm=T)/length(pt)*100)
new_patients  %>% filter(gender==2) %>% summarise(percentage_smoking = sum(smoking, na.rm=T)/length(pt)*100)


male_percentage_smoking <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_smoking = sum(smoking, na.rm=T)/length(pt)*100)
female_percentage_smoking <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_smoking = sum(smoking, na.rm=T)/length(pt)*100)
total_percentage_smoking <- na.omit(total_percentage_smoking)
percentage_smoking <- cbind(total_percentage_smoking,male_smoking=male_percentage_smoking$male_smoking,female_smoking=female_percentage_smoking$female_smoking) %>% data.frame()


write_csv(percentage_smoking, "results/new_patients/6.1/percentage_smoking.csv")


# Ex smoker


new_patients %>% filter(smoking==2) %>% group_by(year_group) %>% count_by()
new_patients %>% filter(gender==1) %>%  filter(smoking==2) %>% group_by(year_group) %>% count_by()
new_patients %>% filter(gender==2) %>% filter(smoking==2) %>% group_by(year_group) %>% count_by()
new_patients %>% filter(gender==2) %>% filter(smoking==2)  %>% count_by()


# total_cholesterol_mmoll
total_mean_total_cholesterol_mmoll <- new_patients %>% group_by(year_group) %>% summarise(mean_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))

new_patients %>% group_by(year_group) %>% summarise(sd_total_cholesterol_mmoll = sd(total_cholesterol_mmoll, na.rm=T))
new_patients%>% summarise(mean_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))
new_patients %>% filter(gender==1)%>% summarise(mean_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))
new_patients%>% filter(gender==2)%>% summarise(mean_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))

male_mean_total_cholesterol_mmoll <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))
female_mean_total_cholesterol_mmoll <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))

total_mean_total_cholesterol_mmoll <- na.omit(total_mean_total_cholesterol_mmoll)

mean_total_cholesterol_mmoll <- cbind(total_mean_total_cholesterol_mmoll,male_total_cholesterol_mmoll=male_mean_total_cholesterol_mmoll$male_total_cholesterol_mmoll,female_total_cholesterol_mmoll=female_mean_total_cholesterol_mmoll$female_total_cholesterol_mmoll) %>% data.frame()


write_csv(mean_total_cholesterol_mmoll, "results/new_patients/6.1/mean_total_cholesterol_mmoll.csv")




# % of total cholesterol
new_patients <- new_patients %>% mutate(chol_gt_4 = ifelse(total_cholesterol_mmoll>=4,1,0))


# chol_gt_4
total_sum_chol_gt_4 <- new_patients %>% group_by(year_group) %>% summarise(sum_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)

new_patients  %>% summarise(sum_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)
new_patients %>% filter(gender==2)  %>% summarise(sum_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)
new_patients  %>% summarise(sum_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)

new_patients  %>% summarise(sum_chol_gt_4 = sd(chol_gt_4, na.rm=T))

new_patients %>% group_by(year_group) %>% summarise(sum_chol_gt_4 = sd(chol_gt_4, na.rm=T))

male_sum_chol_gt_4 <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)
female_sum_chol_gt_4 <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)
total_sum_chol_gt_4 <- na.omit(total_sum_chol_gt_4)
sum_chol_gt_4 <- cbind(total_sum_chol_gt_4,male_chol_gt_4=male_sum_chol_gt_4$male_chol_gt_4,female_chol_gt_4=female_sum_chol_gt_4$female_chol_gt_4) %>% data.frame()


write_csv(sum_chol_gt_4, "results/new_patients/6.1/sum_chol_gt_4.csv")


# GP visits total
total_gp <- new_patients$GP_contacts_this_month %>% length()

new_patients %>% group_by(year_group) %>% summarise(tot = sd(GP_contacts_this_month))
new_patients %>% summarise(tot = sd(GP_contacts_this_month,na.rm=T))


new_patients %>% filter(gender==1) %>% group_by(year_group) %>% summarise(tot = sum(GP_contacts_this_month))
new_patients %>% filter(gender==2) %>% group_by(year_group) %>% summarise(tot = sum(GP_contacts_this_month))

new_patients %>% group_by(year_group) %>% summarise(tot = sum(GP_contacts_this_month))
new_patients %>% summarise(tot = sum(GP_contacts_this_month,na.rm = T))

total_male_gp <- new_patients %>% filter(gender==1) %>% select(GP_contacts_this_month) %>% nrow()
total_female_gp <- new_patients %>% filter(gender==2) %>% select(GP_contacts_this_month) %>% nrow()

total_gp <- cbind(total_gp, total_male_gp, total_female_gp) %>% data.frame()

write_csv(total_gp, "results/new_patients/6.1/total_gp.csv")

# MEan BMI


new_patients %>% summarise(t=mean(BMI,na.rm=T))
new_patients %>% group_by(year_group) %>% summarise(t=mean(BMI,na.rm=T))
new_patients %>% summarise(t=sd(BMI,na.rm=T))
new_patients %>% group_by(year_group) %>% summarise(t=sd(BMI,na.rm=T))
new_patients %>% filter(gender==1)%>% group_by(year_group) %>% summarise(t=sd(BMI,na.rm=T))
new_patients %>% filter(gender==2) %>% group_by(year_group) %>% summarise(t=sd(BMI,na.rm=T))

new_patients %>% filter(gender==1) %>% summarise(t=sd(BMI,na.rm=T))
new_patients %>% filter(gender==2)%>% summarise(t=sd(BMI,na.rm=T))

# CCS

# MEan BMI


new_patients %>% summarise(t=mean(CharlsonIndex,na.rm=T))
new_patients %>% group_by(year_group) %>% summarise(t=mean(CharlsonIndex,na.rm=T))
new_patients %>% summarise(t=sd(BMI,na.rm=T))
new_patients %>% group_by(year_group) %>% summarise(t=sd(CharlsonIndex,na.rm=T))
new_patients %>% filter(gender==1)%>% group_by(year_group) %>% summarise(t=mean(CharlsonIndex,na.rm=T))
new_patients %>% filter(gender==2) %>% group_by(year_group) %>% summarise(t=mean(CharlsonIndex,na.rm=T))

new_patients %>% filter(gender==1) %>% summarise(t=sd(CharlsonIndex,na.rm=T))
new_patients %>% filter(gender==2)%>% summarise(t=sd(CharlsonIndex,na.rm=T))


# GP_contacts_this_month ? patients
total_sum_GP_contacts_this_month <- new_patients %>% group_by(year_group) %>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))


new_patients  %>% summarise(sd_GP_contacts_this_month = sd(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))


new_patients  %>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))
new_patients %>% filter(gender==1)  %>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))
new_patients %>% filter(gender==2)  %>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))

new_patients  %>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))

male_sum_GP_contacts_this_month <- new_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))
female_sum_GP_contacts_this_month <- new_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))

sum_GP_contacts_this_month <- cbind(na.omit(total_sum_GP_contacts_this_month),male_GP_contacts_this_month=male_sum_GP_contacts_this_month$male_GP_contacts_this_month,female_GP_contacts_this_month=female_sum_GP_contacts_this_month$female_GP_contacts_this_month) %>% data.frame()

write_csv(sum_GP_contacts_this_month, "results/new_patients/6.1/sum_GP_contacts_this_month.csv")


# LVD per 1000

total_lvd <- new_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sum(current_lvd)/1000)
new_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sd(current_lvd))

total_male_lvd <- new_patients %>% filter(gender==1) %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sum(current_lvd)/1000)

total_female_lvd <- new_patients %>% filter(gender==2) %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sum(current_lvd)/1000)

lvd_per_1000 <- cbind(na.omit(total_lvd), total_male_lvd=total_male_lvd$LVD_per_1000,total_female_lvd=total_female_lvd$LVD_per_1000) %>% data.frame()

write_csv(lvd_per_1000, "results/new_patients/6.1/lvd_per_1000.csv")



# MI events per 1000

total_mi_events <- new_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sum(current_mi)/1000)

new_patients %>% filter(gender==1) %>% summarise(mi_events_per_1000 = sum(current_mi,na.rm = T)/1000)
new_patients %>% filter(gender==2) %>% summarise(mi_events_per_1000 = sum(current_mi,na.rm = T)/1000)

new_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sd(current_mi))

total_male_mi_events <- new_patients %>% filter(gender==1) %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sum(current_mi)/1000)

total_female_mi_events <- new_patients %>% filter(gender==2) %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sum(current_mi)/1000)

mi_events_per_1000 <- cbind(na.omit(total_mi_events), total_male_mi_events=total_male_mi_events$mi_events_per_1000,total_female_mi_events=total_female_mi_events$mi_events_per_1000) %>% data.frame()

write_csv(mi_events_per_1000, "results/new_patients/6.1/mi_events_per_1000.csv")






















