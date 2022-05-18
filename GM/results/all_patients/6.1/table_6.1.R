

# Packages ----------------------------------------------------------------
library(tidyverse)


getwd()
setwd("/home/arubhardwaj/Downloads/GM") # define WD

all_patients <- read_csv("DM Monthly 01 04 2018.csv")



# total 
total_patients <- all_patients$pt %>% unique() %>% length()

male_patients <- all_patients %>% select(pt, gender) %>% filter(gender==1) %>% unique() %>% nrow()
female_patients <- all_patients %>% select(pt, gender) %>% filter(gender==2) %>% unique() %>% nrow()

total <- cbind(total_patients,male_patients,female_patients) %>% data.frame()

write_csv(total, "results/all_patients/6.1/total.csv")

# Define year groups:

all_patients <- all_patients %>% mutate(year_group = ifelse(study_year>=2000 & study_year<=2002, "2000-2002",
                                                            ifelse(study_year>=2002 & study_year<=2008, "2002-2008",
                                                                   ifelse(study_year>=2008 & study_year<=2012, "2008-2012",
                                                                          ifelse(study_year>=2012 & study_year<=2014, "2012-2014","NULL")))))



# all patients

year_wise_total <-all_patients %>% group_by(year_group) %>%  select(pt) %>% unique() %>%  
  count()



names(year_wise_total) <- c("year","total")

year_wise_total_m <-all_patients %>% filter(gender==1) %>% group_by(year_group) %>%  select(pt) %>% unique() %>%  
  count()

names(year_wise_total_m) <- c("year","males")

year_wise_total_f <-all_patients %>% filter(gender==2) %>% group_by(year_group) %>%  select(pt) %>% unique() %>%  
  count()
names(year_wise_total_f) <- c("year","females")

year_wise_total <- year_wise_total %>% left_join(year_wise_total_m) %>% 
 left_join(year_wise_total_f)

write_csv(year_wise_total,"results/all_patients/6.1/year_wise_total.csv")


# Mean Age
total_mean_age <- all_patients %>% group_by(year_group) %>% summarise(mean_age = mean(age_this_month))
### sd
sd <- all_patients %>% group_by(year_group) %>% summarise(sd_age = sd(age_this_month))
write_csv(sd,"results/all_patients/6.1/sd_age.csv")

male_mean_age <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(mean_age = mean(age_this_month))
female_mean_age <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(mean_age = mean(age_this_month))

mean_age <- cbind(total_mean_age,male_mean_age=male_mean_age$mean_age,female_mean_age=female_mean_age$mean_age) %>% data.frame()

write_csv(mean_age, "results/all_patients/6.1/mean_age.csv")
### overall

all_patients %>% summarise(mean_age = mean(age_this_month))

### male
all_patients %>% filter(gender==1) %>% summarise(mean_age = mean(age_this_month))

###female
all_patients %>% filter(gender==2) %>% summarise(mean_age = mean(age_this_month))

# HbA1c_DCCT
total_mean_HbA1c_DCCT <- all_patients %>% group_by(year_group) %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
total_sd_HbA1c_DCCT <- all_patients %>% group_by(year_group) %>% summarise(sd_HbA1c_DCCT = sd(HbA1c_DCCT, na.rm=T))



male_mean_HbA1c_DCCT <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
female_mean_HbA1c_DCCT <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))

mean_HbA1c_DCCT <- cbind(total_mean_HbA1c_DCCT,male_mean_HbA1c_DCCT=male_mean_HbA1c_DCCT$male_HbA1c_DCCT,female_mean_HbA1c_DCCT=female_mean_HbA1c_DCCT$female_HbA1c_DCCT) %>% data.frame()

write_csv(mean_HbA1c_DCCT, "results/all_patients/6.1/mean_HbA1c_DCCT.csv")

write_csv(total_sd_HbA1c_DCCT, "results/all_patients/6.1/total_sd_HbA1c_DCCT.csv")


all_patients %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
all_patients %>% summarise(sd_HbA1c_DCCT = sd(HbA1c_DCCT, na.rm=T))
all_patients %>% filter(gender==1) %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))
all_patients%>% filter(gender==2) %>% summarise(mean_HbA1c_DCCT = mean(HbA1c_DCCT, na.rm=T))


# HbA1c_IFCC
total_mean_HbA1c_IFCC <- all_patients %>% group_by(year_group) %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))

total_sd_HbA1c_IFCC <- all_patients %>% group_by(year_group) %>% summarise(sd_HbA1c_IFCC = sd(HbA1c_IFCC, na.rm=T))

male_mean_HbA1c_IFCC <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))
female_mean_HbA1c_IFCC <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))

mean_HbA1c_IFCC <- cbind(total_mean_HbA1c_IFCC,male_HbA1c_IFCC=male_mean_HbA1c_IFCC$male_HbA1c_IFCC,female_HbA1c_IFCC=female_mean_HbA1c_IFCC$female_HbA1c_IFCC) %>% data.frame()




all_patients %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))
all_patients %>% summarise(sd_HbA1c_IFCC = sd(HbA1c_IFCC, na.rm=T))
all_patients %>% filter(gender==1) %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))
all_patients%>% filter(gender==2) %>% summarise(mean_HbA1c_IFCC = mean(HbA1c_IFCC, na.rm=T))


write_csv(mean_HbA1c_IFCC, "results/all_patients/6.1/mean_HbA1c_IFCC.csv")
write_csv(total_sd_HbA1c_IFCC, "results/all_patients/6.1/sd_HbA1c_IFCC.csv")


# weight
total_mean_weight <- all_patients %>% group_by(year_group) %>% summarise(mean_weight = mean(weight, na.rm=T))
total_sd_weight <- all_patients %>% group_by(year_group) %>% summarise(sd_weight = sd(weight, na.rm=T))



male_mean_weight <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_weight = mean(weight, na.rm=T))
female_mean_weight <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_weight = mean(weight, na.rm=T))

mean_weight <- cbind(total_mean_weight,male_weight=male_mean_weight$male_weight,female_weight=female_mean_weight$female_weight) %>% data.frame()

write_csv(mean_weight, "results/all_patients/6.1/mean_weight.csv")
write_csv(total_sd_weight, "results/all_patients/6.1/sd_weight.csv")


all_patients %>% summarise(mean_weight = mean(weight, na.rm=T))
all_patients %>% summarise(sd_weight = sd(weight, na.rm=T))
all_patients %>% filter(gender==1) %>% summarise(mean_weight = mean(weight, na.rm=T))
all_patients%>% filter(gender==2) %>% summarise(mean_weight = mean(weight, na.rm=T))


# mean BMI

all_patients %>% summarise(mean_bmi = mean(BMI,na.rm=T))
all_patients %>% filter(gender==1) %>% summarise(mean_bmi = mean(BMI,na.rm=T))
all_patients %>% summarise(mean_bmi = sd(BMI,na.rm=T))

all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(mean_bmi = mean(BMI,na.rm=T)) %>% View()
all_patients %>% filter(gender==2) %>% group_by(year_group)%>% summarise(mean_bmi = mean(BMI,na.rm=T)) %>% View()

all_patients %>% summarise(mean_bmi = mean(BMI,na.rm=T))

# ccs


# mean BMI

all_patients %>% summarise(mean_bmi = mean(CharlsonIndex,na.rm=T))
all_patients %>% filter(gender==2) %>% summarise(mean_bmi = mean(CharlsonIndex,na.rm=T))
all_patients %>% group_by(year_group) %>%  summarise(mean_bmi = mean(CharlsonIndex,na.rm=T))

all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(mean_bmi = mean(CharlsonIndex,na.rm=T)) %>% View()
all_patients %>% filter(gender==2) %>% group_by(year_group)%>% summarise(mean_bmi = mean(CharlsonIndex,na.rm=T)) %>% View()

all_patients %>% summarise(mean_bmi = mean(BMI,na.rm=T))


# smoking
total_percentage_smoking <- all_patients %>% group_by(year_group) %>% summarise(percentage_smoking = sum(smoking, na.rm=T)/length(pt)*100)
all_patients %>% summarise(percentage_smoking = sd(smoking,na.rm=T))

male_percentage_smoking <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_smoking = sum(smoking, na.rm=T)/length(pt)*100)
female_percentage_smoking <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_smoking = sum(smoking, na.rm=T)/length(pt)*100)

percentage_smoking <- cbind(total_percentage_smoking,male_smoking=male_percentage_smoking$male_smoking,female_smoking=female_percentage_smoking$female_smoking) %>% data.frame()

all_patients %>% filter(gender==2)  %>% summarise(male_smoking = sum(smoking, na.rm=T)/length(pt)*100)

write_csv(percentage_smoking, "results/all_patients/6.1/percentage_smoking.csv")


# non smokers
library(tidytidbits)

ns <- all_patients %>% group_by(year_group) %>% 
  count_by(smoking==2) 
ns

ns <- all_patients %>% filter(gender==1)  %>% 
  count_by(smoking==2) 
ns

# total_cholesterol_mmoll
total_mean_total_cholesterol_mmoll <- all_patients %>% group_by(year_group) %>% summarise(mean_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))
all_patients %>% group_by(year_group) %>% summarise(sd_total_cholesterol_mmoll = sd(total_cholesterol_mmoll, na.rm=T))



all_patients %>% filter(gender==2)  %>% summarise(mean_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))

male_mean_total_cholesterol_mmoll <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))
female_mean_total_cholesterol_mmoll <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_total_cholesterol_mmoll = mean(total_cholesterol_mmoll, na.rm=T))

mean_total_cholesterol_mmoll <- cbind(total_mean_total_cholesterol_mmoll,male_total_cholesterol_mmoll=male_mean_total_cholesterol_mmoll$male_total_cholesterol_mmoll,female_total_cholesterol_mmoll=female_mean_total_cholesterol_mmoll$female_total_cholesterol_mmoll) %>% data.frame()


write_csv(mean_total_cholesterol_mmoll, "results/all_patients/6.1/mean_total_cholesterol_mmoll.csv")




# % of total cholesterol
all_patients <- all_patients %>% mutate(chol_gt_4 = ifelse(total_cholesterol_mmoll>=4,1,0))

all_patients %>% group_by(year_group) %>% 
  summarise(sd=sd(chol_gt_4,na.rm=T))

all_patients %>% filter(gender==2) %>% 
  summarise(sum_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)

# chol_gt_4
total_sum_chol_gt_4 <- all_patients %>% group_by(year_group) %>% summarise(sum_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)
male_sum_chol_gt_4 <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)
female_sum_chol_gt_4 <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_chol_gt_4 = sum(chol_gt_4, na.rm=T)/length(chol_gt_4)*100)

sum_chol_gt_4 <- cbind(total_sum_chol_gt_4,male_chol_gt_4=male_sum_chol_gt_4$male_chol_gt_4,female_chol_gt_4=female_sum_chol_gt_4$female_chol_gt_4) %>% data.frame()


write_csv(sum_chol_gt_4, "results/all_patients/6.1/sum_chol_gt_4.csv")


# GP visits total
total_gp <- all_patients$GP_contacts_this_month %>% length()
total_male_gp <- all_patients %>% filter(gender==1) %>% select(GP_contacts_this_month) %>% nrow()
total_female_gp <- all_patients %>% filter(gender==2) %>% select(GP_contacts_this_month) %>% nrow()

total_gp <- cbind(total_gp, total_male_gp, total_female_gp) %>% data.frame()

write_csv(total_gp, "results/all_patients/6.1/total_gp.csv")



# GP_contacts_this_month ? patients
total_sum_GP_contacts_this_month <- all_patients %>% group_by(year_group) %>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))

all_patients %>% filter(gender==1%>% summarise(sum_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month)))


male_sum_GP_contacts_this_month <- all_patients %>% filter(gender==1) %>%  group_by(year_group) %>% summarise(male_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))
female_sum_GP_contacts_this_month <- all_patients %>% filter(gender==2)  %>%  group_by(year_group) %>% summarise(female_GP_contacts_this_month = sum(GP_contacts_this_month, na.rm=T)/length(GP_contacts_this_month))

sum_GP_contacts_this_month <- cbind(total_sum_GP_contacts_this_month,male_GP_contacts_this_month=male_sum_GP_contacts_this_month$male_GP_contacts_this_month,female_GP_contacts_this_month=female_sum_GP_contacts_this_month$female_GP_contacts_this_month) %>% data.frame()

write_csv(sum_GP_contacts_this_month, "results/all_patients/6.1/sum_GP_contacts_this_month.csv")


# LVD per 1000

total_lvd <- all_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sum(current_lvd)/1000)


total_male_lvd <- all_patients %>% filter(gender==1) %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sum(current_lvd)/1000)

total_female_lvd <- all_patients %>% filter(gender==2) %>% group_by(pt) %>% group_by(year_group) %>% summarise(LVD_per_1000 = sum(current_lvd)/1000)

lvd_per_1000 <- cbind(total_lvd, total_male_lvd=total_male_lvd$LVD_per_1000,total_female_lvd=total_female_lvd$LVD_per_1000) %>% data.frame()

write_csv(lvd_per_1000, "results/all_patients/6.1/lvd_per_1000.csv")



# MI events per 1000

total_mi_events <- all_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sum(current_mi)/1000)

total_male_mi_events <- all_patients %>% filter(gender==1) %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sum(current_mi)/1000)

total_female_mi_events <- all_patients %>% filter(gender==2) %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sum(current_mi)/1000)

mi_events_per_1000 <- cbind(total_mi_events, total_male_mi_events=total_male_mi_events$mi_events_per_1000,total_female_mi_events=total_female_mi_events$mi_events_per_1000) %>% data.frame()

write_csv(mi_events_per_1000, "results/all_patients/6.1/mi_events_per_1000.csv")

all_patients %>% group_by(pt) %>% group_by(year_group) %>% summarise(mi_events_per_1000 = sd(current_mi)/1000)



## total GP 

all_patients %>% group_by(year_group)  %>% summarise(mean=sd(GP_contacts_this_month,na.rm=T))

## gp per patients


all_patients_count <- all_patients  %>% count()



s <-all_patients  %>% summarise(sum=sum(GP_contacts_this_month,na.rm=T))

all_patients_count$n/s$sum













