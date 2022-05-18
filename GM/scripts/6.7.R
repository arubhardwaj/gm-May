
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

# Table 6.7 ---------------------------------------------------------------

table6.7_1 <- all_patients %>% group_by(study_year) %>% 
  summarise(Metformin = sum(count_current_met_rx)/length(count_current_met_rx)*100,
            Sulfonylurea = sum(count_current_sul_rx)/length(count_current_sul_rx)*100,
            insulin = sum(count_current_ins_rx)/length(count_current_ins_rx)*100,
            `basal insulin` = sum(count_current_basal_rx)/length(count_current_basal_rx)*100,
            `bolus insulin` = sum(count_current_bolus_rx)/length(count_current_bolus_rx)*100,
            `premixed insulin` = sum(count_current_premix_rx)/length(count_current_premix_rx)*100,
            acarbose = sum(count_current_acrbose_rx)/length(count_current_acrbose_rx)*100,
            nateglinide = sum(count_current_nateglinide_rx)/length(count_current_nateglinide_rx)*100,
            repaglinide = sum(count_current_repaglinide_rx)/length(count_current_repaglinide_rx)*100,
            pioglitazone = sum(count_current_pioglitazone_rx)/length(count_current_pioglitazone_rx)*100,
            rosiglitazone = sum(count_current_rosiglitazone_rx)/length(count_current_rosiglitazone_rx)*100,
            alogliptin = sum(count_current_alogliptin_rx)/length(count_current_alogliptin_rx)*100,
            linagliptin = sum(count_current_linagliptin_rx)/length(count_current_linagliptin_rx)*100,
            saxagliptin = sum(count_current_saxagliptin_rx)/length(count_current_saxagliptin_rx)*100,
            sitagliptin = sum(count_current_sitagliptin_rx)/length(count_current_sitagliptin_rx)*100,
            vildagliptin = sum(count_current_vildagliptin_rx)/length(count_current_vildagliptin_rx)*100,
            exenatide = sum(count_current_exenatide_rx)/length(count_current_exenatide_rx)*100,
            liraglutide = sum(count_current_liraglutide_rx)/length(count_current_liraglutide_rx)*100,
            lixisenatide = sum(count_current_lixisenatide_rx)/length(count_current_lixisenatide_rx)*100,
            canagliflozin = sum(count_current_canagliflozin_rx)/length(count_current_canagliflozin_rx)*100,
            dapagliflozin = sum(count_current_dapagliflozin_rx)/length(count_current_dapagliflozin_rx)*100,
            empagliflozin = sum(count_current_empagliflozin_rx)/length(count_current_empagliflozin_rx)*100,
            others = sum(count_current_other_rx)/length(count_current_other_rx)*100)



table6.7_2 <- all_patients %>% group_by(year_group) %>% 
  summarise(Metformin = sum(count_current_met_rx)/length(count_current_met_rx)*100,
            Sulfonylurea = sum(count_current_sul_rx)/length(count_current_sul_rx)*100,
            insulin = sum(count_current_ins_rx)/length(count_current_ins_rx)*100,
            `basal insulin` = sum(count_current_basal_rx)/length(count_current_basal_rx)*100,
            `bolus insulin` = sum(count_current_bolus_rx)/length(count_current_bolus_rx)*100,
            `premixed insulin` = sum(count_current_premix_rx)/length(count_current_premix_rx)*100,
            acarbose = sum(count_current_acrbose_rx)/length(count_current_acrbose_rx)*100,
            nateglinide = sum(count_current_nateglinide_rx)/length(count_current_nateglinide_rx)*100,
            repaglinide = sum(count_current_repaglinide_rx)/length(count_current_repaglinide_rx)*100,
            pioglitazone = sum(count_current_pioglitazone_rx)/length(count_current_pioglitazone_rx)*100,
            rosiglitazone = sum(count_current_rosiglitazone_rx)/length(count_current_rosiglitazone_rx)*100,
            alogliptin = sum(count_current_alogliptin_rx)/length(count_current_alogliptin_rx)*100,
            linagliptin = sum(count_current_linagliptin_rx)/length(count_current_linagliptin_rx)*100,
            saxagliptin = sum(count_current_saxagliptin_rx)/length(count_current_saxagliptin_rx)*100,
            sitagliptin = sum(count_current_sitagliptin_rx)/length(count_current_sitagliptin_rx)*100,
            vildagliptin = sum(count_current_vildagliptin_rx)/length(count_current_vildagliptin_rx)*100,
            exenatide = sum(count_current_exenatide_rx)/length(count_current_exenatide_rx)*100,
            liraglutide = sum(count_current_liraglutide_rx)/length(count_current_liraglutide_rx)*100,
            lixisenatide = sum(count_current_lixisenatide_rx)/length(count_current_lixisenatide_rx)*100,
            canagliflozin = sum(count_current_canagliflozin_rx)/length(count_current_canagliflozin_rx)*100,
            dapagliflozin = sum(count_current_dapagliflozin_rx)/length(count_current_dapagliflozin_rx)*100,
            empagliflozin = sum(count_current_empagliflozin_rx)/length(count_current_empagliflozin_rx)*100,
            others = sum(count_current_other_rx)/length(count_current_other_rx)*100)



names(table6.7_1)[1] <- "Year"
names(table6.7_2)[1] <- "Year"

table_6.7 <- rbind(table6.7_1, table6.7_2)



write_csv(table_6.7 , "results/all_patients/6.7/6.7.csv")

