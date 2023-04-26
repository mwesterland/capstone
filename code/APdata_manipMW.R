#############################################
####Subject: Data Manipulation
####Author: Maggie Westerland, MCW Biostatistics
####Date: Mar 13th, 2023
####Notes: R code for data manipulation for A. Palatnik
#############################################


#### dependencies ####
library(tidyverse)
library(knitr)
library(gtsummary)
library(skimr)
library(DBI)
library(RSQLite)
library(lubridate)
setwd('/Volumes/stor/capstone')
####

#### notes ####
#ICD-10 hypertension == I10
# wouldn't we want pre_pregnancy bmi to be 9 months before delivery? return to pre
# pregnancy weight wouldn't make sense if we include bmi @ delivery.
####

#### data read in ####

#### use these for easier access after cleaning ####
load('dx_final.Rdata')
load('ob_final.Rdata')
load('dx_ob_merge_pipe.Rdata')
load('vitals_final.Rdata')
load('final_df.Rdata')
####

global_pt_num <- ob_final$patient_num

filename <- "/Volumes/stor/deid-3516-sa12575/sqlite-deid-3516-sa12575.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db)

####

#### ob ####
ob <- dbReadTable(db, "ob_gyn_mother_baby")

#if dates include times run lines below:
ob$mom_admit_dttm_shifted <-
  gsub(" .*", "", ob$mom_admit_dttm_shifted)
ob$mom_disch_dttm_shifted <-
  gsub(" .*", "", ob$mom_disch_dttm_shifted)
ob$baby_dob_shifted <- gsub(" .*", "", ob$baby_dob_shifted)
ob$baby_dod_shifted <- gsub(" .*", "", ob$baby_dod_shifted)
ob$baby_admit_dttm_shifted <-
  gsub(" .*", "", ob$baby_admit_dttm_shifted)
ob$baby_disch_dttm_shifted <-
  gsub(" .*", "", ob$baby_disch_dttm_shifted)

remove <- c('w','d')
ob$gest_age_wks <- str_remove_all(ob$gest_age_wks, paste(remove, collapse = "|"))
ob$gest_age_wks <- gsub(" ", ".", ob$gest_age_wks)

ob[ob == ""] <- NA

ob_pipe <- ob %>%
  select(
    -delivery_encounter_num,
    -del_dept_id,
    -mom_patient_num,
    -del_dept_name,
    -mom_disch_disp,
    -del_stage1_hrs,
    -del_stage2_hrs,
    -del_stage3_hrs,
    -bloodloss_del,
    -bloodloss_total,
    -birth_encounter_num,
    -baby_del_dept,
    -baby_los,
    -baby_disch_disp,
    -apgar1,
    -apgar5,
    -apgar10,
    -anesth_conc,
    -del_meth_id
  ) %>%
  mutate(ob_gravidity = as.factor(ob_gravidity)) %>% 
  mutate(ob_parity = as.factor(ob_parity)) %>% 
  mutate(ob_multiple_births = as.factor(ob_multiple_births)) %>% 
  mutate(
    mom_ethnicity = case_when(
      mom_ethnicity == 'Refused' ~ NA_character_,
      mom_ethnicity == 'Hispanic' ~ 'Hispanic',
      mom_ethnicity == 'Unknown' ~ NA_character_,
      mom_ethnicity == 'NI' ~ 'Other',
      mom_ethnicity == 'Non Hispanic' ~ 'NH'
    )
  ) %>%
  mutate(
    mom_race = case_when(
      mom_race == 'Unknown' ~ NA_character_,
      mom_race == 'Asian' ~ 'Other',
      mom_race == 'Patient Refused' ~ NA_character_,
      mom_race == 'Multiracial' ~ 'Other',
      mom_race == 'Native Hawaiian or Other Pacific Islander' ~ 'Other',
      mom_race == 'American Indian or Alaska Native' ~ 'Other',
      mom_race == 'White or Caucasian' ~ 'White or Caucasian',
      mom_race == 'Black or African American' ~ 'Black or African American',
      mom_race == 'Other' ~ 'Other',
    )
  ) %>%
  mutate(
    baby_ethnicity = case_when(
      baby_ethnicity == 'Refused' ~ NA_character_,
      baby_ethnicity == 'Unknown' ~ NA_character_,
      baby_ethnicity == 'Hispanic' ~ 'Hispanic',
      baby_ethnicity == 'NI' ~ 'Other',
      baby_ethnicity == 'Non Hispanic' ~ 'NH'
    )
  ) %>%
  mutate(
    baby_race = case_when(
      baby_race == 'Unknown' ~ NA_character_,
      baby_race == 'Asian' ~ 'Other',
      baby_race == 'Patient Refused' ~ NA_character_,
      baby_race == 'Multiracial' ~ 'Other',
      baby_race == 'Native Hawaiian or Other Pacific Islander' ~ 'Other',
      baby_race == 'American Indian or Alaska Native' ~ 'Other',
      baby_race == 'White or Caucasian' ~ 'White or Caucasian',
      baby_race == 'Black or African American' ~ 'Black or African American',
      baby_race == 'Other' ~ 'Other',
    )
  ) %>%
  mutate(
    mom_marital_status = case_when(
      mom_marital_status == 'Legally Separated' ~ 'Divorced',
      mom_marital_status == 'Divorced' ~ 'Divorced',
      mom_marital_status == 'Patient Refused' ~ NA_character_,
      mom_marital_status == 'Unknown' ~ NA_character_,
      mom_marital_status == 'Widowed' ~ 'Other',
      mom_marital_status == 'Significant Other' ~ 'Other',
      mom_marital_status == 'Married' ~ 'Married',
      mom_marital_status == 'Single' ~ 'Single'
    )
  ) %>%
  mutate(mom_marital_status = as.factor(mom_marital_status)) %>%
  mutate(mom_race = as.factor(mom_race)) %>%
  mutate(mom_ethnicity = as.factor(mom_ethnicity)) %>%
  mutate(preeclampsia = as.factor(preeclampsia)) %>%
  mutate(pregest_dm = as.factor(pregest_dm)) %>%
  mutate(gestational_dm = as.factor(gestational_dm)) %>%
  mutate(placental_abruption = as.factor(placental_abruption)) %>%
  mutate(maternal_dvt = as.factor(maternal_dvt)) %>%
  mutate(baby_birth_wt_gms = as.numeric(baby_birth_wt_gms)) %>%
  mutate(
    delivery_method = case_when(
      delivery_method == 'C-Section' ~ 'C-Section',
      delivery_method == 'C-Section w/ BTL' ~ 'C-Section',
      delivery_method == 'C-Section, Classical' ~ 'C-Section',
      delivery_method == 'C-Section, Hysterectomy' ~ 'C-Section',
      delivery_method == 'C-Section, Low Transverse' ~ 'C-Section',
      delivery_method == 'C-Section, Low Vertical' ~ 'C-Section',
      delivery_method == 'C-Section, Unspecified' ~ 'C-Section',
      delivery_method == 'Elective Abortion' ~ 'Other',
      delivery_method == 'Miscarriage' ~ 'Other',
      delivery_method == 'NST' ~ 'Other',
      delivery_method == 'Other/Procedure' ~ 'Other',
      delivery_method == 'Spontaneous Abortion' ~ 'Other',
      delivery_method == 'TAB' ~ 'Other',
      delivery_method == 'Vaginal, Breech' ~ 'Vaginal',
      delivery_method == 'Vaginal, Breech Extraction' ~ 'Vaginal',
      delivery_method == 'Vaginal, Spontaneous Breech' ~ 'Vaginal',
      delivery_method == 'Vaginal, Spontaneous' ~ 'Vaginal',
      delivery_method == 'VBAC, Spontaneous' ~ 'Vaginal',
      delivery_method == 'Vacuum Vaginal Delivery' ~ 'Operative Vaginal',
      delivery_method == 'Forceps Vaginal Delivery' ~ 'Operative Vaginal',
      delivery_method == 'Vaginal, Forceps' ~ 'Operative Vaginal',
      delivery_method == 'Vaginal, Forceps Delivery' ~ 'Operative Vaginal',
      delivery_method == 'Vaginal, Vacuum (Extractor)' ~ 'Operative Vaginal',
      delivery_method == 'Vaginal, Vacuum Delivery' ~ 'Operative Vaginal',
      delivery_method == 'VBAC, Forceps Delivery' ~ 'Operative Vaginal',
      delivery_method == 'VBAC, Vacuum Delivery' ~ 'Operative Vaginal',
    )
  ) %>%
  mutate(delivery_method = as.factor(delivery_method)) %>%
  filter(delivery_method != "Other") %>% ##exclusion criteria include in flow
  mutate(
    baby_sex = case_when(
      baby_sex == 'Unknown' ~ NA_character_,
      baby_sex == 'Male' ~ 'Male',
      baby_sex == 'Female' ~ 'Female'
    )
  ) %>%
  separate(col = gest_age_wks,
           into = c("gest_age_weeks", "gest_age_days")
           ) %>%
  mutate(gest_age_days = as.numeric(gest_age_days)) %>% 
  mutate(gest_age_days = round((gest_age_days / 7), 2)) %>% 
  mutate(gest_age_days = as.character(gest_age_days)) %>% 
  unite(col = "gest_age_wks",
        gest_age_weeks:gest_age_days,
        na.rm = TRUE,
        remove = FALSE) %>% 
  mutate(gest_age_wks = na_if(gest_age_wks, "")) %>% 
  slice(-c(2009, # weird values
           17777,
           17271,
           10985,
           27298,
           23438,
           5709)) %>%
  mutate(gest_age_wks = gsub('_0', '', gest_age_wks)) %>% 
  select(-gest_age_days,
         -gest_age_weeks) %>% 
  separate(mom_ht_ftin,
           into = c('mom_ht_ft', 'mom_ht_in'),
           sep = "'") %>% 
  mutate(baby_sex = as.factor(baby_sex)) %>%
  mutate(baby_race = as.factor(baby_race)) %>%
  mutate(baby_ethnicity = as.factor(baby_ethnicity)) %>%
  mutate(baby_admit_dttm_shifted = as.Date(baby_admit_dttm_shifted)) %>%
  mutate(baby_disch_dttm_shifted = as.Date(baby_disch_dttm_shifted)) %>%
  mutate(baby_dob_shifted = as.Date(baby_dob_shifted)) %>%
  mutate(baby_dod_shifted = as.Date(baby_dod_shifted)) %>%
  mutate(mom_admit_dttm_shifted = as.Date(mom_admit_dttm_shifted)) %>%
  mutate(mom_disch_dttm_shifted = as.Date(mom_disch_dttm_shifted))

ob_pipe$mom_ht_in <- gsub('\"' , "", ob_pipe$mom_ht_in) ## ax

ob_pipe <- ob_pipe %>%
  mutate(mom_ht_ft = as.numeric(mom_ht_ft)) %>%
  mutate(mom_ht_in = as.numeric(mom_ht_in)) %>% 
  add_column(mom_ht_fttocm = as.numeric(NA)) %>% 
  add_column(mom_ht_intocm = as.numeric(NA)) %>% 
  mutate(mom_ht_fttocm = mom_ht_ft * 30.48) %>% 
  mutate(mom_ht_intocm = mom_ht_in * 2.54)
  
ob_pipe <- ob_pipe %>%   
  add_column(mom_ht_cm = as.numeric(NA)) %>% 
  mutate(mom_ht_cm = round((mom_ht_fttocm + mom_ht_intocm), 2)) %>% 
  select(-mom_ht_fttocm,
         -mom_ht_intocm,
         -mom_ht_ft,
         -mom_ht_in)

ob_final <- ob_pipe %>% 
  group_by(patient_num) %>%
  filter(mom_admit_dttm_shifted == min(mom_admit_dttm_shifted)) %>% 
  ungroup(patient_num) %>% 
  add_count(patient_num) %>% 
  rename(baby_count = n) %>% 
  group_by(patient_num) %>% 
  filter(baby_patient_num == min(baby_patient_num))

save(ob_final, file="ob_final.Rdata")

global_pt_num <- ob_final$patient_num # used for matching patient IDs between dfs

rm(ob); rm(ob_pipe)
gc()

#### pt_demo ####
pt_demo <- dbReadTable(db, "demographics")

pt_demo_final <- pt_demo %>% 
  select(patient_num,
         birth_date_shifted,
         marital_status,
         employment_status,
         race,
         ethnicity) %>% 
  mutate(
    ethnicity = case_when(
      ethnicity == 'Refused' ~ NA_character_,
      ethnicity == 'Hispanic' ~ 'Hispanic',
      ethnicity == 'Unknown' ~ NA_character_,
      ethnicity == 'NI' ~ 'Other',
      ethnicity == 'Non Hispanic' ~ 'NH'
    )
  ) %>%
  mutate(
    race = case_when(
      race == 'Unknown' ~ NA_character_,
      race == 'Asian' ~ 'Other',
      race == 'Patient Refused' ~ NA_character_,
      race == 'Multiracial' ~ 'Other',
      race == 'Native Hawaiian or Other Pacific Islander' ~ 'Other',
      race == 'American Indian or Alaska Native' ~ 'Other',
      race == 'White or Caucasian' ~ 'White or Caucasian',
      race == 'Black or African American' ~ 'Black or African American',
      race == 'Other' ~ 'Other',
    )
  ) %>%
  mutate(
    marital_status = case_when(
      marital_status == 'Legally Separated' ~ 'Divorced',
      marital_status == 'Divorced' ~ 'Divorced',
      marital_status == 'Patient Refused' ~ NA_character_,
      marital_status == 'Unknown' ~ NA_character_,
      marital_status == 'Widowed' ~ 'Other',
      marital_status == 'Significant Other' ~ 'Other',
      marital_status == 'Married' ~ 'Married',
      marital_status == 'Single' ~ 'Single'
    )
  ) %>%
  mutate(
    employment_status = case_when( ## employment_status_new add column + same for other
      employment_status == 'Full Time' ~ 'Full Time',
      employment_status == 'Part Time' ~ 'Part Time',
      employment_status == 'Not Employed' ~ 'Not Employed',
      employment_status == 'Retired' ~ 'Other',
      employment_status == 'Unknown' ~ NA_character_,
      employment_status == 'Disabled' ~ 'Other',
      employment_status == 'Self Employed' ~ 'Other',
      employment_status == 'Student - Part Time' ~ 'Not Employed',
      employment_status == 'Student - Full Time' ~ 'Not Employed',
      employment_status == 'Patient Refused' ~ NA_character_,
      employment_status == 'On Active Military Duty' ~ 'Other'
    )
  ) %>% 
  mutate(birth_date_shifted = as.Date(birth_date_shifted)) %>% 
  mutate(marital_status = as.factor(marital_status)) %>% 
  mutate(employment_status = as.factor(employment_status)) %>% 
  mutate(race = as.factor(race)) %>% 
  mutate(ethnicity = as.factor(ethnicity)) %>% 
  filter(patient_num %in% global_pt_num)

save(pt_demo_final, file="pt_demo_final.Rdata")

rm(pt_demo)
gc()

####


#### vitals ####
vitals <- dbReadTable(db, "vitals")

vitals$measure_date_shifted <- gsub(" .*", "", vitals$measure_date_shifted)

vitals_final <- vitals %>% 
  select(patient_num,
         measure_date_shifted,
         height,
         weight,
         bmi,
         bp_diastolic,
         bp_systolic) %>% 
  filter(patient_num %in% global_pt_num) %>% 
  mutate(measure_date_shifted = as.Date(measure_date_shifted)) %>% 
  group_by(patient_num) %>% 
  add_column(highest_diastolic = NA) %>% 
  add_column(highest_systolic = NA) %>% 
  add_column(post_bmi_date = as.Date(NA)) %>% 
  add_column(pre_bmi_date = as.Date(NA)) %>% 
  add_column(pre_bmi = NA) %>% 
  add_column(post_bmi = NA)

save(vitals_final, file = 'vitals_final.Rdata')

rm(vitals)
gc()

####
#### dx ####
dx <- dbReadTable(db, "diagnosis")

dx$dx_date_shifted <- gsub(" .*", "", dx$dx_date_shifted)

dx_pipe <- dx %>% 
  select(
    patient_num,
    dx_date_shifted,
    dx_code
  ) %>% 
  mutate(dx_date_shifted = as.Date(dx_date_shifted)) %>% 
  add_column(htn_dx_date = as.Date(NA)) %>% 
  add_column(pre_htn = NA) %>% 
  add_column(post_htn = NA) %>% 
  filter(patient_num %in% global_pt_num)

rm(dx)
gc()

save(dx_pipe, file="dx_final.Rdata")
####

#### dx, ob merge ####
dx_ob_merge <- merge(dx_pipe, ob_final, by.x = 'patient_num')

rm(dx_pipe)
gc()

dx_ob_merge$diff_htn <- as.numeric(difftime(dx_ob_merge$dx_date_shifted, dx_ob_merge$baby_dob_shifted, units = 'days'))

dx_ob_merge_pipe <- dx_ob_merge %>% 
  group_by(patient_num) %>% 
  mutate(
    htn_dx_date = if_else(
      condition = any(str_detect(dx_code, "I10")),
      true = min(dx_date_shifted),
      false = as.Date(NA)
    )
  ) %>% 
  mutate(
    pre_htn = if_else(
      condition = ((diff_htn >= 0 & diff_htn <= 365) & is.na(htn_dx_date)), # @ delivery or up to 1 yr after
      true = "yes",
      false = "no"
    )
  ) %>% 
  mutate(
    post_htn = if_else(
      condition = ((diff_htn >= 366 & diff_htn <= 2160) & is.na(htn_dx_date)) , #1 yr +/- 2 months after delivery
      true = "yes",
      false = "no"
    )
  ) %>% 
  mutate(
    pre_htn = if_else(
      condition = any(str_detect(pre_htn, 'yes')), 
      true = "yes",
      false = "no"
    )
  ) %>% 
  mutate(
    post_htn = if_else(
      condition = any(str_detect(post_htn, 'yes')),
      true = "yes",
      false = "no"
    )
  ) %>% 
  mutate(
    dx_code = if_else(
      condition = any(str_detect(dx_code, 'I10')),
      true = 'I10',
      false = as.character(NA)
    )
  ) %>% 
  distinct(patient_num, .keep_all = T) %>% 
  select(-dx_date_shifted)


rm(dx_ob_merge)
gc()

save(dx_ob_merge_pipe, file = 'dx_ob_merge_pipe.Rdata')

####


#### vitals, dx, ob merged ####
vit_dx_ob_merge <- merge(dx_ob_merge_pipe, vitals_final, by.x = 'patient_num')

vit_dx_ob_merge$diff_date <- as.numeric(difftime(vit_dx_ob_merge$measure_date_shifted, vit_dx_ob_merge$baby_dob_shifted, units = 'days'))

vit_dx_ob_merge_pipe <- vit_dx_ob_merge %>% 
  mutate(
    pre_bmi_date = if_else(
      condition = (diff_date >= -730 & diff_date <= 0), #2 yrs before + up to delivery
      true = measure_date_shifted,
      false = as.Date(NA)
    )
  ) %>% 
    mutate(
      post_bmi_date = if_else(
        condition = (diff_date >= 305 & diff_date <= 425), #1 yr +/- 2 months after delivery
        true = measure_date_shifted,
        false = as.Date(NA)
      )
  ) %>% 
  mutate(
    pre_bmi = if_else(
      condition = (!is.na(pre_bmi_date)),
      true = bmi,
      false = as.double(NA)
    )
  ) %>% 
  mutate(
    post_bmi = if_else(
      condition = (!is.na(post_bmi_date)),
      true = bmi,
      false = as.double(NA)
    )
   ) 


final <- vit_dx_ob_merge_pipe %>% 
  group_by(patient_num) %>% 
  mutate(
    highest_diastolic = if_else(
      condition = (diff_date >= -730 & diff_date <= 0), #2 yrs before + up to delivery
      true = bp_diastolic,
      false = as.double(NA)
    )
  ) %>% 
  mutate(
    highest_systolic = if_else(
      condition = (diff_date >= -730 & diff_date <= 0), #2 yrs before + up to delivery
      true = bp_systolic,
      false = as.double(NA)
    )
  ) %>%
  add_column(mean_pre_bmi = NA) %>%
  add_column(mean_post_bmi = NA) %>%
  mutate(mean_pre_bmi = mean(pre_bmi, na.rm = T)) %>%
  mutate(mean_post_bmi = mean(post_bmi, na.rm = T)) %>%
  slice_max(highest_systolic) %>% ## NOTE
  distinct(patient_num, .keep_all = T) #%>% 
  #filter(!is.na(mean_pre_bmi) & !is.na(mean_post_bmi))

final_final <- merge(final, pt_demo_final, by.x = "patient_num")

final_final$age_at_del <- time_length(interval(final_final$birth_date_shifted, final_final$baby_dob_shifted), "years")

final_final_final <- final_final %>% 
  mutate(mom_age_at_del = as.numeric(mom_age_at_del)) %>% 
  mutate(
    mom_age_at_del = if_else(
      condition = !is.na(mom_age_at_del),
      true = mom_age_at_del,
      false = round(as.numeric(age_at_del))
    )
  ) %>% 
  mutate(
    mom_ethnicity = if_else(
      condition = !is.na(mom_ethnicity),
      true = mom_ethnicity,
      false = ethnicity
    )
  )

final_df <- final_final_final %>% 
  select(-age_at_del,
         -race,
         -ethnicity,
         -marital_status
         ) %>% 
  add_column(bmi_return = NA) %>% 
  mutate(
    bmi_return = if_else(
      condition = mean_pre_bmi >= mean_post_bmi + 1, ## 1 unit 
      true = 'yes',
      false = 'no'
    )
  )

save(final_df, file = "final_df.Rdata")

rm(final_final); rm(final_final_final); rm(ob_final); rm(pt_demo_final); rm(vit_dx_ob_merge) ;rm(vit_dx_ob_merge_pipe); rm(vitals_final); rm(final); rm(dx_ob_merge_pipe)
gc()

####



#### final ####

final_df <- final_df %>% 
  slice(-c(2391, #mom_age_at_del = 0
           2391,
           8691,
           17653))

save(final_df, file='final_df.Rdata')

####

# **REMEMBER TO DISCONNECT FROM DATABASE** #
#### db disconnect ####
dbDisconnect(db)
####
#### testing zone ####

# a <- c(1,2,3,4,5)
# b <- c(2,1,3,5,4)
# View(table(a == b))
# 
# sort_a <- sort(a)
# sort_b <- sort(b)
# View(table(sort_a == sort_b))
# 
# View((a %in% b))


# diff_test <- as.numeric(difftime(test_merge$measure_date_shifted, test_merge$baby_dob_shifted, units = 'days'))
# View(table(diff_test >= -60 & diff_test <= 60))
# 
# mutate(
#   post_bmi_date = case_when(
#     post_bmi_date == (((as.numeric(measure_date_shifted - baby_dob_shifted)) == between(0, -60, 60)) == T) ~ measure_date_shifted,
#     post_bmi_date == (((as.numeric(measure_date_shifted - baby_dob_shifted)) == between(0, -60, 60)) == F) ~ NA,
#   ))
# 
# filter((post_bmi_date = measure_date_shifted), ((as.numeric(difftime(measure_date_shifted, baby_dob_shifted, units = 'days')) == between(0, -60, 60))))
# 
# mutate(
#   post_bmi_date = case_when(
#     (as.numeric(difftime(measure_date_shifted, baby_dob_shifted, units = 'days')) == between(0, -60, 60)) == T ~ measure_date_shifted,
#     (as.numeric(difftime(measure_date_shifted, baby_dob_shifted, units = 'days')) == between(0, -60, 60)) == F ~ NA
#   )
# )
# 
# filter((post_bmi_date = measure_date_shifted) %in% ((difftime(measure_date_shifted, ob_final$baby_dob_shifted, units = 'days') == between(0, -60, 60))))
# 
# 
# filter(post_bmi_date == ((difftime(measure_date_shifted, ob_final$baby_dob_shifted, units = 'days') == between(0, -60, 60))))
# 
# mutate(
#   ob_final$post_bmi = case_when(
#     (difftime(measure_date_shifted, ob_final$baby_dob_shifted, units = 'days') == between(0, -60, 60)) == T ~ bmi,
#     (difftime(measure_date_shifted, ob_final$baby_dob_shifted, units = 'days') == between(0, -60, 60)) == F ~ NA
#   )
# )
# 
# for (i in nrow(vitals_final)) {
#   if (difftime(i[vitals_final$measure_date_shifted, i], ob_final$baby_dob_shifted, units = 'days') == between(0, -60, 60)) {
#     post_bmi = bmi
#   }
# }
# 
# mutate(
#   post_bmi = if_else(
#     condition = (ifdifftime(measure_date_shifted, ob_final$baby_dob_shifted, units = 'days') == between(0, -60, 60)),
#     TRUE == bmi,
#     FALSE == NA
#   ))
# 
# post_bmi_date = if_else(
#   condition = (as.numeric(difftime(measure_date_shifted, baby_dob_shifted, units = 'days')) == between(diff_date, -60, 60)), #only doing 60 days + not including -
#   true = measure_date_shifted,
#   false = as.Date(NA)
# )
# )
####





