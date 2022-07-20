# library ------------------------------------------------------

library(tidyverse)

# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))

# Create empty data frame ------------------------------------------------------

df <- data.frame(active = logical(),
                 outcome = character(),
                 outcome_group = character(),
                 outcome_variable = character(),
                 covariates = character(),
                 model = character(),
                 main = character(),
                 covid_pheno_hospitalised = character(),
                 covid_pheno_non_hospitalised = character(),
                 agegp_18_39 = character(),
                 agegp_40_59 = character(),
                 agegp_60_79 = character(),
                 agegp_80_110 = character(),
                 sex_Male = character(),
                 sex_Female = character(),
                 ethnicity_White = character(),
                 ethnicity_Mixed = character(),
                 ethnicity_South_Asian = character(),
                 ethnicity_Black = character(),
                 ethnicity_Other = character(),
                 ethnicity_Missing = character(),
                 prior_history_TRUE = character(),
                 prior_history_FALSE = character(),
                 prior_history_var = character(),
                 venn = character(),
                 stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Add diabetes outcomes --------------------------------------------------------
# ------------------------------------------------------------------------------

outcomes <- c("type 1 diabetes",
              "type 2 diabetes",
              "type 2 diabetes - pre diabetes",
              "type 2 diabetes - no pre diabetes",
              "type 2 diabetes - obesity",
              "type 2 diabetes - no obesity",
              "other or non-specific diabetes",
              "gestational diabetes")

outcome_group <- "diabetes"

outcomes_short <- c("t1dm","t2dm", "t2dm_pd","t2dm_pd_no", "t2dm_obes","t2dm_obes_no", "otherdm","gestationaldm")
outcome_venn <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       outcome_group,
                       paste0("out_date_",outcomes_short[i]),
                       "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                       rep("all",1),
                       rep(FALSE,3),
                       rep(FALSE,14),
                       "",
                       outcome_venn[i])
}

# change outcome group so that gestational diabetes has its own group

df <- df %>% mutate(outcome_group = case_when(outcome_variable == "out_date_gestationaldm" ~ "diabetes_gestational",
                                              TRUE ~ as.character(outcome_group)))

# change outcome group for pre diabetes and obesity analysis

df <- df %>% mutate(outcome_group = case_when(outcome == "type 2 diabetes - pre diabetes" ~ "diabetes_prediabetes",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - no pre diabetes" ~ "diabetes_no_prediabetes",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - obesity" ~ "diabetes_obesity",
                                              TRUE ~ as.character(outcome_group)), 
                    outcome_group = case_when(outcome == "type 2 diabetes - no obesity" ~ "diabetes_no_obesity",
                                              TRUE ~ as.character(outcome_group)))

# turn on subgroups for main t2dm analyses

df[2,c(10:21)] <- FALSE

# Remove sex as a covariate for gestational diabetes analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_gestationaldm" ~ "cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                              TRUE ~ as.character(covariates)))

# remove BMI for obesity subgroup analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_obes" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_obes_no" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# remove pre-diabetes for pre-diabetes subgroup analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_pd" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_pd_no" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# add pre diabetes subgroup analysis

# df$prior_history_var <- ifelse(df$outcome=="type 2 diabetes" ,"cov_bin_prediabetes",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="type 2 diabetes" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="type 2 diabetes" ,TRUE,df$prior_history_FALSE)

# ------------------------------------------------------------------------------
# Add mental health outcomes --------------------------------------------------------
# ------------------------------------------------------------------------------

outcomes <- c("Depression", "Depression - Prescription", "Depression - Primary Care", "Depression - Secondary Care", 
              "Anxiety - general", "Anxiety - general Prescription", "Anxiety - general Primary Care", "Anxiety - general Secondary Care",
              "Anxiety - obsessive compulsive disorder", "Anxiety - post traumatic stress disorder", "Eating disorders", 
              "Serious mental illness", "Serious mental illness - Prescription", "Serious mental illness - Primary Care", "Serious mental illness - Secondary Care",
              "Self harm", "Self harm - Primary Care", "Self harm - Secondary Care",
              "Suicide", "Addiction", "Addiction - Prescription")

outcome_group <- "mental_health"

outcomes_short <- c("depression", "depression_prescription", "depression_primarycare", "depression_secondarycare",
                    "anxiety_general", "anxiety_general_prescription","anxiety_general_primarycare", "anxiety_general_secondarycare",
                    "anxiety_ocd", "anxiety_ptsd", "eating_disorders", 
                    "serious_mental_illness", "serious_mental_illness_prescription", "serious_mental_illness_primarycare", "serious_mental_illness_secondarycare",
                    "self_harm", "self_harm_primarycare", "self_harm_secondarycare",
                    "suicide", "addiction", "addiction_prescription")

out_venn <- c(TRUE, FALSE, FALSE, FALSE, #depression
              TRUE, FALSE, FALSE, FALSE, #anxiety
              TRUE, FALSE, TRUE,       #anxiety -ocd, ptsd, eating disorders
              TRUE, FALSE, FALSE, FALSE, #serious mental illness
              TRUE, FALSE, FALSE, #self harm
              TRUE, #suicide
              TRUE, FALSE) #addiction

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       outcome_group,
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_age;cov_cat_sex;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_num_consulation_rate;cov_bin_healthcare_worker;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_ami;cov_bin_stroke_isch;cov_bin_recent_depression;cov_bin_history_depression;cov_bin_recent_anxiety;cov_bin_history_anxiety;cov_bin_recent_eating_disorders;cov_bin_history_eating_disorders;cov_bin_recent_serious_mental_illness;cov_bin_history_serious_mental_illness;cov_bin_recent_self_harm;cov_bin_history_self_harm",
                       rep("all",1),
                       rep(TRUE,1),
                       rep(FALSE,16),
                       "",
                       out_venn[i])
}

#Main outcomes:
#df[c(1:8,10:12,14:16,21:23,25:26,29), 1] <- FALSE

#Depression - Main outcome + prescriptions + primary care + secondary care
df[c(1:8,13:29),1] <- FALSE
#Anxiety - Main outcome + prescriptions + primary care + secondary care
#df[c(1:12,17:29),1] <- FALSE
#Serious mental illness Main outcome + prescriptions + primary care + secondary care
#df[c(1:19,24:29),1] <- FALSE
#Self harm (included addiction) - Main outcome + prescriptions + primary care + secondary care
#df[c(1:23,27),1] <- FALSE

#Depression, anxiety, serious mental illness, and self harm: subgroup analysis 
# df[c(1:8,10:12,14:19,21:23,25:29),c(1,7:23)] <- FALSE
# #Prior_history variables (table_2 script):
# Depression
# df$prior_history_var <- ifelse(df$outcome=="Depression" ,"sub_bin_depression",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="Depression" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="Depression" ,TRUE,df$prior_history_FALSE)
# 
# # #Anxiety - general
# df$prior_history_var <- ifelse(df$outcome=="Anxiety - general" ,"sub_bin_anxiety_general",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="Anxiety - general" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="Anxiety - general" ,TRUE,df$prior_history_FALSE)
# #
# # #Serious mental illness
# df$prior_history_var <- ifelse(df$outcome=="Serious mental illness" ,"sub_bin_serious_mental_illness",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="Serious mental illness" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="Serious mental illness" ,TRUE,df$prior_history_FALSE)
# #
# # #Self harm
# df$prior_history_var <- ifelse(df$outcome=="Self harm" ,"sub_bin_self_harm",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="Self harm" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="Self harm" ,TRUE,df$prior_history_FALSE)

#Main outcomes + prescriptions + primary care + secondary care
#df[c(1:8,17:19,27),1] <- FALSE

# df[6,1] <- TRUE

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "lib/active_analyses.rds")
