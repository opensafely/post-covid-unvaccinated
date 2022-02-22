##################################################################################
# 
# Description: This script reads in the input data prepares it for data cleaning.
#
# Input: output/input.feather
# Output: output/
#
# Author(s): Rachel Denholm (??),  Kurt Taylor
#
# Date last updated: 
#
##################################################################################

# Load libraries ---------------------------------------------------------------

library(magrittr)
library(tidyverse)

# Define parameters ------------------------------------------------------------

## Study start date
study_start <- "2020-01-01"

## Load dataset
df <- arrow::read_feather(file = "output/input.feather")

# Describe data --------------------------------------------------------------

sink(paste0("output/describe_input_studydefinition.txt"))
print(Hmisc::describe(df))
sink()

# Format columns -----------------------------------------------------
# dates, numerics, factors, logicals

df <- df %>%
  rename(tmp_cov_max_hba1c_mmol_mol_date = tmp_cov_num_max_hba1c_mmol_mol_date) %>%
  mutate(across(contains('_date'), ~ as.Date(as.character(.)))) %>%
  mutate(across(contains('_birth_year'), ~ format(as.Date(.), "%Y"))) %>%
  mutate(across(contains('_num'), ~ as.numeric(.))) %>%
  mutate(across(contains('_cat'), ~ as.factor(.))) %>%
  mutate(across(contains('_bin'), ~ as.logical(.)))

# Define COVID-19 severity --------------------------------------------------------------

df <- df %>%
  mutate(sub_cat_covid19_hospital = 
           ifelse(!is.na(exp_date_covid19_confirmed) &
                    !is.na(sub_date_covid19_hospital) &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed >= 0 &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed < 29, "hospitalised",
                  ifelse(!is.na(exp_date_covid19_confirmed), "non_hospitalised", 
                         ifelse(is.na(exp_date_covid19_confirmed), "no_infection", NA))))


df$sub_cat_covid19_hospital <- "no_infection"
  
df$sub_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed),
                                      "non_hospitalised",df$sub_cat_covid19_hospital)

df$sub_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed) & 
                                      !is.na(df$sub_date_covid19_hospital) &
                                      (df$sub_date_covid19_hospital-df$exp_date_covid19_confirmed>=0 &
                                      df$sub_date_covid19_hospital-df$exp_date_covid19_confirmed<29),
                                      "hospitalised",df$sub_cat_covid19_hospital)
  
df$sub_cat_covid19_hospital <- as.factor(df$sub_cat_covid19_hospital)
df[,c("sub_date_covid19_hospital")] <- NULL

# Define diabetes outcome (using Sophie Eastwood algorithm) ----------------------------
diabetes <- tibble(patient_id = df$patient_id,
                   out_date_gestationaldm = df$out_date_gestationaldm,
                   out_date_otherdm = df$out_date_otherdm, 
                   tmp_out_date_t1dm_snomed = df$tmp_out_date_t1dm_snomed, 
                   tmp_out_date_t1dm_hes = df$tmp_out_date_t1dm_hes, 
                   tmp_out_date_t2dm_snomed = df$tmp_out_date_t2dm_snomed, 
                   tmp_out_date_t2dm_hes = df$tmp_out_date_t2dm_hes, 
                   out_date_poccdm = df$out_date_poccdm,
                   tmp_out_count_poccdm_snomed = df$tmp_out_count_poccdm_snomed,
                   cov_cat_ethnicity = df$cov_cat_ethnicity,
                   tmp_cov_date_nonmetform_drugs_snomed = df$tmp_cov_date_nonmetform_drugs_snomed,
                   tmp_out_count_t1dm_hes = df$tmp_out_count_t1dm_hes,
                   tmp_out_count_t2dm_hes = df$tmp_out_count_t2dm_hes,
                   tmp_out_count_t1dm_snomed = df$tmp_out_count_t1dm_snomed,
                   tmp_out_count_t2dm_snomed = df$tmp_out_count_t2dm_snomed,
                   tmp_cov_date_diabetes_medication = df$tmp_cov_date_diabetes_medication,
                   tmp_cov_num_max_hba1c_mmol_mol = df$tmp_cov_num_max_hba1c_mmol_mol,
                   tmp_cov_num_max_hba1c_mmol_mol_date = df$tmp_cov_num_max_hba1c_mmol_mol_date)

diabetes$out_date_t1dm <- pmin(diabetes$tmp_out_date_t1dm_snomed, 
                               diabetes$tmp_out_date_t1dm_hes, na.rm = TRUE)
diabetes$out_date_t2dm <- pmin(diabetes$tmp_out_date_t2dm_snomed, 
                               diabetes$tmp_out_date_t2dm_hes, na.rm = TRUE)

diabetes$tmp_out_count_t1dm <- sum(diabetes$tmp_out_count_t1dm_hes,diabetes$tmp_out_count_t1dm_snomed, na.rm=TRUE)
diabetes$tmp_out_count_t2dm <- sum(diabetes$tmp_out_count_t2dm_hes,diabetes$tmp_out_count_t2dm_snomed, na.rm=TRUE)

diabetes$first_diab <- pmin(diabetes$out_date_gestationaldm, 
                                 diabetes$out_date_otherdm, 
                                 diabetes$out_date_t1dm, 
                                 diabetes$out_date_t2dm, 
                                 diabetes$out_date_poccdm, na.rm = TRUE)

diabetes$year_first_diab <- format(diabetes$first_diab,"%Y")
diabetes$year_first_diab <- as.integer(diabetes$year_first_diab)
diabetes$age_1st_diag <- diabetes$year_first_diab - df$qa_num_birth_year

diabetes$age_under_35_30_1st_diag <-"No"
diabetes$age_under_35_30_1st_diag <- ifelse(!is.na(diabetes$age_1st_diag) &
                                    (diabetes$age_1st_diag<35 & 
                                    (diabetes$cov_cat_ethnicity==1|diabetes$cov_cat_ethnicity==5)) | 
                                    (diabetes$age_1st_diag<30 & 
                                    (diabetes$cov_cat_ethnicity==2|diabetes$cov_cat_ethnicity==3|diabetes$cov_cat_ethnicity==4)),
                                    "Yes",diabetes$age_under_35_30_1st_diag)
  
diabetes$over5_pocc <- "No"
diabetes$over5_pocc <- ifelse(!is.na(df$tmp_out_count_poccdm_snomed) &
                      (df$tmp_out_count_poccdm_snomed>5),
                      "Yes",diabetes$over5_pocc)

# Diabetes adjudication algorithm
diabetes$step_1 <- "No"
diabetes$step_1 <- ifelse(!is.na(diabetes$out_date_gestationaldm), "Yes", diabetes$step_1) # Step 1. Any gestational diabetes code?

diabetes$step_1a <- "NA"                                       
diabetes$step_1a <- ifelse(diabetes$step_1=="Yes" &                 # Step 1a. Any T1/ T2 diagnostic codes present?
                           is.na(diabetes$out_date_t1dm) &          # denominator for step 1a is those with yes to step 1
                           is.na(diabetes$out_date_t2dm),
                           "No", diabetes$step_1a)
diabetes$step_1a <- ifelse(diabetes$step_1=="Yes" &
                           (!is.na(diabetes$out_date_t1dm)|
                           !is.na(diabetes$out_date_t2dm)),
                           "Yes", diabetes$step_1a)  

diabetes$step_2 <- "NA"
diabetes$step_2 <- ifelse((diabetes$step_1=="No" | diabetes$step_1a=="Yes" ) &  # Step 2. Non-metformin antidiabetic?
                          !is.na(df$tmp_cov_date_nonmetform_drugs_snomed),       # denominator for step 2: no to step 1 or yes to step 1a
                          "Yes", diabetes$step_2) 
diabetes$step_2 <- ifelse((diabetes$step_1=="No" | diabetes$step_1a=="Yes") & 
                          is.na(df$tmp_cov_date_nonmetform_drugs_snomed),
                          "No", diabetes$step_2)   

diabetes$step_3 <- "NA"
diabetes$step_3 <- ifelse(diabetes$step_2=="No" &                   # Step 3. Type 1 code in the absence of type 2 code?
                          !is.na(diabetes$out_date_t1dm) &           # denominator for step 3: no to step 2
                           is.na(diabetes$out_date_t2dm),
                           "Yes", diabetes$step_3)
diabetes$step_3 <- ifelse(diabetes$step_2=="No" &                            
                          !is.na(diabetes$out_date_t1dm) &       
                          !is.na(diabetes$out_date_t2dm),
                          "No", diabetes$step_3)

diabetes$step_4 <- "NA"
diabetes$step_4 <- ifelse(diabetes$step_3=="No" &                   # Step 4. Type 2 code in the absence of type 1 code
                            is.na(diabetes$out_date_t1dm) &           # denominator for step 3: no to step 3
                            !is.na(diabetes$out_date_t2dm),
                          "Yes", diabetes$step_4)
diabetes$step_4 <- ifelse(diabetes$step_3=="No" &                            
                            !is.na(diabetes$out_date_t1dm) &       
                            !is.na(diabetes$out_date_t2dm),
                          "No", diabetes$step_4)

diabetes$step_5 <- "NA"
diabetes$step_5 <- ifelse(diabetes$step_4=="No" &                            # Aged <35yrs (or <30 yrs for SAs and AFCS) at first diagnostic code?
                          diabetes$age_under_35_30_1st_diag=="Yes",        # denominator for step 5: no to step 4
                          "Yes", diabetes$step_5)  
diabetes$step_5 <- ifelse(diabetes$step_4=="No" &                            
                          diabetes$age_under_35_30_1st_diag=="No",        
                          "No", diabetes$step_5)      

diabetes$step_6 <- "NA"
diabetes$step_6 <- ifelse(diabetes$step_5=="No" &                            # Type 1 and type 2 codes present?
                         !is.na(diabetes$out_date_t1dm) &                   # denominator for step 6: no to step 5
                         !is.na(diabetes$out_date_t2dm),
                         "Yes", diabetes$step_6)
diabetes$step_6 <- ifelse(diabetes$step_5=="No" &                           
                          is.na(diabetes$out_date_t1dm) |                  
                          is.na(diabetes$out_date_t2dm),
                          "No", diabetes$step_6)

diabetes$step_6a <- "NA"
diabetes$step_6a <- ifelse(diabetes$step_6=="Yes" &                             # Type 1 only reported in primary care
                          !is.na(diabetes$tmp_out_date_t1dm_snomed) &        # denominator for step 6: no to step 6
                           is.na(diabetes$tmp_out_date_t2dm_snomed),
                          "Yes", diabetes$step_6a)
diabetes$step_6a <- ifelse(diabetes$step_6=="Yes" &                             
                           !is.na(diabetes$tmp_out_date_t1dm_snomed) &       
                           !is.na(diabetes$tmp_out_date_t2dm_snomed),
                           "No", diabetes$step_6a)

diabetes$step_6b <- "NA"
diabetes$step_6b <- ifelse(diabetes$step_6a=="No" &                             # Type 2 only reported in primary care
                             is.na(diabetes$tmp_out_date_t1dm_snomed) &        # denominator for step 6: no to step 6
                             !is.na(diabetes$tmp_out_date_t2dm_snomed),
                           "Yes", diabetes$step_6b)
diabetes$step_6b <- ifelse(diabetes$step_6a=="No" &                             
                             !is.na(diabetes$tmp_out_date_t1dm_snomed) &       
                             !is.na(diabetes$tmp_out_date_t2dm_snomed),
                           "No", diabetes$step_6b)

diabetes$step_6c <- "NA"
diabetes$step_6c <- ifelse(diabetes$step_6b=="No" &                         # Number of type 1 codes>number of type 2 codes?
                           diabetes$tmp_out_count_t1dm>diabetes$tmp_out_count_t2dm,           # denominator for step 6c: no to step 6b
                           "Yes", diabetes$step_6c) 
diabetes$step_6c <- ifelse(diabetes$step_6b=="No" &                         
                           diabetes$tmp_out_count_t1dm<diabetes$tmp_out_count_t2dm,        
                           "No", diabetes$step_6c) 

diabetes$step_6d <- "NA"
diabetes$step_6d <- ifelse(diabetes$step_6c=="No" &                         # Number of type 2 codes>number of type 1 codes
                           diabetes$tmp_out_count_t2dm>diabetes$tmp_out_count_t1dm,           # denominator for step 6d: no to step 6c
                           "Yes", diabetes$step_6d) 
diabetes$step_6d <- ifelse(diabetes$step_6c=="No" &                         
                           diabetes$tmp_out_count_t2dm<diabetes$tmp_out_count_t1dm,        
                           "No", diabetes$step_6d) 

diabetes$step_6e <- "NA"
diabetes$step_6e <- ifelse(diabetes$step_6d=="No" &                         # Type 2 code most recent?
                             diabetes$out_date_t2dm>diabetes$out_date_t1dm,           # denominator for step 6e: no to step 6d
                           "Yes", diabetes$step_6e) 
diabetes$step_6e <- ifelse(diabetes$step_6d=="No" &                         
                             diabetes$out_date_t2dm<diabetes$out_date_t1dm,        
                           "No", diabetes$step_6e) 

diabetes$step_7 <- "NA"
diabetes$step_7 <- ifelse(diabetes$step_6=="No" &                                    # Diabetes medication or >5 process of care codes or HbA1c>=6.5?
                        ((!is.na(diabetes$tmp_cov_date_diabetes_medication)) |      # denominator for step 7: no to step 6
                        (diabetes$tmp_cov_num_max_hba1c_mmol_mol>=6.5) |
                        (diabetes$tmp_out_count_poccdm_snomed>=5)),
                        "Yes", diabetes$step_7)
diabetes$step_7 <- ifelse(diabetes$step_6=="No" &                                    
                         ((is.na(diabetes$tmp_cov_date_diabetes_medication)) &     
                         (diabetes$tmp_cov_num_max_hba1c_mmol_mol<6.5) &
                         (diabetes$tmp_out_count_poccdm_snomed<5)),
                         "No", diabetes$step_7)    

# Create data table for diabetes algorithm table
check_missing <- data.frame(variable = character(), N_missing = character(), Perc_missing = character())
#check_missing[nrow(check_missing)+1,] <- c(" N",N,"")
covariate_names <- tidyselect::vars_select(names(input), starts_with(c('sub_','cov_','qa_','vax_cat','exp_cat'), ignore.case = TRUE))
for (i in covariate_names){
  check_missing[nrow(check_missing)+1,1] <- i
  check_missing[nrow(check_missing),2] <- nrow(input[is.na(input[,i]),])
  check_missing[nrow(check_missing),3] <- 100*(nrow(input[is.na(input[,i]),])/N)
}

write.csv(table1, file = file.path("output", paste0("Table1_",cohort_name, ".csv")) , row.names=F)

df$out_cat_diabetes <- "no diabetes"
df$out_cat_diabetes <- ifelse(diabetes$step_1a=="No", "GDM", df$out_cat_diabetes)
df$out_cat_diabetes <- ifelse(diabetes$step_3=="Yes" | 
                              diabetes$step_5=="Yes" |
                              diabetes$step_6a=="Yes" |
                              diabetes$step_6c=="Yes" |
                              diabetes$step_6e=="No",
                              "T1DM", df$out_cat_diabetes)
df$out_cat_diabetes <- ifelse(diabetes$step_2=="Yes" |
                              diabetes$step_4=="Yes" |
                              diabetes$step_6b=="Yes" |
                              diabetes$step_6d=="Yes" |
                              diabetes$step_6e=="Yes",
                              "T2DM", df$out_cat_diabetes)
df$out_cat_diabetes <- ifelse(diabetes$step_7=="Yes",
                              "DM unspecified", df$out_cat_diabetes)
df$out_cat_diabetes <- ifelse(diabetes$step_7=="No",
                              "DM unlikely", df$out_cat_diabetes)

# Remove JCVI age variables ----------------------------------------------------
# NB: These are used to determine JCVI category only

df[,c("vax_jcvi_age_1","vax_jcvi_age_2")] <- NULL
  
# Restrict columns and save analysis dataset ---------------------------------
  
df1 <- df[,c("patient_id","death_date",
                 colnames(df)[grepl("sub_",colnames(df))], # Subgroups
                 colnames(df)[grepl("exp_",colnames(df))], # Exposures
                 colnames(df)[grepl("out_",colnames(df))], # Outcomes
                 colnames(df)[grepl("cov_",colnames(df))], # Covariates
                 colnames(df)[grepl("qa_",colnames(df))], # Quality assurance
                 colnames(df)[grepl("vax_",colnames(df))])] # Vaccination
  
df1[,colnames(df)[grepl("df_out_",colnames(df))]] <- NULL
  
saveRDS(df1, file = paste0("output/input.rds"))
  
# Restrict columns and save Venn diagram input dataset -----------------------
  
df2 <- df[,c("patient_id",colnames(df)[grepl("out_",colnames(df))])]
  
saveRDS(df2, file = paste0("output/venn.rds"))
  
