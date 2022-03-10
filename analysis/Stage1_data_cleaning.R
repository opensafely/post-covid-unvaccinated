## =============================================================================
## Project:     Post covid unvaccinated project
##
##
## Purpose:  Apply stage 1. Data cleaning
##  - Prepare variables
##  - Apply QA rules
##  - Apply inclusion exclusion criteria
##  - Create cleaned dataset
## 
## Authors: Yinghui Wei, Renin Toms, Rochelle Knight, Genevieve Cezard, Rachel Denholm, Kurt Taylor
## Reviewer: 
## 
##
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Prepare all variables (re-factoring, re-typing)
## 2. Apply QA rules
## 3. Apply exclusion/inclusion criteria
##    (Differentiate criteria for the two sub-cohorts)
##    3.a. Define index start date and general end date
##    3.b. Apply the criteria
##    3.d. Create csv file 
## 4. Create the final stage 1 dataset 
## 
## NOTE: This code output are 3 .csv files and 1 R dataset
##       Output files have a specific name to reflect either the Vaccinated 
##       or Electively unvaccinated cohort
##
## =============================================================================

###############################################
# 0. Load relevant libraries and read in data #
###############################################

library(readr)
library(dplyr)
library(stringr)
library(tidyverse)

# Input dataset
input <-read_rds("output/input.rds")

# Define general start date and end date
start_date = as.Date("2020-01-01")
end_date = as.Date("2021-06-18") # General End date: 2021-06-18 (date last JCVI group eligible for vaccination - Decision on Jan 18th 2022)

input <- input %>%
  mutate(index_date = as.Date(start_date),
         end_date = as.Date(end_date))

# NOTE: no censoring of end date for death/event at this stage

######################################################
# 1. Prepare all variables (re-factoring, re-typing) # 
######################################################

# Get the names of variables which are factors
factor_names <- tidyselect::vars_select(names(input), contains(c('_cat_'), ignore.case = TRUE))
  
input <- input %>%
  # handle missing values 
  mutate(cov_cat_smoking_status = replace_na(cov_cat_smoking_status, "M"),
  # Replace " " with "_"     
         cov_cat_region = gsub(" ", "_", cov_cat_region)) %>% 
  # Set the variables that should be factor variables as factor
  mutate(across(any_of(factor_names), function(x) factor(x, ordered = FALSE))) %>%
  # Re-code vars and specify references
  mutate(sub_cat_covid19_hospital = ordered(sub_cat_covid19_hospital, levels = c("non_hospitalised","hospitalised","no_infection")),
         # ethnicity
         cov_cat_ethnicity = case_when(cov_cat_ethnicity == 0 ~ "Missing",
                                       cov_cat_ethnicity == 1 ~ "White",
                                       cov_cat_ethnicity == 2 ~ "Mixed",
                                       cov_cat_ethnicity == 3 ~ "South Asian",
                                       cov_cat_ethnicity == 4 ~ "Black",
                                       cov_cat_ethnicity == 5 ~ "Other")) %>%
  mutate(cov_cat_ethnicity = ordered(cov_cat_ethnicity, levels = c("White","Mixed","South Asian","Black","Other","Missing")),
         # smoking 
         cov_cat_smoking_status = case_when(cov_cat_smoking_status == "E" ~ "Ever smoker",
                                            cov_cat_smoking_status == "M" ~ "Missing",
                                            cov_cat_smoking_status == "N" ~ "Never smoker",
                                            cov_cat_smoking_status == "S" ~ "Current smoker")) %>%
  mutate(cov_cat_smoking_status = ordered(cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing")),
         # region
         cov_cat_region = relevel(cov_cat_region, ref = "London"),
         # sex
         cov_cat_sex = case_when(cov_cat_sex == "F" ~ "Female",
                                 cov_cat_sex == "M" ~ "Male")) %>%
  mutate(cov_cat_sex = relevel(factor(cov_cat_sex), ref = "Female"),
         # cat jcvi group
         vax_cat_jcvi_group = ordered(vax_cat_jcvi_group, levels = c("12","11","10","09","08","07","06","05","04","03","02","01","99"))) %>%
         # deprivation: First - most deprived; fifth -least deprived
  mutate(cov_cat_deprivation = ifelse(cov_cat_deprivation == 1 | cov_cat_deprivation == 2, "1-2 (most deprived)",
                                          ifelse(cov_cat_deprivation == 3 | cov_cat_deprivation == 4, "3-4",
                                                 ifelse(cov_cat_deprivation == 5 | cov_cat_deprivation == 6, "5-6",
                                                        ifelse(cov_cat_deprivation == 7 | cov_cat_deprivation == 8, "7-8",
                                                               ifelse(cov_cat_deprivation == 9 | cov_cat_deprivation == 10, "9-10 (least deprived)", NA)))))) %>%
         mutate_at(vars(cov_cat_deprivation), as.factor) %>%
  mutate(cov_cat_deprivation = ordered(cov_cat_deprivation, levels = c("1-2 (most deprived)","3-4","5-6","7-8","9-10 (least deprived)")))
  
# Save meta data

describe_vars <- tidyselect::vars_select(names(input), contains(c('_cat_', 'cov_bin','cov_cat','qa_bin','exp_cat','vax_cat'), ignore.case = TRUE))
meta_data_factors <- lapply(input[,describe_vars], table)
sink(file = file.path("output", paste0("meta_data_factors2.csv")))
print(meta_data_factors)
sink()

#####################
# 2. Apply QA rules #
#####################

input <- input %>%
  # Rule 1: Year of birth is after year of death or patient only has year of death
  mutate(rule1 = ifelse((qa_num_birth_year > (format(death_date, format="%Y")) &
                             ! is.na(qa_num_birth_year) & 
                             ! is.na(death_date) | is.na(qa_num_birth_year) &
                             ! is.na(death_date)), TRUE, FALSE),
  # Rule 2: Year of birth predates NHS established year or year of birth exceeds current date       
         rule2 = ifelse((qa_num_birth_year < 1793 | (qa_num_birth_year > format(Sys.Date(),"%Y"))) & ! is.na(qa_num_birth_year), TRUE, FALSE),
  # Rule 3: Date of death is NULL or invalid (on or before 1/1/1900 or after current date)     
         rule3 = ifelse((death_date <= "1900-01-01" | death_date > format(Sys.Date(),"%Y-%m-%d")) & ! is.na(death_date), TRUE, FALSE),
  # Rule 4: Pregnancy/birth codes for men
         rule4 = ifelse(qa_bin_pregnancy == TRUE & cov_cat_sex=="Male", TRUE, FALSE),
  # Rule 5: HRT or COCP meds for men
         rule5 = ifelse(cov_cat_sex=="Male" & cov_bin_hormone_replacement_therapy==TRUE | cov_cat_sex=="Male" & cov_bin_combined_oral_contraceptive_pill == TRUE, TRUE, FALSE),
  # Rule 6: Prostate cancer codes for women
         rule6 = ifelse(qa_bin_prostate_cancer == TRUE & cov_cat_sex=="Female", TRUE, FALSE))
  # Rule 7: Check index_date (from new 2022 datasets)
input$rule7=NA
input$rule7= (is.na(start_date)==TRUE)

# Remove rows that are TRUE for at least one rule

input_QA <- input %>% filter(rule1 == FALSE & rule2 == FALSE & rule3 == FALSE & rule4 == FALSE & rule5 == FALSE & rule6 == FALSE & rule7 == FALSE) 

# Produce QA summary

QA_summary <- data.frame(matrix(ncol = 2))
colnames(QA_summary) <- c('Rule', 'N rule TRUE')
QA_summary[1:8, 1] <- c("Rule 1", "Rule 2", "Rule 3", "Rule 4", "Rule 5", "Rule 6", "Rule 7", "Total excluded from QA")
QA_summary[1,2]=nrow(input%>%filter(rule1==T))
QA_summary[2,2]=nrow(input%>%filter(rule2==T))
QA_summary[3,2]=nrow(input%>%filter(rule3==T))
QA_summary[4,2]=nrow(input%>%filter(rule4==T))
QA_summary[5,2]=nrow(input%>%filter(rule5==T))
QA_summary[6,2]=nrow(input%>%filter(rule6==T))
QA_summary[7,2]=nrow(input%>%filter(rule7==T))
QA_summary[8,2]=nrow(input)-nrow(input_QA)

#Save QA summary as .csv

write.csv(QA_summary, file = file.path("output", paste0("QA_summary_.csv")) , row.names=F)

# Remove QA variables from dataset

input <- input_QA %>%
  select(-c(rule1,rule2,rule3,rule4,rule5,rule6,rule7,
          qa_num_birth_year, qa_bin_pregnancy, qa_bin_prostate_cancer))

#########################################
# 3. Apply exclusion/inclusion criteria #
#########################################

# Define the cohort flow

cohort_flow <- data.frame(N = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Study defined sample size")

#----------------------------------------------------------------#
# 3.a. Apply the 6 common criteria applicable to both sub-cohort #
#----------------------------------------------------------------#

# Inclusion criteria 1: Alive on the first day of follow up

input <- input %>%
  mutate(start_alive = ifelse(death_date < index_date, 0, 1)) %>%
  mutate(start_alive = replace_na(start_alive, 1)) %>%
  filter(start_alive == 1)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 1 (Inclusion): Alive on the first day of follow up") # Feed into the cohort flow

# Inclusion criteria 2: Known age between 18 and 110 on 01/01/2020 

input <- input %>% 
  filter(cov_num_age >= 18 & cov_num_age <= 110)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 2 (Inclusion): Known age between 18 and 110 on 01/01/2020") # Feed into the cohort flow

#Inclusion criteria 3: Known sex
input <- input[!is.na(input$cov_cat_sex),] # removes NAs, if any
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 3 (Inclusion): Known sex")

#Inclusion criteria 4: Known deprivation 
input <- input[!is.na(input$cov_cat_deprivation),] # removes NAs, if any
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 4 (Inclusion): Known deprivation")

#Inclusion criteria 5: Registered in an English GP with TPP software for at least 6 months prior to the study start date
# NOTE: Dealt with in Study definition
#input <- input # This criteria is met in study definition 
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 5 (Inclusion): Registered in an English GP with TPP software for at least 6 months prior to the study start date")

#Exclusion criteria 6: SARS-CoV-2 infection recorded prior to the start of follow-up
# Removed for now as we need those with covid history for a sensitivity analysis
#input$prior_infections <- ifelse(input$exp_date_covid19_confirmed < input$index_date, 1,0)# Determine infections prior to start date : 1-prior infection; 0 - No prior infection
#input$prior_infections[is.na(input$prior_infections)] <- 0
#input <- subset(input, input$prior_infections ==0)
#cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 6 (Exclusion): SARS-CoV-2 infection recorded prior index date")

#-------------------------------------------------#
# 3.c. Apply criteria specific to each sub-cohort #
#-------------------------------------------------#

if (cohort_name == "vaccinated") {

  #Exclusion criteria 7: Do not have a record of two vaccination doses prior to the study end date
  input$vacc_gap <- input$vax_date_covid_2 - input$vax_date_covid_1 #Determine the vaccination gap in days : gap is NA if any vaccine date is missing
  input <- input[!is.na(input$vacc_gap),] # Subset the fully vaccinated group
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 7 (Exclusion): No record of two vaccination doses prior to the study end date") # Feed into the cohort flow
  
  #Exclusion criteria 8: Received a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)
  input <- subset(input, input$vax_date_covid_1 >= as.Date("2020-12-08")|input$vax_date_covid_2 >= as.Date("2020-12-08"))
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 8 (Exclusion): Recorded vaccination prior to the start date of vaccination program")
  
  #Exclusion criteria 9: Received a second dose vaccination before their first dose vaccination
  input <- subset(input, input$vacc_gap >= 0) # Keep those with positive vaccination gap
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 9 (Exclusion): Second dose vaccination recorded before the first dose vaccination")
  
  #Exclusion criteria 10: Received a second dose vaccination less than three weeks after their first dose
  input <- subset(input, input$vacc_gap >= 21) # Keep those with at least 3 weeks vaccination gap
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 10 (Exclusion): Second dose vaccination recorded less than three weeks after the first dose")
  
  #Exclusion criteria 11: Mixed vaccine products received before 07-05-2021
  #Determines mixed vaccination before 7/5/2021
  input$vax_mixed <- ifelse((input$vax_cat_product_1!=input$vax_cat_product_2 & (is.na(input$vax_date_covid_2)==FALSE & input$vax_date_covid_2 < as.Date ("2021-05-07")) ),1,0)
  #Determines unknown vaccine product before 7/5/2021
  input$vax_prior_unknown <- ifelse(is.na(input$vax_cat_product_1) | is.na(input$vax_cat_product_2), 1,0)# unknown products
  input$vax_prior_unknown <- ifelse(is.na(input$vax_date_covid_2), 1,input$vax_prior_unknown) #unknown vaccination 2 date
  input$vax_prior_unknown <- ifelse(input$vax_prior_unknown==1 & input$vax_date_covid_2 < as.Date ("2021-05-07"),1,0)#Remove if vaccination products are mixed or not known, prior to "2021-05-07"
  input <- subset(input, input$vax_mixed==0 | input$vax_prior_unknown==0)
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 11 (Exclusion): Received mixed vaccine products before 07-05-2021")

    
} else if (cohort_name == "electively_unvaccinated"){
  
  #Exclusion criteria 7: Have a record of one or more vaccination doses on the study start date
  #a.Determine the vaccination status on index start date
  input$prior_vacc1 <- ifelse(input$vax_date_covid_1 <= input$index_start_date, 1,0)
  input$prior_vacc1[is.na(input$prior_vacc1)] <- 0
  input$prior_vacc2 <- ifelse(input$vax_date_covid_2 <= input$index_start_date, 1,0)
  input$prior_vacc2[is.na(input$prior_vacc2)] <- 0
  input$prior_vacc3 <- ifelse(input$vax_date_covid_3 <= input$index_start_date, 1,0)
  input$prior_vacc3[is.na(input$prior_vacc3)] <- 0
  input$prior_vacc <- ifelse((input$prior_vacc1==1 | input$prior_vacc2==1 |input$prior_vacc3==1), 1,0)
  #Note NAs don't have any vaccination date, hence move to '0' or unvaccinated category
  input$prior_vacc[is.na(input$prior_vacc)] <- 0
  input <- subset(input, input$prior_vacc == 0) #Exclude people with prior vaccination
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 7 (Exclusion): Have a record of one or more vaccination doses on the study start date")
  
  #Exclusion criteria 8: Missing JCVI group
  input <- subset(input, is.na(input$vax_cat_jcvi_group)== FALSE)
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 8 (Exclusion): Exclude missing JCVI group")
}

#----------------------#
# 3.d. Create csv file #
#----------------------#
write.csv(cohort_flow, file = file.path("output", paste0("Cohort_flow_",cohort_name, ".csv")) , row.names=F)

#-------------------------------------#
# 4. Create the final stage 1 dataset #
#-------------------------------------#
saveRDS(input, file = file.path("output", paste0("dataset_stage1_",cohort_name, ".rds")))
