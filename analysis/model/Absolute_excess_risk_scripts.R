## =============================================================================
## Project:     Post covid unvaccinated project
##
## Purpose:  
##  Apply stage 5. Absolute excess risk analysis
##  - Create a function to calculate absolute excess risks
## 
## Authors: Lucy Teece (adapted from RT, XJ, VW)
## Reviewer: Genevieve Cezard
## 
## TO RUN OUTSIDE OPENSAFELY PLATFORM
## Content: 
## 1. Load in input data and make file names and variable structure compatible
## 2. Call AER function and load relevant libraries
## 3. AER calculation for active analyses
## 4. Load results
## 5. Run AER function
## 6. Compile results
## =============================================================================

#---------------------------------------------------------------------------------
# Step 1: Load in input data and make file names and variable structure compatible
#---------------------------------------------------------------------------------
fs::dir_create(here::here("output", "review", "model"))
fs::dir_create(here::here("output", "not-for-review", "AER_results"))
scripts_dir <- "analysis/model"
hr_dir <- "output/review/model"
table_2_dir <- "output/review/descriptives"
aer_raw_results_dir <- "output/not-for-review/AER_results"
aer_results_dir <- "output/review/AER_results"

#------------------------------------------------------
# Step 2: Call AER function and load relevant libraries
#------------------------------------------------------
source(file.path(scripts_dir,"Absolute_excess_risk_function.R"))
library(purrr)
library(data.table)
library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  cohort_name <- ""
}else{
  cohort_name <- args[[1]]
}

agebreaks <- c(0, 40, 60, 80, 111)
agelabels <- c("18_39", "40_59", "60_79", "80_110")

#-------------------------------
#Step 3: AER for active analyses
#-------------------------------
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             # selects active analyses
#Turn on for t1dm so can work on AER scripts
active$active <- ifelse(active$outcome_variable=="out_date_t1dm",TRUE,active$active)
#Turn main analysis on for all so can calculate total AER without subgroups
active$main <- "TRUE"

active <- active[active$active==TRUE,]   

#Preprocess the active analyses
active$event <- gsub("out_date_","",active$outcome_variable)                                     # refine event name                                                    
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL       # removes un used columns
active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")                                               # converts to long data                                        
active <- active[active$value==TRUE, c("event","model","strata")]                       # refines to active models                         
active <- subset(active,strata=="main")                                                #AER only for main analysis 
active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)                # includes 2 model types     
active <- tidyr::separate_rows(active, model, sep = ";")                                         # separate rows for each model
active <- subset(active,model=="mdl_max_adj")                                   #Only require AER for full models
# active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort) # includes 2 cohorts
# active <- tidyr::separate_rows(active, cohort, sep = ";")                                        # separate rows for each cohort

colnames(active) <- c("event","model","subgroup")
active <- active %>% select(-model, everything())                                                  #Order the columns 


#----------------------
#Step 4: Load results
#----------------------
#-----------------------------Input hazard ratios-------------------------------
hr_files=list.files(path = hr_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,"_to_release.csv")]
hr_files=paste0(hr_dir,"/",hr_files)
hr_files
input <- purrr::pmap(list(hr_files),
                     function(fpath){
                       df <- fread(fpath)
                       return(df)})
input=rbindlist(input, fill=TRUE)


#-------------------Select required columns and term----------------------------
input <- input %>% 
  select(-std.error,-robust.se, -P, -redacted_results) %>%
  filter(str_detect(term, "^days"))

#Only require AER for main analysis and full models
input <- subset(input,subgroup=="main")
input <- subset(input,model=="mdl_max_adj")


#---------------------------------Input Table 2---------------------------------
table_2 <- read_csv(paste0(table_2_dir,"/table2_diabetes.csv"))


#-------------------Select required columns and term----------------------------
table_2 <- table_2 %>% select(-stratify_by_subgroup, -strata, -subgroup_cat, -unexposed_person_days, -unexposed_event_count, -post_exposure_event_count, -total_person_days, -day_0_event_counts)
table_2$event <- gsub("out_date_","",table_2$event)

input <- input %>% left_join(table_2, by=c("event","subgroup"))

# #Determine which analyses have a complete set of results so that AER can be calculated
# df <- input %>% select(event, subgroup, model) %>% distinct
# active_available <- merge(active,df)
# results_unavailable <- active %>% anti_join(active_available)
# rm(active,table_2,df)

#-----------------------
#Step 5: Run AER funtion
#-----------------------

lapply(split(active,seq(nrow(active))),
       function(active)
         excess_risk(
           event_of_interest = active$event,
           subgroup_of_interest = active$subgroup,
           input))

#-----------------------
#Step 6: Compile results
#-----------------------
#------------------------------Compile the results------------------------------
# AER_files=list.files(path = aer_raw_results_dir, pattern = "AER_raw_results_*")
# AER_files=paste0(aer_raw_results_dir,"/",AER_files)
# AER_compiled_results <- purrr::pmap(list(AER_files),
#                                     function(fpath){
#                                       df <- fread(fpath)
#                                       return(df)})
# AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)
# write.csv(AER_compiled_results, paste0(aer_results_dir,"/AER_compiled_results.csv"), row.names = F)


#--------------------------Compile results for AER figure-----------------------

# lt_files=list.files(path = aer_raw_results_dir, pattern = "lifetable_*")
# lt_files=paste0(aer_raw_results_dir,"/",lt_files)
# compiled_lifetables <- purrr::pmap(list(lt_files),
#                                    function(fpath){
#                                      df <- fread(fpath)
#                                      return(df)
#                                    })
# compiled_lifetables=rbindlist(compiled_lifetables, fill=TRUE)
# 
# #3.output the csv
# write.csv(compiled_lifetables, paste0(aer_results_dir,"/Figure4_compiled_lifetables.csv"), row.names = F)



#3.Clear the folder(except compiled results)
#if (file.exists(AER_files)) { file.remove(AER_files)}
#4.Sample the results
#print(AER_compiled_results)                                                      #-ve AERs not expected with actual data, but possible.                                                    
#table(AER_compiled_results$AER_196<0)                                            #264 obs with 5 variables as per active analysis list.

