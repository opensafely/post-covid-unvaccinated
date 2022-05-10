## =============================================================================
## Project:     Post covid unvaccinated project
##
## Purpose:  
##  Apply stage 5. Absolute excess risk analysis
##  - Excess risk function
##  - Create table AER for active analyses
## 
## Authors: Lucy Teece (adapted from original written by RT, XJ & VW for vaccinated project)
## Reviewer: TBD
## 
## Content: 
## 0. Load relevant libraries
## 1. Code excess risk function
## 2. Apply absolute excess risk for active analyses
##
## TO RUN OUTSIDE OPENSAFELY PLATFORM
## 1. load the right input data and make sure of the file names and variable structure
## 2. Cntrl+A run the whole script and find the results in working directory
## =============================================================================

## Search for "!!!check" to view comments on items to check before pushing - remove comments when resolved

###############################################
# 0. Load relevant libraries and read in data #
###############################################

#USE - TO CHECK SINGLE AER
outcome <- "ate" 
group <- "unvaccinated"             ##Single group in unvax analysis - keep for compatibility with vax code (!!!check)
strata <- "prior_history_FALSE"     
fit <- "mdl_max_adj"

library(purrr)
library(data.table)
library(tidyverse)

#CALCULATE THE EXCESS RISK
excess_risk <- function(outcome, group, strata, fit) {
  
  #Load data 
  #1.Input1 - 1.unexposed person days
  input1.1 <- readr::read_csv("output/input1_aer_main_unvaccinated.csv")        #!!!check file path (sub-folders) and name of population (unvax or "")? 
  input1.2 <- readr::read_csv("output/input1_aer_subgroups_unvaccinated.csv")   #!!!check file path (sub-folders) and name of population (unvax or "")? - wait for t2 code update
  
  #Preprocess input1                                                            #ADDS TWO MODEL FITS WITH SAME PERSON DAYS
  input1.3 <- rbind(input1.1,input1.2)
  input1.3$fit <- "mdl_agesex"
  
  input1.4 <- input1.3
  input1.4$fit <- "mdl_max_adj"
  
  input1 <-rbind(input1.3,input1.4)
  input1 <- input1 %>% select(-strata)
  rm(input1.1, input1.2, input1.3, input1.4)
  
  #structure the input
  input1$unexposed_person_days <- as.numeric(input1$unexposed_person_days)
  
  #input2 - 2.unexposed events, 3.total population cases, 4.HR                  #COMBINES THE HR TABLES
  hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")       #!!!check file path (sub-folders)
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0("output/",hr_files)                                           #!!!check file path (sub-folders)

  input2 <- purrr::pmap(list(hr_files),
                        function(fpath){
                          df <- fread(fpath)
                          return(df)})
  input2=rbindlist(input2, fill=TRUE)

  #Preprocess input2                                                            #SELECTS REQUIRED COLUMNS & TERMS
  input2 <- input2 %>% select(-conf.low, -conf.high, -std.error, -robust.se, -P, -covariates_removed, -cat_covars_collapsed)
  
  #!!!check the time intervals - code from vax in comments, code with protocol splits as below, what is used in COX code?:
  # input2 <- input2 %>% filter(term == "days0_14" |
  #                             term == "days14_28" |
  #                             term == "days28_56" |
  #                             term == "days56_84" |
  #                             term == "days84_197"|
  #                             term == "days0_28"|
  #                             term == "days28_197")
  input2 <- input2 %>% filter(term == "days0_7"     |
                              term == "days7_14"    |
                              term == "days14_28"   |
                              term == "days28_56"   |
                              term == "days56_84"   |
                              term == "days84_182"  |
                              term == "days182_365" |
                              term == "days0_28"    |
                              term == "days28_365")
  
  #--------------------------------------
  # Step1: Extract the required variables
  #--------------------------------------

  #1. Person days
  fp_person_days <- input1[input1$event == outcome & input1$fit == fit  &
                             input1$cohort == group & input1$subgroup == strata,]$unexposed_person_days
  #2.unexposed events
  unexposed_events <- input2[input2$event == outcome & input2$model == fit  & 
                               input2$cohort == group & input2$subgroup == strata & 
                               input2$expo_week== "pre expo",]$events_total
  #3.Total cases
  total_cases <-  input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata & 
                           input2$expo_week== "pre expo",]$total_covid19_cases
  
  #4.locate the estimates
  #!!!check these intervals - code from vax in comments, code with protocol splits as below:
  # #0-14 days
  # hr_14 <- input2[input2$event == outcome  & input2$model == fit  & 
  #                   input2$cohort == group & input2$subgroup == strata & input2$term == "days0_14",]$estimate
  # #14-28 days
  # hr_28 <- input2[input2$event == outcome & input2$model == fit  & 
  #                   input2$cohort == group & input2$subgroup == strata& input2$term == "days14_28",]$estimate
  # #28-56 days
  # hr_56 <- input2[input2$event == outcome & input2$model == fit  & 
  #                   input2$cohort == group & input2$subgroup == strata& input2$term == "days28_56",]$estimate
  # #56-84 days
  # hr_84 <- input2[input2$event == outcome & input2$model == fit  & 
  #                   input2$cohort == group & input2$subgroup == strata& input2$term == "days56_84",]$estimate
  # #84-196 days
  # hr_196 <- input2[input2$event == outcome & input2$model == fit  & 
  #                    input2$cohort == group & input2$subgroup == strata& input2$term == "days84_197",]$estimate
  # #Alternative 0-28 days
  # hr0_28 <- input2[input2$event == outcome  & input2$model == fit  & 
  #                    input2$cohort == group & input2$subgroup == strata& input2$term == "days0_28",]$estimate
  # #Alternative 28_196 days
  # hr28_196 <- input2[input2$event == outcome  & input2$model == fit  & 
  #                      input2$cohort == group & input2$subgroup == strata& input2$term == "days28_197",]$estimate
  #0-7 days
  hr_6 <- input2[input2$event == outcome  & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata & input2$term == "days0_7",]$estimate
  #7-14 days
  hr_14 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days7_14",]$estimate
  #14-28 days
  hr_28 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days14_28",]$estimate
  #28-56 days
  hr_56 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days28_56",]$estimate
  #56-84 days
  hr_84 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days56_84",]$estimate
  #84-182 days
  hr_182 <- input2[input2$event == outcome & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days84_182",]$estimate
  #182-365 days
  hr_365 <- input2[input2$event == outcome & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days182_365",]$estimate
  
  #Alternative 0-28 days
  hr0_28 <- input2[input2$event == outcome  & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days0_28",]$estimate
  #Alternative 28_365 days
  hr28_365 <- input2[input2$event == outcome  & input2$model == fit  & 
                       input2$cohort == group & input2$subgroup == strata& input2$term == "days28_365",]$estimate

  #!!!check 3 duplicate entries (HRs) for each - are these needed?
  
  #--------------------------------------------------------------
  #Step2.Average daily adverse event incidence - in the unexposed
  #--------------------------------------------------------------
  #Number of new events / sum of person-time at risk
  incidence_rate <- unexposed_events/fp_person_days
  
  #-------------------------------------------------------------
  #Step3. Make life table to calculate cumulative risk over time
  #-------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 
  #!!!check code runs for all strata, not just age-sex specific ones - is that correct?
  lifetable <- data.frame(c(1:365))                                             #!!!check correct follow up limits
  colnames(lifetable) <- c("days")
  lifetable$event <- outcome
  lifetable$model <- fit
  lifetable$cohort <- group
  lifetable$subgroup <- strata 
  lifetable$q <- incidence_rate 
  lifetable$'1-q' <- 1 - lifetable$q 
  lifetable$s <- cumprod(lifetable$`1-q`)
  
  #-----------------------------------
  #Step4.Daily adverse event incidence
  #-----------------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19.
  
  #assign the hr estimates
  #!!!check these intervals - code from vax in comments, code with protocol splits as below:
  # lifetable$h <- ifelse(lifetable$days < 15, rep(hr_14),0)
  # lifetable$h <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28),lifetable$h)
  # lifetable$h <- ifelse(lifetable$days < 29 & is.na(lifetable$h), rep(hr0_28),lifetable$h)#alternative for 0-28 days
  # 
  # lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56),lifetable$h)
  # lifetable$h <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84),lifetable$h)
  # lifetable$h <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196),lifetable$h)
  # lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h), rep(hr28_196),lifetable$h)#alternative for 28-196 days

  #!!!check first interval - vax code combines 0-7 and 7-14, should we?
  lifetable$h <- ifelse(lifetable$days < 8, rep(hr_7),0)
  lifetable$h <- ifelse(lifetable$days > 7 & lifetable$days < 15, rep(hr_14),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28),lifetable$h)
  lifetable$h <- ifelse(lifetable$days < 29 & is.na(lifetable$h), rep(hr0_28),lifetable$h)    #Alternative for 0-28 days
 
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 84 & lifetable$days < 183, rep(hr_182),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 182 & lifetable$days < 366, rep(hr_365),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 366 & is.na(lifetable$h), rep(hr28_365),lifetable$h)   #Alternative for 28-196 days
  
  lifetable$qh <- lifetable$q*lifetable$h
  lifetable$'1-qh' <- 1 - lifetable$qh
  lifetable$sc <- cumprod(lifetable$`1-qh`)

  #---------------------------
  #Step5. Absolute excess risk
  #---------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  #1.AER =difference in absolute risk
  lifetable$'s-sc' <- lifetable$s - lifetable$sc
  AER_365 <- lifetable[nrow(lifetable),]$'s-sc' * total_cases
  
  results <- data.frame(event=outcome,
                        cohort=group,
                        subgroup=strata,
                        model=fit,
                        AER_365=AER_365)
  write.csv(results, paste0("output/AER_" , group, "_", fit, "_", strata, "_", outcome,".csv"), row.names = F)
  return(results)
  #return(print(results)) 
}

#-------------------------------
#Step6. AER for active analyses
#-------------------------------
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                            # Selects active analyses
#active <- active_analyses                                                      # Manual alternative
active <- active[active$active==TRUE,]   

#Preprocess the active analyses
active$event <- gsub("out_date_","",active$outcome_variable)                                     # refine event name                                                    
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL       # removes unused columns
active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")                                               # converts to long data                                        
active <- active[active$value==TRUE, c("event","model","cohort","strata")]                       # refines to active models                         
active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)                # includes 2 model types     
active <- tidyr::separate_rows(active, model, sep = ";")                                         # separate rows for each model

#!!!check how managing cohrts in table 2 / active analysis and update below
active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort) # includes 2 cohorts
active <- tidyr::separate_rows(active, cohort, sep = ";")                                        # separate rows for each cohort

colnames(active)[colnames(active) == 'group'] <- 'strata'                                        #Tweaks the column names                   
colnames(active)[colnames(active) == 'cohort'] <- 'group'
colnames(active)[colnames(active) == 'model'] <- 'fit'
colnames(active)[colnames(active) == 'event'] <- 'outcome'
active <- active %>% select(-fit, everything())                                                  #Order the columns 

active <- active %>% filter(!strata == "ethnicity_Missing")                                      #Remove not available models                  
input1 <- input1 %>% filter(!subgroup== "ethnicity_Missing")                                     #matches above with input1

#----------------------
#Step7. Output results
#----------------------
#1.For Loop the function.
for (i in 1:nrow(active)) {excess_risk(active$outcome[i], active$group[i],active$strata[i], active$fit[i])}
#2.Compile the results
AER_files=list.files(path = "output", pattern = "AER_*")                        #!!!check file path (sub-folders)?
AER_files=AER_files[endsWith(AER_files,".csv")]
AER_files=paste0("output/",AER_files)                                           #!!!check file path (sub-folders)?
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)
write.csv(AER_compiled_results, "output/AER_compiled_results.csv", row.names = F)   #!!!check file path (sub-folders)?
#3.Clear the folder(except compiled results)
if (file.exists(AER_files)) { file.remove(AER_files)}
#4.Sample the results
print(AER_compiled_results)                                                      #-ve AERs not expected with actual data
table(AER_compiled_results$AER_365<0)