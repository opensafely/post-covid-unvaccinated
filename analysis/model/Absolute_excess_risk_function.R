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
## 1. Extract required variables
## 2. Average daily incidence of each outcome in unexposed age/sex subgroups
## 3. Create life table to calculate cumulative risk over time
## 4. Daily event incidence after multiplying by adjusted hazard ratio
## 5. Absolute excess risk calculation (difference between risk with/without covid)
## =============================================================================

event_of_interest <- "t1dm"
subgroup_of_interest <- "main"

#CREATE A FUNCTION TO CALCULATE THE EXCESS RISK
excess_risk <- function(event_of_interest, subgroup_of_interest, input) {
  
  
  #-------------------------Check structure the input---------------------------
  input <- input %>% mutate(across(starts_with(c("Female","Male")), as.numeric))
  input <- as.data.frame(input)
  
  #-------------------------Subset to relevant data-----------------------------
  input <- input[input$event == event_of_interest & 
                   input$subgroup == subgroup_of_interest,]
  
  #----Add start and end days for time periods which are needed for lifetable---
  for(i in c("time_period_start","time_period_end")){
    input[,i] <- input$term
    input[,i] <- gsub("days", "",input[,i])#Remove 'days'
  }
  
  input$time_period_start <- gsub("\\_.*", "",input[,i])#Remove everything after _
  input$time_period_end <- gsub(".*_", "",input[,i])#Remove everything before _
  input <- input %>% mutate(across(c("time_period_start", "time_period_end"), as.numeric))
  

  #---------------------------------------------------------------
  #Step1. Create life table with the required age/sex variables
  #---------------------------------------------------------------
  lifetable <- data.frame(c(0:535))
  
  colnames(lifetable) <- c("days")
  lifetable$event <- event_of_interest
  lifetable$subgroup <- subgroup_of_interest 
  
  for(l in c("Female","Male")){
    for(m in agelabels){
      for(n in c("unexposed_days","unexposed_events","covid_cases")){
        lifetable[,paste0(l,"_",m,"_",n)] <- input[,paste0(l,"_",m,"_",n)][1]
      }
    }
  } 
  

  #-----------------------------------------------------------------------------
  #Step2: Average daily incidence of each outcome in unexposed age/sex subgroups
  #-----------------------------------------------------------------------------
  #Number of new events / sum of person-time at risk
  for(l in c("Female","Male")){
    for(m in agelabels){
        lifetable[,paste0(l,"_",m,"_incidence_unexp")] <- lifetable[,paste0(l,"_",m,"_unexposed_events")] / lifetable[,paste0(l,"_",m,"_unexposed_days")]
    }
  } 

  #-----------------------------------------------------------------------------
  #Step3: Use life table approach to calculate cumulative risk over time
  #-----------------------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 

  for(l in c("Female","Male")){
    for(m in agelabels){
      lifetable[,paste0(l,"_",m,"_survival_unexp")] <- 1 - lifetable[,paste0(l,"_",m,"_incidence_unexp")] 
      lifetable[,paste0(l,"_",m,"_cumulative_survival_unexp")] <- cumprod(lifetable[,paste0(l,"_",m,"_survival_unexp")]) 
    }
  } 
  

  #----------------------------------------------------------------------
  #Step4.Daily event incidence after multiplying by adjusted hazard ratio
  #----------------------------------------------------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19.
  
  #assign the hr estimates
  lifetable$hr <- 0
  for(i in 1:nrow(input)){
    tmp <- input[i,]
    lifetable$hr <- ifelse(lifetable$days >= tmp$time_period_start & lifetable$days < tmp$time_period_end, tmp$estimate,lifetable$h)
  }
  
  lifetable$hr <- ifelse(lifetable$hr=="[Redacted]","",lifetable$hr)
  lifetable$hr <- as.numeric(lifetable$hr)
  
  for(l in c("Female","Male")){
    for(m in agelabels){
      lifetable[,paste0(l,"_",m,"_incidence_expos")] <- lifetable$hr * lifetable[,paste0(l,"_",m,"_incidence_unexp")] 
      lifetable[,paste0(l,"_",m,"_survival_expos")] <- 1 - lifetable[,paste0(l,"_",m,"_incidence_expos")] 
      lifetable[,paste0(l,"_",m,"_cumulative_survival_expos")] <- cumprod(lifetable[,paste0(l,"_",m,"_survival_expos")]) 
    }
  } 


  #------------------------------------------------------------------------------------
  #Step5. Absolute excess risk calculation (difference between risk with/without covid)
  #------------------------------------------------------------------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis.
  
  #1.AER =difference in absolute risk
  for(l in c("Female","Male")){
    for(m in agelabels){
      lifetable[,paste0(l,"_",m,"_AER")] <- lifetable[,paste0(l,"_",m,"_cumulative_survival_unexp")] - lifetable[,paste0(l,"_",m,"_cumulative_survival_expos")]
    }
  } 
  
  #2.CI of the AER
  #Confidence Interval = Attributable risk +/- 1.96 x Square Root of [p x q (1/n1+ 1/n2)]
  #Where, p = qh, q = 1-qh, n1= unexposed person days, n2 = exposed person days
  #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
  
  for(l in c("Female","Male")){
    for(m in agelabels){
      lifetable[,paste0(l,"_",m,"_std_err")] <- lifetable[,paste0(l,"_",m,"_incidence_expos")] * lifetable[,paste0(l,"_",m,"_cumulative_survival_unexp")]
    }
  } 
  
  lifetable$CI <- 1.96*lifetable$qh*lifetable$'1-qh'*(1/fp_person_days + 1/fp_person_days)
  
  #3.AER%
  lifetable$AERp <-lifetable$'s-sc'*100
  
  #CI of AER%
  #95% CI = ARP +/- ARP x (C.I. range from the attributable risk / the attributable risk)
  #Where, ARP=AERp, CI range= CI, attributable risk = s-sc
  #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
  
  lifetable$CIp <- lifetable$AERp*(lifetable$CI / lifetable$`s-sc`)
  lifetable$CIp.low <- lifetable$AERp - lifetable$CIp
  lifetable$CIp.high <- lifetable$AERp + lifetable$CIp
  
  #Save life table for AER figure
  write.csv(lifetable, paste0(aer_raw_results_dir, "/lifetable_" , cohort_of_interest, "_", model_of_interest, "_", event_of_interest, "_", subgroup_of_interest,".csv"), row.names = F)
  
  
  AER_196 <- lifetable[nrow(lifetable),]$'s-sc' * total_cases
  
  results <- data.frame(event=event_of_interest,
                        cohort=cohort_of_interest,
                        subgroup=subgroup_of_interest,
                        model=model_of_interest,
                        AER_196=AER_196)
  
  write.csv(results, paste0(aer_raw_results_dir, "/AER_raw_results_", cohort_of_interest, "_", model_of_interest, "_", subgroup_of_interest, "_", event_of_interest,".csv"), row.names = F)
  return(results)
  #return(print(results)) 
}
