## =============================================================================
## Project:     Post covid unvaccinated project
##
##
## Purpose: 
##  Apply stage 2. Create missing data, range and descriptive statistics tables
##  - Table 0 - Missing data and range
##  - Table 1 - Descriptive statistics
## 
## Authors: Lucy Teece (adapted from original written by Genevieve & Rochelle for vaccinated project)
## Reviewer: Yinghui Wei
## 
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Output missing data and range check tables
## 2. Output table 1
## 
## NOTE: This code outputs 3 .csv files and 1 R dataset
##  
## =============================================================================

###############################################
# 0. Load relevant libraries and read in data #
###############################################
library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)

# # Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort_name <- "" # interactive testing - NOTE - left empty as we only have a single cohort. 
} else {
  cohort_name <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "descriptives"))

# Define stage2 function -------------------------------------------------------

stage2 <- function(group) {
  
  # Load relevant data
  input <- readr::read_rds(file.path("output", paste0("input_stage1_",group,".rds")))
  
  ################################
  # 1. Output missing data table #
  ################################
  
  N <- nrow(input)
  
  #-----------------------------------------------------------------------#
  # 1.a. Create a table with missing data information (N,%) for variables #
  #-----------------------------------------------------------------------#
  
  covariate_names <- tidyselect::vars_select(names(input), starts_with(c('sub_bin_','cov_','vax_cat'), ignore.case = TRUE))
  check_missing <- data.frame(variable = as.vector(covariate_names), N_missing = NA)
  
  for (i in covariate_names){
    if (is.factor(input[,i])) {
      check_missing[check_missing$variable==i,]$N_missing <- nrow(input[input[,i]=="Missing",])
    } else {
      check_missing[check_missing$variable==i,]$N_missing <- nrow(input[is.na(input[,i]),])
    }
  }
  
  check_missing$Perc_missing <- 100*(check_missing$N_missing/N)
  
  #---------------------------------------------------------------#
  # 1.b. Create a table with min and max for numerical covariates #
  #---------------------------------------------------------------#
  
  check_range <- data.frame(variable = character(), Minimum_value = character(), Maximum_value = character())
  numeric_var_names=colnames(select_if(input, is.numeric))
  numeric_var_names = numeric_var_names [!numeric_var_names == "patient_id"]
  
  for (i in numeric_var_names){
    check_range[nrow(check_range)+1,1] <- i
    check_range[nrow(check_range),2] <- min(na.omit(input %>% dplyr::select(i)))
    check_range[nrow(check_range),3] <- max(na.omit(input %>% dplyr::select(i)))
  }
  
  #---------------------------------------------------------------------------#
  # 1.a. c. Output table merging missing and range information for covariates #
  #---------------------------------------------------------------------------#
  
  check_both <- merge(x=check_missing, y=check_range, by = "variable",all.x=TRUE)
  write.csv(check_both, file = file.path("output/not-for-review", paste0("Check_missing_range.csv")) , row.names=F)
  
  #---------------------------------------------------------#
  # 1.d. Create a table with min and max for date variables #
  #---------------------------------------------------------#
  
  check_dates <- data.frame(variable = character(), Earliest_date = character(), Latest_date = character())
  date_variables_names <- colnames(select_if(input, is.Date))
  input_date <- input[,date_variables_names]
  
  for (i in date_variables_names){
    date_var <- input_date %>% pull(i)
    check_dates[nrow(check_dates)+1,1] <- i
    check_dates[nrow(check_dates),2] <- paste0("",min(na.omit(date_var)))
    check_dates[nrow(check_dates),3] <- paste0("",max(na.omit(date_var)))
  }
  
  write.csv(check_dates, file = file.path("output/not-for-review", paste0("Check_dates_range.csv")) , row.names=F)
  
  #####################
  # 2. Output table 1 #
  #####################
  
  #Define populations of interest
  
  pop <- data.frame(rbind(c("Whole_population","!is.na(input$patient_id)"),
                          c("COVID_exposed","is.na(input$exp_date_covid19_confirmed)==F"),
                          c("COVID_hospitalised","input$sub_cat_covid19_hospital=='hospitalised'"),
                          c("COVID_non_hospitalised","input$sub_cat_covid19_hospital=='non_hospitalised'")),
                    stringsAsFactors = FALSE)
  
  colnames(pop) <- c("name","condition")
  
  # Define age groups
  
  input$cov_cat_age_group <- ""
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=29, "18-29", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=30 & input$cov_num_age<=39, "30-39", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=49, "40-49", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=50 & input$cov_num_age<=59, "50-59", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=69, "60-69", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=70 & input$cov_num_age<=79, "70-79", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=80 & input$cov_num_age<=89, "80-89", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=90, "90+", input$cov_cat_age_group)
  
  # Define consultation rate groups
  
  input$cov_cat_consulation_rate_group <- ""
  input$cov_cat_consulation_rate_group <- ifelse(input$cov_num_consulation_rate==0, "0", input$cov_cat_consulation_rate_group)
  input$cov_cat_consulation_rate_group <- ifelse(input$cov_num_consulation_rate>=1 & input$cov_num_consulation_rate<=5, "1-5", input$cov_cat_consulation_rate_group)
  input$cov_cat_consulation_rate_group <- ifelse(input$cov_num_consulation_rate>=6, "6+", input$cov_cat_consulation_rate_group)
  
  # Populate table 1 
  
  ##When ready to use active analysis can uncomment below
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>% filter(active==TRUE,
                                                outcome_group == group)
  covar_names<-str_split(active_analyses$covariates, ";")[[1]]
  
  ##Remember to also uncomment the covariate codes and comment out the existing ones
  #categorical_cov <- colnames(input)[grep("cov_cat", colnames(input))]
  categorical_cov <- covar_names[grep("cov_cat", covar_names)]
  categorical_cov <- append(categorical_cov, c("cov_cat_age_group","cov_cat_consulation_rate_group"))
  
  # numerical_cov <- colnames(input)[grep("cov_num", colnames(input))]
  # numerical_cov <- numerical_cov[!numerical_cov=="cov_num_age"]
  numerical_cov <- covar_names[grep("cov_num", covar_names)]
  numerical_cov <- numerical_cov[!numerical_cov=="cov_num_age"]
  
  # binary_cov <- colnames(input)[grep("cov_bin", colnames(input))]
  binary_cov <- covar_names[grep("cov_bin", covar_names)]
  
  # Base table
  
  table1 <- input %>% 
    dplyr::select(c(categorical_cov,numerical_cov,binary_cov))
  
  table1 <- table1 %>% 
    mutate_if(is.character,as.factor)
  
  table1 <- table1 %>% 
    mutate_if(is.logical,as.factor)
  
  table1 <- as.data.frame(summary(table1,maxsum=50))
  
  table1 <- rename(table1, Covariate_level = Freq, Covariate = Var2)
  
  table1 <- table1 %>% 
    filter(!str_detect(Covariate_level, "^FALSE"))
  
  table1$Covariate <- gsub("\\s","",table1$Covariate) # Remove spaces
  
  table1$Covariate_level <-  sub('\\:.*', '', table1$Covariate_level) # Remove everything after :
  
  table1 <- table1 %>%
    filter(!(Covariate == "cov_num_consulation_rate" & !Covariate_level=="Mean   ")) %>%
    filter(!(Covariate == "cov_num_tc_hdl_ratio" & !Covariate_level=="Mean   "))
  
  table1_count_all <- as.data.frame(matrix(nrow = 1, ncol = 2))
  colnames(table1_count_all) <- c("Covariate","Covariate_level")
  table1_count_all[1,] <- c("All","All")
  table1 <- table1 %>% dplyr::select(Covariate, Covariate_level)
  table1 <- rbind(table1_count_all,table1)
  
  for (j in 1:nrow(pop)) {
    
    population <- pop[j,]$name
    
    df <- subset(input, eval(parse(text = pop[j,]$condition)))
    
    # Count population size
    
    pop_summary <- data.frame(matrix(ncol=3))
    colnames(pop_summary) <- c("Covariate","Covariate_level",population)
    pop_summary[1,] <- c("All","All",nrow(df))
    
    # Categorical covariates
    
    cat_pop <- df %>% dplyr::select(categorical_cov)
    cat_pop <- cat_pop %>% mutate_if(is.character,as.factor)
    cat_summary <- as.data.frame(summary(cat_pop,maxsum=50))
    cat_summary[,population] <- cat_summary$Freq
    cat_summary <- rename(cat_summary, Covariate_level = Freq, Covariate = Var2)
    cat_summary <- cat_summary %>% 
      dplyr::select("Covariate","Covariate_level",population)
    
    # Numerical covariates
    
    num_pop <- df %>% dplyr::select(numerical_cov)
    num_summary <- as.data.frame(summary(num_pop))
    num_summary[,population] <- num_summary$Freq
    num_summary <- rename(num_summary, Covariate_level = Freq, Covariate = Var2)
    num_summary <- num_summary %>% dplyr::select("Covariate","Covariate_level",population)
    num_summary <- num_summary %>% filter(startsWith(num_summary$Covariate_level, "Mean")==T)
    
    # Binary covariates
    
    bin_pop <- df %>% dplyr::select(binary_cov)
    bin_pop <- bin_pop %>% mutate_if(is.logical,as.factor)
    bin_summary <- as.data.frame(summary(bin_pop))
    bin_summary[,population] <- bin_summary$Freq
    bin_summary <- rename(bin_summary, Covariate_level = Freq, Covariate = Var2)
    bin_summary <- bin_summary %>% filter(str_detect(Covariate_level, "^TRUE"))
    bin_summary <- bin_summary%>% dplyr::select("Covariate","Covariate_level",population)
    
    # Population summary
    
    pop_summary <- rbind(pop_summary,cat_summary,num_summary,bin_summary)
    
    # Tidy summary table
    
    pop_summary$Covariate <- gsub("\\s","",pop_summary$Covariate) #Remove spaces
    pop_summary$Covariate_level <- sub('\\:.*', '', pop_summary$Covariate_level) #Remove everything after :
    
    pop_summary[,population] <- gsub(".*:", "",pop_summary[,population])#Remove everything before:
    pop_summary <- pop_summary %>% drop_na(Covariate_level)#Remove rows with NA
    
    # Left join onto base table
    
    table1 <- left_join(table1,pop_summary, by=c("Covariate","Covariate_level"))
    
  }
  
  # Tidy table 1
  
  table1$Covariate <- gsub("cov_bin_", "History of ",table1$Covariate)
  table1$Covariate <- gsub("cov_\\D\\D\\D_", "",table1$Covariate)
  table1$Covariate <- gsub("_", " ",table1$Covariate)
  
  # Add in suppression controls for counts <=5 and then alter totals accordingly
  table1_suppressed <- table1[0,]
  unique(table1$Covariate[which(!startsWith(table1$Covariate_level, "Mean"))])
  
  for(i in unique(table1$Covariate[which(!startsWith(table1$Covariate_level, "Mean"))])){
    df<- table1 %>% filter(Covariate == i)
    df <- df %>% mutate(across(!c("Covariate","Covariate_level"),as.numeric))
    df$No_infection <- df$Whole_population - df$COVID_exposed
    print(df$Whole_population)    
    if(any(df$COVID_hospitalised <= 5 | df$COVID_non_hospitalised <= 5 | is.na(df$COVID_hospitalised | is.na(df$COVID_non_hospitalised)))){
      df$COVID_hospitalised <- "[Redacted]"
      df$COVID_non_hospitalised <- "[Redacted]"
    }
    
    if(any(df$COVID_exposed <= 5 | df$No_infection <=5 | is.na(df$No_infection))){
      df$COVID_exposed <- "[Redacted]"
      df$COVID_hospitalised <- "[Redacted]"
      df$COVID_non_hospitalised <- "[Redacted]"
    }
    
    if(any(df$Whole_population <= 5)){
      df$Whole_population <- "[Redacted]"
      df$COVID_hospitalised <- "[Redacted]"
      df$COVID_non_hospitalised <- "[Redacted]"
      df$COVID_exposed <- "[Redacted]"
    }
    
    df <- df %>% select(!No_infection)
    df <- df %>% mutate(across(!c("Covariate","Covariate_level"),as.character))
    if(i == "consulation rate group"){
      df <- rbind(df, table1 %>% filter(startsWith(Covariate_level, "Mean")))
    }
    
    table1_suppressed <- rbind(table1_suppressed,df)
    
  }
  #table1_suppressed[which(startsWith(table1_suppressed$Covariate_level, "Mean")),] <- table1[startsWith(table1$Covariate_level, "Mean"),]
  table1_suppressed <- table1_suppressed %>% filter(!str_detect(Covariate_level, "^FALSE"))
  
  # Save table 1
  
  write.csv(table1_suppressed, file = file.path("output/review/descriptives", paste0("Table1_",group,".csv")) , row.names=F)
  
}

# Run function using specified commandArgs and different outcome groups
active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active==TRUE)
groups <- unique(active_analyses$outcome_group)

for(i in groups){
  stage2(i)
}

