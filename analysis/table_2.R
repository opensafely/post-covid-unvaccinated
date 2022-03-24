## =============================================================================
## Purpose:  Create Table 2
## 
## Author:   Kurt Taylor
##
## Reviewer: Rochelle Knight
## 
## Date:     March 2022
##
## Data:     Post covid unvaccinated project study population
##
## Content:  Number of outcome events;
##           person years of follow up and rates of events, for each outcome
##
## Output:   CSV file for table 2.
## =============================================================================

# Libraries ---------------------------------------------------------------

library(readr); library(dplyr); library(data.table); library(lubridate)

# Study start and end date ------------------------------------------------

cohort_start = as.Date("2020-01-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-06-18", format="%Y-%m-%d")

# Read active analyses -----------------------------------------------

active_analyses <- read_rds("lib/active_analyses.rds")

# Read data------------------------------------------------------------

input <- read_rds(paste0("output/input_stage1.rds"))

# cohort start date and end date

input <- input %>% 
  mutate(cohort_start_date = cohort_start,
         cohort_end_date = cohort_end)

# Events and Table 2 df ------------------------------------------------------------------

event_dates_names <- active_analyses$outcome_variable[which(active_analyses$active==T)]
event_names <- event_names <- gsub("out_date_","",event_dates_names)
col_headings <- c("event", "event_count", "person_years_follow_up", "incidence_rate")
table_2 <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_dates_names)))
colnames(table_2) <- col_headings
table_2$event <- event_names

# Build function for Table 2 ----------------------------------------------

summary_stats <- function(input, infection_subgroup, event_dates_names, index){
  
  # event date 
  input <- input %>%
    mutate(event_date = get(event_dates_names[index]))
  # signify follow up end date
  input <- input %>% rowwise() %>% mutate(follow_up_end = min(event_date, death_date, cohort_end_date, vax_date_covid_1, na.rm = TRUE))
  # follow-up days
  input = input %>% mutate(follow_up_period = as.numeric((as.Date(follow_up_end) - as.Date(index_date)))+1) 
  # follow up period and follow up years
  input = input %>% filter(follow_up_period >=1 & follow_up_period <= 535) # filter out follow up period 
  input = input %>% mutate(follow_up_years = follow_up_period / 365.2) # follow-up years
  # Event count
  if(infection_subgroup == "no_infection"){
    event_count <- length(which((input$event_date >= input$index_date & input$event_date <= input$follow_up_end) &
                                  (input$event_date < input$exp_date_covid19_confirmed | is.na(input$exp_date_covid19_confirmed))
    ))
  }else{
    event_count <- length(which(input$event_date   >= input$index_date &
                                  input$event_date >= input$exp_date_covid19_confirmed & 
                                  input$event_date <= input$follow_up_end))
  }
  person_years_follow_up  = round(sum(input$follow_up_years, na.rm = TRUE), 1)
  incidence_rate = round(event_count/person_years_follow_up, 4)
  incidence_rate_lower = incidence_rate - 1.96 * sqrt(event_count/person_years_follow_up^2)
  incidence_rate_upper = incidence_rate + 1.96 * sqrt(event_count/person_years_follow_up^2)
  return(c(event_count, person_years_follow_up, incidence_rate, incidence_rate_lower, incidence_rate_upper))
}

# Run function on outcomes ------------------------------------------------

for(i in 1:length(event_dates_names)){
  table_2[i,2:6] <- summary_stats(input, "no_infection", event_dates_names, i)
  table_2[i,7:11] <- summary_stats(input[input$sub_cat_covid19_hospital=="non_hospitalised",], "non_hospitalised", event_dates_names, i)
  table_2[i,12:16] <- summary_stats(input[input$sub_cat_covid19_hospital=="hospitalised",], "hospitalised", event_dates_names, i)
  table_2$total_event_count <- table_2[,2] + table_2[,7] + table_2[,12]
  table_2$total_person_yrs <-  table_2[,3] + table_2[,8] + table_2[,13]
  table_2$overall_incidence_rate <- round(table_2$total_event_count/table_2$total_person_yrs,4)
  table_2$overall_incidence_rate_lower <- table_2$overall_incidence_rate - 1.96*sqrt(table_2$total_event_count/table_2$total_person_yrs^2)
  table_2$overall_incidence_rate_upper <- table_2$overall_incidence_rate + 1.96*sqrt(table_2$total_event_count/table_2$total_person_yrs^2)
  names(table_2)[2:6] <- c("no_infection_sub_event_count", "no_infection_sub_person_yrs_fp", "no_infection_sub_incidence_rate", "no_infection_sub_incidence_rate_lower", "no_infection_sub_incidence_rate_upper")
  names(table_2)[7:11] <- c("non_hospitalised_sub_event_count", "non_hospitalised_sub_person_yrs_fp", "non_hospitalised_sub_incidence_rate", "non_hospitalised_sub_incidence_rate_lower","non_hospitalised_sub_incidence_rate_upper")
  names(table_2)[12:16] <- c("hospitalised_sub_event_count", "hospitalised_sub_person_yrs_fp", "hospitalised_sub_incidence_rate", "hospitalised_sub_incidence_rate_lower", "hospitalised_sub_incidence_rate_upper")
  names(table_2)[17:21] <- c("total_event_count", "total_person_yrs", "overall_incidence_rate", "overall_incidence_rate_lower", "overall_incidence_rate_upper")
}

# Low number suppression --------------------------------------------------
# change to "NA" if event count lower than or equal to 5

table_2[which(table_2$no_infection_sub_event_count <= 5), c(2,4,5,6)] = c("<=5", "NA", "NA", "NA")
table_2[which(table_2$non_hospitalised_sub_event_count <= 5),c(7,9,10,11)] = c("<=5", "NA", "NA", "NA")
table_2[which(table_2$hospitalised_sub_event_count <= 5),c(12,14,15,16)] = c("<=5", "NA", "NA", "NA")
table_2[which(table_2$total_event_count <= 5),c(17,19,20,21)] = c("<=5", "NA", "NA", "NA")
table_2[which(table_2$no_infection_sub_event_count == "<=5" | table_2$non_hospitalised_sub_event_count == "<=5" | table_2$hospitalised_sub_event_count == "<=5" ),c(17,19,20,21)] = c("<=5", "NA","NA","NA")

# OUTPUT ------------------------------------------------------------------

# add suffix to out file name using active analyses (diabetes or mental health)
out_group <- active_analyses %>% filter(active == TRUE) %>% distinct(outcome_group) %>% pull(outcome_group)
# save 
write.csv(table_2, file= paste0("output/", "table2_",out_group,".csv"), row.names = F)

# END