# Load libraries ---------------------------------------------------------------
library(magrittr)

# Define parameters ------------------------------------------------------------

## Study start date
study_start <- "2020-01-01"

## Load dataset
df <- arrow::read_feather(file = "output/input.feather")

# Convert dates to date format -------------------------------------------------
date_names <- tidyselect::vars_select(names(df), starts_with(c('_date'), ignore.case = TRUE))

for (colname in date_names){
  df[[colname]] <- as.Date(df[[colname]])
}

df$qa_num_birth_year <- format(df$qa_num_birth_year,"%Y")

# Convert numbers to number format ---------------------------------------------
num_names <- tidyselect::vars_select(names(df), starts_with(c('_num'), ignore.case = TRUE))

for (colname in num_names){
  df[[colname]] <- as.numeric(df[[colname]])
}

# Convert categories to factor format ------------------------------------------
factor_names <- tidyselect::vars_select(names(df), starts_with(c('_cat'), ignore.case = TRUE))

for (colname in factor_names){
  df[[colname]] <- as.factor(df[[colname]])
}

# Convert binaries to logical format -------------------------------------------
bin_names <- tidyselect::vars_select(names(df), starts_with(c('_bin'), ignore.case = TRUE))

for (colname in bin_names){
  df[[colname]] <- as.logical(df[[colname]])
}

# Define COVID-19 severity --------------------------------------------------------------
  
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
  
