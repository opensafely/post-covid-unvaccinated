# Load libraries ---------------------------------------------------------------
library(magrittr)

# Define parameters ------------------------------------------------------------

## Study start date
study_start <- "2020-01-01"

## Load dataset
df <- arrow::read_feather(file = "C:/Users/rd16568/Documents/Github/post-covid-unvaccinated/output/input.feather")

## Identify dynamic variables in dataset

keep <- c("patient_id",
          colnames(tmp)[grepl("sub_",colnames(tmp))], # Subgroups
          colnames(tmp)[grepl("exp_",colnames(tmp))], # Exposures
          colnames(tmp)[grepl("out_",colnames(tmp))], # Outcomes
          colnames(tmp)[grepl("cov_",colnames(tmp))]) # Covariates

keep <- keep[!grepl("tmp_exp_",keep)]
keep <- keep[!grepl("tmp_sub_",keep)]
keep <- keep[!grepl("tmp_cov_",keep)]

keep <- intersect(keep,colnames(tmp))

tmp_dynamic <- tmp[,keep]