## =============================================================================
## Project:     Post covid unvaccinated project
##
##
## Purpose:  Derive diabetes outcome variables using Algorithm to adjudicate diabetes presence and type
## 
## Authors: Rachel Denholm based on stata code from Sophie Eastwood
## Reviewer: 
##
## DATA REQUIRED: 		(in wide format i.e. 1 observation per person)
## 1) Diabetes diagnostic and non-diagnostic codes (see codelist) and corresponding dates
## 2) Antidiabetic medication and corresponding dates
## 3) Year of birth to calculate age at first diagnostic code
## 4) Ethnicity
## 5) Latest HbA1c value (if available)
##
## OUTPUT: Individuals are classified into one of five end states:
## (coded by the "adj_dm" variable)
## type 1 diabetes, type 2 diabetes, gestational diabetes, 
## diabetes unspecified type, diabetes unlikely	
## 
##
##
## =============================================================================