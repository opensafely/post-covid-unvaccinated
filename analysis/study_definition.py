# Import statements

## Set seed
import numpy as np
np.random.seed(123456)

## Cohort extractor
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Study definition helper
import study_def_helper_functions as helpers

## Import common variables function
from common_variables import generate_common_variables
(
    dynamic_variables
) = generate_common_variables(index_date_variable="index_date")

## Variables for deriving JCVI groups
from grouping_variables import (
    jcvi_variables, 
    start_date,
    end_date,
)

study = StudyDefinition(

# Specify index date for study
    index_date = "2020-01-01",

# Configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

# Define the study population 
 # NB: not all inclusions and exclusions are written into study definition
    population = patients.satisfying(
        """
            NOT has_died
            AND
            registered        
            AND
            has_follow_up_previous_6months
            """,
        
        has_died = patients.died_from_any_cause(
        on_or_before = "index_date",
        returning="binary_flag",
        ),
        
        registered = patients.satisfying(
        "registered_at_start",
        registered_at_start = patients.registered_as_of("index_date"),
        ),
        
        has_follow_up_previous_6months = patients.registered_with_one_practice_between(
        start_date = "index_date - 6 months",
        end_date = "index_date",
        return_expectations = {"incidence": 0.95},
        ),
    ),
   
# Define quality assurances
   ## Prostate cancer
        ### Primary care
            prostate_cancer_snomed=patients.with_these_clinical_events(
                prostate_cancer_snomed_clinical,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.03,
                },
            ),
        ### HES APC
            prostate_cancer_hes=patients.admitted_to_hospital(
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.03,
                },
            ),
        ### ONS
            prostate_cancer_death=patients.with_these_codes_on_death_certificate(
                prostate_cancer_icd10,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.02
                },
            ),
        ### Combined
            qa_bin_prostate_cancer=patients.maximum_of(
                "prostate_cancer_snomed", "prostate_cancer_hes", "prostate_cancer_death"
            ),

        ## Pregnancy
            qa_bin_pregnancy=patients.with_these_clinical_events(
                pregnancy_snomed_clinical,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.03,
                },
            ),
        
 ## Year of birth
     qa_num_birth_year=patients.date_of_birth(
        date_format="YYYY",
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest": "today"},
            "rate": "uniform",
            },
        ),   

# Define death date
    ## Primary care
        primary_care_death_date=patients.with_death_recorded_in_primary_care(
            on_or_after="index_date",
            returning="date_of_death",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": "index_date", "latest" : "today"},
                "rate": "exponential_increase",
            },
        ),
    ## ONS
        ons_died_from_any_cause_date=patients.died_from_any_cause(
            on_or_after="index_date",
            returning="date_of_death",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": "index_date", "latest" : "today"},
                "rate": "exponential_increase",
            },
        ),
    ## Combined
        death_date=patients.minimum_of(
            "primary_care_death_date", "ons_died_from_any_cause_date"
        ),

# Define fixed covariates 
    # Define sex 
        cov_cat_sex = patients.sex(
            return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
            }
        ),

        ## 2019 consultation rate
            cov_num_consulation_rate=patients.with_gp_consultations(
                between=["2019-01-01", "2019-12-31"],
                returning="number_of_matches_in_period",
                return_expectations={
                    "int": {"distribution": "poisson", "mean": 5},
                },
            ),
    
        ## Healthcare worker    
        cov_bin_healthcare_worker=patients.with_healthcare_worker_flag_on_covid_vaccine_record(
            returning='binary_flag', 
            return_expectations={"incidence": 0.01},
        ),
    
    # Most recent BMI  
      cov_num_bmi=patients.most_recent_bmi(
        on_or_after="2010-02-01",
        minimum_age_at_measurement=16,
        include_measurement_date=True,
        include_month=True,
        return_expectations={
            "date": {},
            "float": {"distribution": "normal", "mean": 35, "stddev": 10},
            "incidence": 0.95,
        },
    ),

# Date de-registered from any OpenSafely GP CURRENTLY UNSUPPORTED
#cov_date_dereg=patients.date_deregistered_from_all_supported_practices(
#    on_or_after="index_date",
#    date_format="YYYY-MM-DD",
#    return_expectations={
#        "date": {"earliest": "index_date", "latest": "today"},
#        "incidence": 0.05
#    }
#)

    # Define vaccine eligibility variables

        **jcvi_variables, 
    
    # Define common variables (e.g., exposures, outcomes, covariates) that require dynamic dates

        **dynamic_variables
)

