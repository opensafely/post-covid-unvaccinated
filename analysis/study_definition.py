######################################

# This script provides the formal specification of the study data that will be extracted from 
# the OpenSAFELY database.

######################################

# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

# --- DEFINE STUDY POPULATION ---

## Define study start and end variables explicitly


study = StudyDefinition(
    index_date = "2020-01-01",

    #configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    #Define the study population
    population = patients.satisfying(
    """
        NOT has_died
        AND
        registered
        AND
        age >= 18 
        AND
        age <=110
        AND
        has_follow_up_previous_year
        AND
        (sex = "M" OR sex = "F")
        AND
        imd != "0"
        """,
    
    has_died = patients.died_from_any_cause(
      on_or_before = "index_date",
      returning="binary_flag",
    ),
    
    registered = patients.satisfying(
      "registered_at_start",
      registered_at_start = patients.registered_as_of("index_date"),
    ),
    
    has_follow_up_previous_year = patients.registered_with_one_practice_between(
      start_date = "index_date - 1 year",
      end_date = "index_date",
      return_expectations = {"incidence": 0.95},
    ),
    
  ),

## DEMOGRAPHIC INFORMATION
  
  ### Age
  age = patients.age_as_of(
    "index_date",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),
  
  ### Sex
  sex = patients.sex(
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }
  ),
  
### Index of multiple deprivation
  imd = patients.categorised_as(
    {"0": "DEFAULT",
      "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
      "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
      "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
      "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
      "5": """index_of_multiple_deprivation >= 32844*4/5 """,
    },
    index_of_multiple_deprivation = patients.address_as_of(
      "index_date",
      returning = "index_of_multiple_deprivation",
      round_to_nearest = 100,
    ),
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "0": 0.01,
          "1": 0.20,
          "2": 0.20,
          "3": 0.20,
          "4": 0.20,
          "5": 0.19,
        }},
    },
  ),





# --- DEFINE COVID-19 EXPOSURE VARIABLES ---

positive_covid_test_ever=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        on_or_after="index_date",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date"},
            "rate": "exponential_increase",
        },
    ),

   covid_tpp_probable=patients.with_these_clinical_events(
        combine_codelists(
            covid_identification_in_primary_care_case_codes_clinical,
            covid_identification_in_primary_care_case_codes_test,
            covid_identification_in_primary_care_case_codes_seq,
        ),
        return_first_date_in_period=True,
        include_day=True,
        return_expectations={"date": {"earliest": "index_date"}, "incidence" : 0.95},
    ), 

   covid_tpp_codes_clinical=patients.with_these_clinical_events(
        combine_codelists(covid_identification_in_primary_care_case_codes_clinical),
        return_first_date_in_period=True,
        include_day=True,
        return_expectations={"date": {"earliest": "index_date"}, "incidence" : 0.5},
    ), 

   covid_tpp_codes_test=patients.with_these_clinical_events(
        combine_codelists(covid_identification_in_primary_care_case_codes_test),
        return_first_date_in_period=True,
        include_day=True,
        return_expectations={"date": {"earliest": "index_date"}, "incidence" : 0.5},
    ), 

   covid_tpp_codes_seq=patients.with_these_clinical_events(
        combine_codelists(covid_identification_in_primary_care_case_codes_seq),
        return_first_date_in_period=True,
        include_day=True,
        return_expectations={"date": {"earliest": "index_date"}, "incidence" : 0.5},
    ), 
  


)
