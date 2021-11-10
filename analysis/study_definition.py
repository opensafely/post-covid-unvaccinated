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
import study_def_helper_functions as helpers

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
        cov_age >= 18 
        AND
        cov_age <=110
        AND
        has_follow_up_previous_year
        AND
        (cov_sex = "M" OR cov_sex = "F")
        AND
        cov_deprivation != "0"
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

##NB exclusions not written into study definition


# --- DEFINE COVID-19 EXPOSURE VARIABLES ---

     # Record COVID-19 infection date
    sgss_covid19_date=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        on_or_after="index_date",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    primary_care_covid19_date=patients.with_these_clinical_events(
        combine_codelists(
            covid_primary_care_code,
            covid_primary_care_positive_test,
            covid_primary_care_sequalae,
        ),
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    hospital_covid19_date=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    death_covid19_date=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    exp_confirmed_covid19_date=patients.minimum_of(
        "sgss_covid19_date","primary_care_covid19_date","hospital_covid19_date","death_covid19_date"
    ),

# --- DEFINE FOLLOW-UP VARIABLES

    # Record death date
    primary_care_death_date=patients.with_death_recorded_in_primary_care(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "exponential_increase",
        },
    ),
    ons_died_from_any_cause_date=patients.died_from_any_cause(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "exponential_increase",
        },
    ),
    death_date=patients.minimum_of(
        "primary_care_death_date", "ons_died_from_any_cause_date"
    ),
  
  ###  COVID vaccination
    # First covid vaccination date (first vaccine given on 8/12/2020 in the UK)
    covid19_vaccination_date1=patients.with_tpp_vaccination_record(
        # code for TPP only, when using patients.with_tpp_vaccination_record() function
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="2020-12-07",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-08", "latest": "today"},
            "incidence": 0.7
        },
    ),
    # Second covid vaccination date (first second dose reported on 29/12/2020 in the UK)
    covid19_vaccination_date2=patients.with_tpp_vaccination_record(
        # code for TPP only, when using patients.with_tpp_vaccination_record() function
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="covid19_vaccination_date1 + 14 days",  # Allowing for less days between 2 vaccination dates
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-29", "latest": "today"},
            "incidence": 0.6
        },
    ),

# --- DEFINE COVARIATES ---

  ### Sex
  cov_sex = patients.sex(
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }
  ),

  ### Age
  cov_age = patients.age_as_of(
    "index_date",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),

  ### Ethnicity 
        cov_ethnicity=patients.categorised_as(
        helpers.generate_ethnicity_dictionary(16),
        cov_ethnicity_sus=patients.with_ethnicity_from_sus(
            returning="group_16", use_most_frequent_code=True
        ),
        cov_ethnicity_gp_opensafely=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_16,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_opensafely_date=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_16,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis_date=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        return_expectations=helpers.generate_universal_expectations(16),
    ),

  ###deprivation
  cov_deprivation=patients.categorised_as(
        helpers.generate_deprivation_ntile_dictionary(10),
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations=helpers.generate_universal_expectations(10),
    ),

  ###Region
  cov_region=patients.registered_practice_as_of(
            "index_date",
            returning="nuts1_region_name",
            return_expectations={
                "rate": "universal",
                "category": {
                    "ratios": {
                        "North East": 0.1,
                        "North West": 0.1,
                        "Yorkshire and The Humber": 0.1,
                        "East Midlands": 0.1,
                        "West Midlands": 0.1,
                        "East": 0.1,
                        "London": 0.2,
                        "South East": 0.1,
                        "South West": 0.1,
                    },
                },
            },
        ),


#Medications

  ###Smoking status
    cov_smoking_status=patients.categorised_as(
        {
            "S": "most_recent_smoking_code = 'S'",
            "E": """
                 most_recent_smoking_code = 'E' OR (
                   most_recent_smoking_code = 'N' AND ever_smoked
                 )
            """,
            "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
            "M": "DEFAULT",
        },
        return_expectations={
            "category": {"ratios": {"S": 0.6, "E": 0.1, "N": 0.2, "M": 0.1}}
        },
        most_recent_smoking_code=patients.with_these_clinical_events(
            smoking_clear,
            find_last_match_in_period=True,
            on_or_before="index_date",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(smoking_clear, include=["S", "E"]),
            on_or_before="index_date",
        ),
    ),

  ### BMI (to derive obesity binary variable)
  cov_bmi = patients.with_these_clinical_events(
    bmi_codes,
    returning = "numeric_value",
    ignore_missing_values = True,
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {
      "float": {"distribution": "normal", "mean": 25, "stddev": 5},
    },
  ),

  ###No. primary care consultation in year prior to index date
    cov_n_disorder=patients.with_gp_consultations(
        between=["index_date - 12 months", "index_date"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 10, "stddev": 3},
            "incidence": 1,
        },
    ),

#Diabetes
#Any mental illness
#Cancer
#Chronic Obstructive Pulmonary Disorder

  ###Liver disease (primary care)
  chronic_liver_disease = patients.with_these_clinical_events(
    chronis_liver_disease_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.01},
  ),

  #Major surgery
#Hypertension
#Dementia
    dementia=patients.with_these_clinical_events(
        dementia_codes,
        on_or_before="index_date",
        return_expectations={"incidence": 0.05},
    ),
#Antiplatelet medication
#Lipid lowering medication
#Anticoagulation medication
#Combined oral contraceptive pill
#Blood pressure lowering medication
#Hormone replacement therapy



)
