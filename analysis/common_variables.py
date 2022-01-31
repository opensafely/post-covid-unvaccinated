# Based on script developed by Alex Walker and Robin Park (https://github.com/opensafely/long-covid-sick-notes/blob/master/analysis/common_variables.py)

# Import statements

## Cohort extractor
from cohortextractor import (
    patients,
    codelist,
    filter_codes_by_category,
    combine_codelists,
    codelist_from_csv,
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Study definition helper
import study_def_helper_functions as helpers

# Define common variables function

def generate_common_variables(index_date_variable):

    dynamic_variables = dict(

    # Define exposures

    ## Date of positive SARS-COV-2 PCR antigen test
    tmp_exp_date_covid19_confirmed_sgss=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        on_or_after=f"{index_date_variable}",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    ## First COVID-19 code (diagnosis, positive test or sequalae) in primary care
    tmp_exp_date_covid19_confirmed_snomed=patients.with_these_clinical_events(
        combine_codelists(
            covid_primary_care_code,
            covid_primary_care_positive_test,
            covid_primary_care_sequalae,
        ),
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    ## Start date of episode with confirmed diagnosis in any position
    tmp_exp_date_covid19_confirmed_hes=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    ## Date of death with SARS-COV-2 infection listed as primary or underlying cause
    tmp_exp_date_covid19_confirmed_death=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ## Generate variable to identify first date of confirmed COVID
    exp_date_covid19_confirmed=patients.minimum_of(
        "tmp_exp_date_covid19_confirmed_sgss","tmp_exp_date_covid19_confirmed_snomed","tmp_exp_date_covid19_confirmed_hes","tmp_exp_date_covid19_confirmed_death"
    ),

    # Define outomes 

    ## Diabetes (date of first ever recording, count of number of records from each data source)
    ## Type 1
    ## Date of first ever recording
    ### Primary care
    tmp_out_date_dm_type1_snomed=patients.with_these_clinical_events(
        diabetes_type1_snomed,
        returning="date",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_dm_type1_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=diabetes_type1_icd10,
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),

## count of number of records
    ### Primary care
    tmp_out_count_dm_type1_snomed=patients.with_these_clinical_events(
        diabetes_type1_snomed,
        returning="return_number_of_matches_in_period",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
        },
    ),

    ### HES APC
    tmp_out_count_dm_type1_hes=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        with_these_diagnoses=diabetes_type1_icd10,
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
        },
    ),

    ## Type 2
    ## Date of first ever recording
    ### Primary care
    tmp_out_date_dm_type2_snomed=patients.with_these_clinical_events(
        diabetes_type2_snomed,
        returning="date",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_dm_type2_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=diabetes_type2_icd10,
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),

## count of number of records
    ### Primary care
    tmp_out_count_dm_type2_snomed=patients.with_these_clinical_events(
        diabetes_type2_snomed,
        returning="return_number_of_matches_in_period",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
        },
    ),

    ### HES APC
    tmp_out_count_dm_type2_hes=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        with_these_diagnoses=diabetes_type2_icd10,
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
        },
    ),

    ## Diabetes unspecified
    ## Date of first ever recording
    ### Primary care
    tmp_out_date_dm_other_snomed=patients.with_these_clinical_events(
        diabetes_other_snomed,
        returning="date",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),

## count of number of records
    ### Primary care
    tmp_out_count_dm_other_snomed=patients.with_these_clinical_events(
        diabetes_other_snomed,
        returning="return_number_of_matches_in_period",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
        },
    ),

    ## Gestational diabetes
    ## Date of first ever recording
    ### Primary care
    tmp_out_date_dm_gestational_snomed=patients.with_these_clinical_events(
        diabetes_gestational_snomed,
        returning="date",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),

    ## Diabetes diagnostic codes
    ## Date of first ever recording
    ### Primary care
    tmp_out_date_dm_diagnostic_snomed=patients.with_these_clinical_events(
        diabetes_diagnostic_snomed,
        returning="date",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),

## count of number of records
    ### Primary care
    tmp_out_count_dm_diagnostic_snomed=patients.with_these_clinical_events(
        diabetes_diagnostic_snomed,
        returning="return_number_of_matches_in_period",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
        },
    ),

 ## Variables needed to define diabetes
 ## last HbA1c measure
    temp_cov_num_latest_hba1c=patients.with_these_clinical_events(
    hba1c_codes,
    returning="numeric_value", 
    on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
    find_last_match_in_period=True
    ),

    temp_cov_date_latest_hba1c=patients.with_these_clinical_events(
    hba1c_codes,
    returning="date", 
    on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
    find_last_match_in_period=True
    return_expectations={
        "date": {"earliest": "1900-01-01", "latest" : "today"},
        "rate": "uniform",
        "incidence": 0.03,
        }
    ),

## Diabetes drugs
    temp_cov_date_dm_drugs_dmd=with_these_medications(
        diabetes_drugs_dmd,
        returning="date",
        on_or_after=f"{index_date_variable}" / on_or_before=f"{index_date}",
        find_first_date_in_period=True    
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        }
    ),
    

    # Define covariates 

    ## Age
    cov_num_age = patients.age_as_of(
        f"{index_date_variable}",
        return_expectations = {
        "rate": "universal",
        "int": {"distribution": "population_ages"},
        "incidence" : 0.001
        },
    ),

    ## Ethnicity 
    cov_cat_ethnicity=patients.categorised_as(
        helpers.generate_ethnicity_dictionary(6),
        cov_ethnicity_sus=patients.with_ethnicity_from_sus(
            returning="group_6", use_most_frequent_code=True
        ),
        cov_ethnicity_gp_opensafely=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_opensafely_date=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis_date=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        return_expectations=helpers.generate_universal_expectations(5,False),
    ),

    ## Deprivation
    cov_cat_deprivation=patients.categorised_as(
        helpers.generate_deprivation_ntile_dictionary(10),
        index_of_multiple_deprivation=patients.address_as_of(
            f"{index_date_variable}",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations=helpers.generate_universal_expectations(10,False),
    ),

    ## Region
    cov_cat_region=patients.registered_practice_as_of(
        f"{index_date_variable}",
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

    ## Smoking status
    cov_cat_smoking_status=patients.categorised_as(
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
            on_or_before=f"{index_date_variable}",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(smoking_clear, include=["S", "E"]),
            on_or_before=f"{index_date_variable}",
        ),
    ),

    ## Care home status
    cov_bin_carehome_status=patients.care_home_status_as_of(
        f"{index_date_variable}", 
        categorised_as={
            "Yes": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "Yes": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "Yes": "IsPotentialCareHome",
            "No": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"Yes": 0.30, "No": 0.70},},
        },
    ),

    ## Acute myocardial infarction
    ### Primary care
    tmp_cov_bin_ami_snomed=patients.with_these_clinical_events(
        ami_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_ami_prior_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=ami_prior_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    tmp_cov_bin_ami_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=ami_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_ami=patients.maximum_of(
        "tmp_cov_bin_ami_snomed", "tmp_cov_bin_ami_prior_hes", "tmp_cov_bin_ami_hes",
    ),

    ## All stroke
    ### Primary care
    tmp_cov_bin_stroke_isch_snomed=patients.with_these_clinical_events(
        stroke_isch_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    tmp_cov_bin_stroke_sah_hs_snomed=patients.with_these_clinical_events(
        stroke_sah_hs_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_stroke_isch_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=stroke_isch_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    tmp_cov_bin_stroke_sah_hs_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=stroke_sah_hs_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_all_stroke=patients.maximum_of(
        "tmp_cov_bin_stroke_isch_hes", "tmp_cov_bin_stroke_isch_snomed", "tmp_cov_bin_stroke_sah_hs_hes", "tmp_cov_bin_stroke_sah_hs_snomed",
    ),

    ## Other arterial embolism  
    cov_bin_other_arterial_embolism=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=other_arterial_embolism_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    
    ## Venous thrombolism events
    ### Primary care
    tmp_cov_bin_vte_snomed=patients.with_these_clinical_events(
        all_vte_codes_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_vte_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=all_vte_codes_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_vte=patients.maximum_of(
        "tmp_cov_bin_vte_snomed", "tmp_cov_bin_vte_hes",
    ),

    ## Heart failure
    ### Primary care
    tmp_cov_bin_hf_snomed=patients.with_these_clinical_events(
        hf_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_hf_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=hf_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_hf=patients.maximum_of(
        "tmp_cov_bin_hf_snomed", "tmp_cov_bin_hf_hes",
    ),

    ## Angina
    ### Primary care
    tmp_cov_bin_angina_snomed=patients.with_these_clinical_events(
        angina_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_angina_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=angina_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_angina=patients.maximum_of(
        "tmp_cov_bin_angina_snomed", "tmp_cov_bin_angina_hes",
    ),

    ## Dementia
    ### Primary care
    tmp_cov_bin_dementia_snomed=patients.with_these_clinical_events(
        dementia_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC (Hospital Episode Statistics Admitted Patient Care)
    tmp_cov_bin_dementia_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=dementia_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Primary care - vascular
    tmp_cov_bin_dementia_vascular_snomed=patients.with_these_clinical_events(
        dementia_vascular_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC - vascular
    tmp_cov_bin_dementia_vascular_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=dementia_vascular_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_dementia=patients.maximum_of(
        "tmp_cov_bin_dementia_snomed", "tmp_cov_bin_dementia_hes", "tmp_cov_bin_dementia_vascular_snomed", "tmp_cov_bin_dementia_vascular_hes",
    ),

    ## Liver disease
     ### Primary care
    tmp_cov_bin_liver_disease_snomed=patients.with_these_clinical_events(
        liver_disease_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_liver_disease_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=liver_disease_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_liver_disease=patients.maximum_of(
        "tmp_cov_bin_liver_disease_snomed", "tmp_cov_bin_liver_disease_hes",
    ),

    ## Chronic kidney disease
    ### Primary care
    tmp_cov_bin_chronic_kidney_disease_snomed=patients.with_these_clinical_events(
        ckd_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_chronic_kidney_disease_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=ckd_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_chronic_kidney_disease=patients.maximum_of(
        "tmp_cov_bin_chronic_kidney_disease_snomed", "tmp_cov_bin_chronic_kidney_disease_hes",
    ),

    ## Cancer
    ### Primary care
    tmp_cov_bin_cancer_snomed=patients.with_these_clinical_events(
        cancer_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_cancer_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=cancer_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_cancer=patients.maximum_of(
        "tmp_cov_bin_cancer_snomed", "tmp_cov_bin_cancer_hes",
    ),

    ## Hypertension
    ### Primary care
    tmp_cov_bin_hypertension_snomed=patients.with_these_clinical_events(
        hypertension_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_hypertension_hes=patients.admitted_to_hospital(
       returning='binary_flag',
       with_these_diagnoses=hypertension_icd10,
       on_or_before=f"{index_date_variable}",
       return_expectations={"incidence": 0.01},
    ),
    ### DMD
    tmp_cov_bin_hypertension_drugs_dmd=patients.with_these_clinical_events(
        hypertension_drugs_dmd,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_hypertension=patients.maximum_of(
        "tmp_cov_bin_hypertension_snomed", "tmp_cov_bin_hypertension_hes", "tmp_cov_bin_hypertension_drugs_dmd",
    ),

    ## Obesity
    ### Primary care
    tmp_cov_bin_obesity_snomed=patients.with_these_clinical_events(
        bmi_obesity_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_obesity_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=bmi_obesity_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_obesity=patients.maximum_of(
        "tmp_cov_bin_obesity_snomed", "tmp_cov_bin_obesity_hes",
    ),

    ## BMI
    cov_bmi=() 

    ## Depresssion
    ### Primary care
    tmp_cov_bin_depression_snomed=patients.with_these_clinical_events(
        depression_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_depression_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=depression_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_depression=patients.maximum_of(
        "tmp_cov_bin_depression_snomed", "tmp_cov_bin_depression_hes",
    ),

    ## Chronic obstructive pulmonary disease
    ### Primary care
    tmp_cov_bin_chronic_obstructive_pulmonary_disease_snomed=patients.with_these_clinical_events(
        copd_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### HES APC
    tmp_cov_bin_chronic_obstructive_pulmonary_disease_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses= copd_icd10,
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
    ### Combined
    cov_bin_chronic_obstructive_pulmonary_disease=patients.maximum_of(
        "tmp_cov_bin_chronic_obstructive_pulmonary_disease_snomed", "tmp_cov_bin_chronic_obstructive_pulmonary_disease_hes",
    ),

    ## Lipid medications
    cov_bin_lipid_medications_dmd=patients.with_these_clinical_events(
        lipid_lowering_dmd,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),

    ## Antiplatelet_medications
    cov_bin_antiplatelet_medications=patients.with_these_clinical_events(
        antiplatelet_dmd,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),

    ## Anticoagulation_medications
    cov_bin_anticoagulation_medications=patients.with_these_clinical_events(
        anticoagulant_dmd, 
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),
   
    ## Combined oral contraceptive pill
    ### dmd: dictionary of medicines and devices
    cov_bin_combined_oral_contraceptive_pill=patients.with_these_clinical_events(
        cocp_dmd, 
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),

    ## Hormone replacement therapy
    cov_bin_hormone_replacement_therapy=patients.with_these_clinical_events(
        hrt_dmd, 
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        return_expectations={"incidence": 0.01},
    ),

    # Define subgroups (for variables that don't have a corresponding covariate only)

    ## COVID-19 severity
    sub_date_covid19_hospital=patients.admitted_to_hospital(
        with_these_primary_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after="exp_date_covid19_confirmed",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),

    )
    return dynamic_variables
