
# This script combines the cleaned datasets from 0.X scripts (containing 
# time-varying information) with info on dementia and other time-fixed 
# characteristics and applies the inclusion/exclusion criteria of the analysis. 
# 
# It constructs the final long-format analytic dataset, where
# each subject has the same number of rows as the number of years of followup.

# set up libraries and paths ----------------------------------------------

library(tidyverse)
library(haven)
library(here)
library(openxlsx)
library(survival)

source(here("01_R", "path.R"))

# Load data -------------------------------------------------------------

data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

# main tte data
full_data <- read_sas(paste0(data_folder, "analysis_data_tables/", 
                             "aa_adrd_cardiometabolic_tte.sas7bdat"))

# additional EHR variables
tte_data_add <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/", "tte_data_add.RDS")
)

# BMI, BP, and lipids
bmi_followup <- readRDS(paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                               "bmi_followup.RDS"))
bp_followup <- readRDS(paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                              "bp_followup.RDS"))
lipids_followup <- readRDS(paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                                  "lipids_followup.RDS"))

# stroke variables derived from new stroke defn 
stroke <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "stroke.RDS")) %>% 
  select(subjid, isd_acvd, hstroke_noties, combined_stroke, combined_isd_hstroke) %>% 
  rename(SUBJID = subjid)

# these are time at first stroke, with four stroke defns: 
# isd_acvd: ischemic stroke, including ACVD
# hstroke_noties: hemorrhagic stroke
# combined_stroke: ischemic stroke (with ACVD) or hemorrhagic stroke, no TIA
# combined_isd_hstroke: ischemic stroke (NO ACVD) or hemorrhagic stroke, no TIA

# combined_stroke is going to be the main defn of stroke
# isd_acvd is used in secondary analysis
  

# Merge and recode tte data with all derived EHR variables  ------------------------

full_data <- tte_data_add %>% 
  select(-DIAB_REG_AGE) %>% 
  left_join(
    bmi_followup %>% filter(AGE == SURVEY_AGE_r) %>% select(-AGE, -SURVEY_AGE_r),
    by = "SUBJID"
  ) %>% 
  left_join(
    bp_followup %>% filter(AGE_yr == SURVEY_AGE_r) %>% select(-AGE_yr, -SURVEY_AGE_r),
    by = "SUBJID"
  ) %>% 
  left_join(
    lipids_followup %>% filter(LAB_AGE_yr == SURVEY_AGE_r) %>% select(-LAB_AGE_yr, -SURVEY_AGE_r),
    by = "SUBJID"
  ) %>% 
  select(-SURVEY_AGE) %>% 
  right_join(full_data, by = "SUBJID")

full_data <- full_data %>% 
  select(- contains("STROKE")) %>% # remove old stroke vars in the tte data
  left_join(., stroke, by = "SUBJID")


# update tte variables to end FU at 90 
full_data <- full_data %>% 
  mutate(
    end_age = case_when(
      MAIN_DEM_V1_END_TYPE %in% c("ADMIN CENSORED", "END OF MEMBERSHIP") ~
        MAIN_DEM_V1_END_AGE, 
      MAIN_DEM_V1_END_TYPE %in% c("DEMENTIA", "DEATH", "CENSORED 90+") & 
        MAIN_DEM_V1_END_AGE > 90 ~ 90, 
      TRUE ~ MAIN_DEM_V1_END_AGE
    ), 
    end_type = case_when(
      MAIN_DEM_V1_END_TYPE %in% c("ADMIN CENSORED", "END OF MEMBERSHIP") ~
        MAIN_DEM_V1_END_TYPE, 
      MAIN_DEM_V1_END_TYPE %in% c("DEMENTIA", "DEATH", "CENSORED 90+") & 
        MAIN_DEM_V1_END_AGE > 90 ~ "ADMIN CENSORED", 
      TRUE ~ MAIN_DEM_V1_END_TYPE
    )
  ) 

# check
# with(full_data, table(MAIN_DEM_V1_END_TYPE, end_type, useNA = "ifany"))

full_data <- full_data %>% 
  mutate(
    # round age at events by yr
    across(
      c(SURVEY_AGE, end_age, isd_acvd, hstroke_noties, combined_stroke, combined_isd_hstroke), 
      round, .names = "{.col}_r"
    ), 
    max_FU_yr = end_age_r - SURVEY_AGE_r, 
    # create event indicators
    eventEndMem = ifelse(end_type == "END OF MEMBERSHIP", 1, 0),
    eventDeath = ifelse(end_type == "DEATH", 1, 0), 
    eventDem = ifelse(end_type == "DEMENTIA", 1, 0),
    start = -0.1
  )

# Inclusion criteria - Flowchart ------------------------------------------

# Number in the entire study
nrow(full_data) # n = 184,929

# Number after excluding missing ethnicity: n = 165,705
data <- full_data %>% filter(!is.na(ETHNICITY_REV))
nrow(data)

# Number after subsetting to subjects 60-89 yo at survey: 
data <- data %>% 
  filter(SURVEY_AGE_r <= 89) # n = 165,150
# filter(SURVEY_AGE <= 89) # slightly more restrictive: n = 164,489
nrow(data)

# Question: 
# Which version should we use? 
# The more restrictive case would make sure that subjects are in fact 
# followed for at least one year until 90, while if we use the rounded case,
# some subjects might start follow up at 89.4 yo and only followed for 0.6 yrs
# at most. 

# 8/31: decided to use rounded age for all criteria to be consistent


# Number after removing prevalent dementia and 
# subjects whose FU is zero because of rounding: n = 157,658
data <- data %>% filter(MAIN_DEM_V1_SAMPLE == 1, max_FU_yr != 0)
nrow(data)

# remove prevalent stroke cases (any defn)
data <- data %>% 
  mutate(
    any_stroke_r = pmin(combined_stroke_r, isd_acvd_r, 
                        hstroke_noties_r, combined_isd_hstroke_r, na.rm = TRUE)
  ) %>% 
  filter(is.na(any_stroke_r) | any_stroke_r > SURVEY_AGE_r) %>% 
  select(-any_stroke_r)

nrow(data) # n = 150,184 eligible subjects remain

# save baseline data for final eligible subjects
# saveRDS(
#   data,
#   paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#          "baseline_tte_data_rounded_by_yr.RDS")
# )

# alternative order when applying exclusion steps
full_data %>% 
  # n = 174671
  mutate(
    any_stroke_r = pmin(combined_stroke_r, isd_acvd_r, 
                        hstroke_noties_r, combined_isd_hstroke_r, na.rm = TRUE)
  ) %>% 
  filter(is.na(any_stroke_r) | any_stroke_r > SURVEY_AGE_r) %>% 
  # n = 167017
  filter(MAIN_DEM_V1_SAMPLE == 1, max_FU_yr != 0) %>%
  # this steps does not additionally remove anyone probably because of max_FU_yr restriction
  filter(SURVEY_AGE_r <= 89) %>%
  # n = 150184
  filter(!is.na(ETHNICITY_REV)) %>%
  nrow()

## Table e4 counts by ethnicity --------------------------------------------

count_data <- full_data %>% 
  filter(!is.na(ETHNICITY_REV)) %>% 
  mutate(
    ETHNICITY_REV = factor(
      ETHNICITY_REV, levels = c(9, 2, 5, 3, 1, 4, 6, 8, 7, 10),
      labels = c("White",
                 "Chinese", "Filipino", "Japanese",
                 "South Asian", "Korean", "Vietnamese", "Pacific Islander",
                 "Other SE Asian", "Multiple")
    )
  )

count_ouput <- count_data %>% 
  # subset to analytic sample and derive the inclusion variable
  mutate(
    any_stroke_r = pmin(combined_stroke_r, isd_acvd_r, 
                        hstroke_noties_r, combined_isd_hstroke_r, na.rm = TRUE)
  ) %>% 
  filter(is.na(any_stroke_r) | any_stroke_r > SURVEY_AGE_r) %>% 
  filter(MAIN_DEM_V1_SAMPLE == 1, max_FU_yr != 0) %>% 
  filter(SURVEY_AGE_r <= 89) %>% 
  mutate(included = 1) %>% 
  select(SUBJID, included) %>% 
  right_join(count_data, by = "SUBJID") %>% 
  mutate(included = ifelse(is.na(included), 0, included)) %>% 
  group_by(ETHNICITY_REV) %>% 
  summarise(
    KPNC_total = n(), 
    n_excluded = n() - sum(included), 
    n_included = sum(included),
    n_dementia = sum(included == 1 & end_type == "DEMENTIA"), 
    n_death = sum(included == 1 & end_type == "DEATH"),
    n_endmem = sum(included == 1 & end_type == "END OF MEMBERSHIP")
  ) %>% 
  ungroup()

# write.xlsx(
#   count_ouput,
#   here("02_outputs", "table_e4_participants_included.xlsx")
# )


## secondary analysis data -------------------------------------------------

#!!! This no longer applies !!!

# # remove prevalent ishcemic stroke cases 
# data_isd <- data %>% 
#   filter(is.na(isd_acvd_r) | isd_acvd_r > SURVEY_AGE_r)
# nrow(data_isd) # n = 150,339 eligible subjects remain
# 
# # there is a nuance here: whether we remove prevalent ischemic stroke 
# # before removing prevalent combined stroke or after? 
# # the latter is easier to deal with, because data for secondary analysis
# # will be a subset of the data for main analysis.
# # the former would mean reconstructing and imputing long data separately.
# 
# # right now I'm choosing the easier option
# # save the subjects that are excluded from secondary analysis
# 
# id_remove_isd <- anti_join(data, data_isd) %>% pull(SUBJID)
# # saveRDS(
# #   id_remove_isd,
# #   paste0(data_folder, "aa_stroke_dementia/final_datasets/",
# #          "ids_exclude_isd_analysis.RDS")
# # )



# Set up time-varying long tte dataset -----------------------------------

tte_vars <- data %>% 
  select(SUBJID, 
         SURVEY_AGE_r, end_age_r, end_type, max_FU_yr, 
         starts_with("event"), start) %>% 
  arrange(SUBJID)

max(tte_vars$max_FU_yr) # subjects are followed for up to 18 yrs

## this is the main data we will merge additional info onto 
long_tte_data <- survSplit(
  data = tte_vars, 
  cut = 0:max(tte_vars$max_FU_yr), 
  start = "start", 
  end = "max_FU_yr", 
  event = "eventEndMem"
) %>% 
  rename(FU_YR = max_FU_yr)

## other long tte indicators (death, dem)
long_Death_data <- survSplit(
  data = tte_vars, 
  cut = 0:max(tte_vars$max_FU_yr), 
  start = "start", 
  end = "max_FU_yr", 
  event = "eventDeath"
) %>% 
  rename(FU_YR = max_FU_yr)

long_Dem_data <- survSplit(
  data = tte_vars, 
  cut = 0:max(tte_vars$max_FU_yr), 
  start = "start", 
  end = "max_FU_yr", 
  event = "eventDem"
) %>% 
  rename(FU_YR = max_FU_yr)

# merge in the event indicators
long_tte_data$eventDeath <- long_Death_data$eventDeath
long_tte_data$eventDem <- long_Dem_data$eventDem

long_tte_data <- long_tte_data %>% 
  mutate(
    # if censored due to end of membership, then death and dem status are set to missing
    eventDeath = ifelse(eventEndMem == 1, NA, eventDeath),
    eventDem = ifelse(eventEndMem == 1, NA, eventDem),
    # if died at end of followup, then dem status is set to missing
    eventDem = ifelse(eventDeath == 1, NA, eventDem)
  )


# Add stroke to long tte data ---------------------------------------------

long_tte_data <- data %>% 
  select(SUBJID, 
         isd_acvd_r, hstroke_noties_r, combined_stroke_r, combined_isd_hstroke_r) %>% 
  right_join(long_tte_data, by = "SUBJID") %>%
  arrange(SUBJID, FU_YR) %>% 
  mutate(
    stroke_isd_acvd = case_when(
      is.na(isd_acvd_r) ~ 0, 
      isd_acvd_r == 90 ~ 0, 
      isd_acvd_r <= SURVEY_AGE_r + FU_YR ~ 1, 
      TRUE ~ 0
    ), 
    stroke_hstroke = case_when(
      is.na(hstroke_noties_r) ~ 0, 
      hstroke_noties_r == 90 ~ 0, 
      hstroke_noties_r <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ),
    stroke_combined = case_when(
      is.na(combined_stroke_r) ~ 0, 
      combined_stroke_r == 90 ~ 0, 
      combined_stroke_r <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ), 
    stroke_combined_isd_hstroke = case_when(
      is.na(combined_isd_hstroke_r) ~ 0, 
      combined_isd_hstroke_r == 90 ~ 0, 
      combined_isd_hstroke_r <= SURVEY_AGE_r + FU_YR ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  select(-c(isd_acvd_r, hstroke_noties_r, combined_stroke_r, combined_isd_hstroke_r)) 


# Add BMI, BP, and lipids to long tte data --------------------------------

long_tte_data <- bmi_followup %>% 
  mutate(FU_YR = AGE - SURVEY_AGE_r) %>% 
  select(SUBJID, BMI, FU_YR) %>% 
  right_join(long_tte_data, by = c("SUBJID", "FU_YR"))

long_tte_data <- bp_followup %>% 
  mutate(FU_YR = AGE_yr - SURVEY_AGE_r) %>% 
  select(SUBJID, SBP, FU_YR) %>% 
  right_join(long_tte_data, by = c("SUBJID", "FU_YR"))

long_tte_data <- lipids_followup %>% 
  mutate(FU_YR = LAB_AGE_yr - SURVEY_AGE_r) %>% 
  select(SUBJID, HDL, LDL, TRIGL, TOT_CHOLES, FU_YR) %>% 
  right_join(long_tte_data, by = c("SUBJID", "FU_YR"))


# Add other EHR indicators to long tte data -------------------------------

names(long_tte_data)
names(tte_data_add)

## diabetes and hypertension ----

long_tte_data <- data %>%
  select(SUBJID, DIAB_REG_FLAG, DIAB_REG_AGE, FIRST_HTN_DX_AGE, HTN_DX_FLAG) %>% 
  mutate(across(c(DIAB_REG_AGE, FIRST_HTN_DX_AGE), round)) %>% 
  right_join(long_tte_data, by = "SUBJID", multiple = "all") %>%
  mutate(
    DIAB = case_when(
      DIAB_REG_FLAG == 0 ~ 0,
      DIAB_REG_AGE == 90 ~ 0, # 90+ censored
      DIAB_REG_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ),
    HTN = case_when(
      is.na(HTN_DX_FLAG) ~ 0,
      FIRST_HTN_DX_AGE == 90 ~ 0, 
      FIRST_HTN_DX_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    )
  )

long_tte_data <- long_tte_data %>% 
  select(-c(DIAB_REG_FLAG, DIAB_REG_AGE, FIRST_HTN_DX_AGE, HTN_DX_FLAG))

## acute MI, CHF, PVD ----

# possible to include
# (1) baseline flag for prev AMI and time varying flag for inc AMI
# or 
# (2) flag for first event, regardless of prev/inc AMI

# CHF and PVD are first Dx only

long_tte_data <- data %>% 
  select(
    SUBJID, 
    FIRST_PREV_AMI_FLAG, FIRST_INC_AMI_FLAG, FIRST_INC_AMI_AGE, 
    CHF_DX_AGE, PVD_DX_AGE
  ) %>% 
  mutate(across(ends_with("AGE"), round)) %>% 
  right_join(long_tte_data, by = "SUBJID", multiple = "all") %>% 
  mutate(
    PREV_AMI = FIRST_PREV_AMI_FLAG, 
    INC_AMI = case_when(
      FIRST_INC_AMI_FLAG == 0 ~ 0,
      # FIRST_INC_AMI_AGE == 90 ~ 0, 
      FIRST_INC_AMI_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ), 
    CHF = case_when(
      is.na(CHF_DX_AGE) ~ 0, 
      # CHF_DX_AGE == 90 ~ 0, 
      CHF_DX_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ), 
    PVD = case_when(
      is.na(PVD_DX_AGE) ~ 0, 
      # PVD_DX_AGE == 90 ~ 0, 
      PVD_DX_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    )
  )

long_tte_data <- long_tte_data %>% 
  select(-c(FIRST_PREV_AMI_FLAG, FIRST_INC_AMI_FLAG, FIRST_INC_AMI_AGE, 
            CHF_DX_AGE, PVD_DX_AGE))

## cancer, IHD, dislipidemia ----
long_tte_data <- data %>% 
  select(
    SUBJID, 
    CANCER_DX_AGE, IHD_DX_AGE, DYSLIP_DX_AGE
  ) %>% 
  mutate(across(ends_with("AGE"), round)) %>% 
  right_join(long_tte_data, by = "SUBJID") %>% 
  mutate(
    CANCER = case_when(
      is.na(CANCER_DX_AGE) ~ 0,
      CANCER_DX_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ), 
    IHD = case_when(
      is.na(IHD_DX_AGE) ~ 0,
      IHD_DX_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    ), 
    DYSLIP = case_when(
      is.na(DYSLIP_DX_AGE) ~ 0, 
      DYSLIP_DX_AGE <= SURVEY_AGE_r + FU_YR ~ 1,
      TRUE ~ 0
    )
  )

long_tte_data <- long_tte_data %>% 
  select(-c(CANCER_DX_AGE, IHD_DX_AGE, DYSLIP_DX_AGE))

# Merge in time-fixed covariates and save long tte dataset ----

unique(long_tte_data$SUBJID) %>% length()
nrow(data)

long_tte_data <- data %>% 
  select(
    SUBJID, FEMALE, ASIAN, ETHNICITY_REV, USABORN_REV, 
    EDUCATION_REV, EDU_GE_COLLEGE, MARITALSTATUS, GENERALHEALTH, SMOKING_STATUS,
    # auxiliary variables for imputation
    USABORNFATHER_REV, USABORNMOTHER_REV, 
    SR_BMI, INCOME, SIZEOFHH, INCOME_PP
  ) %>% 
  right_join(long_tte_data, by = "SUBJID")

long_tte_data <- long_tte_data %>% janitor::clean_names()

# saveRDS(long_tte_data,
#         paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                "long_tte_data_rounded_by_yr.RDS"))

