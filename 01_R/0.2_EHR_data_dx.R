
# This script derives variables from EHR Dx of cardiometabolic conditions
# and conditions that are predictive of mortality. 

# For a given condition, if all Dx's are requested, then flags and ages for 
# the first prevalent and first incident events are derived. 
# If only the first Dx is requested, then age at Dx is derived, along with
# a flag for whether the Dx is prevalent (before baseline). 
# Flags for whether an event occurs at age 90+ are also derived where 
# possible. 

# set up libraries and paths ----------------------------------------------

library(tidyverse)
library(lubridate)
library(haven)
library(here)
library(table1)

source(here("01_R", "path.R"))

# load data -------------------------------------------------------------
data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

tte_data <- read_sas(paste0(data_folder, "analysis_data_tables/",
                            "aa_adrd_cardiometabolic_tte.sas7bdat"))
# we mainly want SUBJID and SURVEY_AGE to derive other variables
tte_data_survey_age <- tte_data %>% 
  mutate(SURVEY_AGE_r = round(SURVEY_AGE)) %>% 
  select(SUBJID, SURVEY_AGE, SURVEY_AGE_r)

# diabetes ----

# most diabetes-related variables are already derived
# just adding a few simple flags here

tte_data_add <- tte_data %>% 
  select(SUBJID, SURVEY_AGE, contains("DIAB")) %>% 
  mutate(
    # prevalent diabetes at survey
    PREV_DIAB_FLAG = case_when(
      DIAB_REG_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    # whether entry into diabetes registry is 90+ censored
    DIAB_REG_AGE_90plus_FLAG = case_when(
      DIAB_REG_AGE == 90 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select(SUBJID, SURVEY_AGE, 
         DIAB_REG_AGE, PREV_DIAB_FLAG, DIAB_REG_AGE_90plus_FLAG)

# acute MI ----
## this is derived the same way as the stroke variables that Taylor did 
## i.e. flag and age for first prevalent and first incident events
## this is the case for the rest of the EHR variables in this script

ami_hypogly <- read_sas(paste0(data_folder, 
                               "raw_data_tables/Membership_EHR_mortality_data/",
                               "c10049_all_obs_dx20220531.sas7bdat"))

# there could be multiple ami records for each subject 

ami_data <- ami_hypogly %>% 
  # use AMI indicators only
  filter(AMI == 1) %>% select(- HYPOGLY) %>% 
  mutate(
    SUBJID = as.numeric(SUBJID),
    # calculate age at event 
    AGE = ifelse(
      as.Date(DX_DATE) == "1600-01-01", 90, 
      interval(as.Date("1900-01-01"), as.Date(DX_DATE)) / years(1))
  ) %>% 
  # remove 90+ censored events
  filter(as.Date(DX_DATE) != "1600-01-01") %>% 
  arrange(SUBJID, AGE) %>% 
  left_join(tte_data_survey_age, by = "SUBJID") %>% 
  # subset to subjects in the analytical data
  filter(SUBJID %in% tte_data$SUBJID) %>% 
  mutate(PREV_AMI_FLAG = ifelse(AGE < SURVEY_AGE, 1, 0)) %>% 
  group_by(SUBJID, PREV_AMI_FLAG) %>% 
  # slices the earliest events, one from prevalent dx's, one from incident dx's,
  slice_head() %>% # since we have arranged by age already
  ungroup()

ami_data_wide <- ami_data %>% 
  select(SUBJID, AGE, PREV_AMI_FLAG) %>% 
  pivot_wider(
    names_from = PREV_AMI_FLAG, 
    values_from = AGE, 
    names_prefix = "FIRST_AMI_AGE_"
  ) %>% 
  rename(
    FIRST_PREV_AMI_AGE = FIRST_AMI_AGE_1,
    FIRST_INC_AMI_AGE = FIRST_AMI_AGE_0
  ) 

tte_data_add <- ami_data_wide %>% 
  right_join(tte_data_add, by = "SUBJID") %>% 
  mutate(
    FIRST_PREV_AMI_FLAG = ifelse(is.na(FIRST_PREV_AMI_AGE), 0, 1), 
    FIRST_INC_AMI_FLAG = ifelse(is.na(FIRST_INC_AMI_AGE), 0, 1), 
    FIRST_INC_AMI_90plus_FLAG = case_when(
      FIRST_INC_AMI_AGE == 90 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  relocate(
    starts_with("FIRST_PREV_AMI"), starts_with("FIRST_INC_AMI"), 
    .after = last_col()
  )

# hypertension ----
# we requested the first recorded hypertension Dx date if available
# so each subject only has one record
# htn_data <- read_sas(paste0(data_folder, 
#                             "raw_data_tables/Membership_EHR_mortality_data/",
#                             "c10049_htn.sas7bdat"))

# I did not use the raw dataset since Taylor already derived FIRST_HTN_DX_AGE

tte_data_add <- tte_data %>% 
  mutate(
    PREV_HTN_FLAG = case_when(
      FIRST_HTN_DX_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    HTN_DX_90plus_FLAG = case_when(
      FIRST_HTN_DX_AGE == 90 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select(SUBJID, PREV_HTN_FLAG, HTN_DX_90plus_FLAG) %>% 
  right_join(tte_data_add, by = "SUBJID") %>% 
  relocate(contains("HTN"), .after = last_col())

# CHF and PVD ----
chf_pvd <- read_sas(paste0(data_folder, 
                           "raw_data_tables/Membership_EHR_mortality_data/",
                           "c10049_cvd20220531.sas7bdat")) 

# we have: 
# first recorded Dx for congestive heart failure (CHF)
# first recorded Dx for peripheral vascular disease (PVD)

# derivation is similar to hypertension above

tte_data_add <- chf_pvd %>% 
  select(SUBJID, contains("CHF"), contains("PVD")) %>% 
  mutate(
    SUBJID = as.numeric(SUBJID),
    # calculate age at event 
    CHF_DX_AGE = ifelse(
      as.Date(FIRST_CHF_DX_DATE) == "1600-01-01", NA, #90
      interval(as.Date("1900-01-01"), as.Date(FIRST_CHF_DX_DATE)) / years(1)),
    PVD_DX_AGE = ifelse(
      as.Date(FIRST_PVD_DX_DATE) == "1600-01-01", NA, #90
      interval(as.Date("1900-01-01"), as.Date(FIRST_PVD_DX_DATE)) / years(1))
  ) %>% 
  select(SUBJID, CHF_DX_AGE, PVD_DX_AGE) %>% 
  right_join(tte_data_add, by = "SUBJID") %>% 
  mutate(
    PREV_CHF_FLAG = case_when(
      CHF_DX_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    # CHF_DX_90plus_FLAG = case_when(
    #   CHF_DX_AGE == 90 ~ 1,
    #   TRUE ~ 0
    # ), 
    PREV_PVD_FLAG = case_when(
      PVD_DX_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    # PVD_DX_90plus_FLAG = case_when(
    #   PVD_DX_AGE == 90 ~ 1,
    #   TRUE ~ 0
    # )
  ) %>% 
  relocate(contains("CHF"), contains("PVD"), .after = last_col()) 


# IHD and cancer ----

# we requested: 
# first recorded Dx for cancer, and
# first recorded Dx for ischemic heart disease (IHD) 

cvd_new <- read_sas(paste0(data_folder, 
                           "raw_data_tables/Membership_EHR_mortality_data/",
                           "c10049_cvd_20230630.sas7bdat")) 

# each subject has at most 1 row
length(unique(cvd_new$SUBJID)) == nrow(cvd_new)
# derive flag for prevalent Dx and age at first Dx

# check all subjects in our tte data are in the new CVD data: yes
# (not sure why the new CVD data has about 4000 more subjects though)
sum(tte_data$SUBJID %in% cvd_new$SUBJID) == nrow(tte_data)

tte_data_add <- cvd_new %>% 
  mutate(
    SUBJID = as.numeric(SUBJID),
    # calculate age at event 
    CANCER_DX_AGE = ifelse(
      as.Date(FIRST_CANCER_DX_DATE) == "1600-01-01", NA, 
      interval(as.Date("1900-01-01"), as.Date(FIRST_CANCER_DX_DATE)) / years(1)),
    IHD_DX_AGE = ifelse(
      as.Date(FIRST_IHD_DX_DATE) == "1600-01-01", NA, 
      interval(as.Date("1900-01-01"), as.Date(FIRST_IHD_DX_DATE)) / years(1))
  ) %>%
  select(SUBJID, CANCER_DX_AGE, IHD_DX_AGE) %>% 
  right_join(tte_data_add, by = "SUBJID") %>% 
  mutate(
    PREV_CANCER_FLAG = case_when(
      CANCER_DX_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    # CANCER_DX_90plus_FLAG = case_when(
    #   CANCER_DX_AGE == 90 ~ 1,
    #   TRUE ~ 0
    # ), 
    PREV_IHD_FLAG = case_when(
      IHD_DX_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    # IHD_DX_90plus_FLAG = case_when(
    #   IHD_DX_AGE == 90 ~ 1,
    #   TRUE ~ 0
    # )
  ) %>% 
  relocate(contains("CANCER"), contains("IHD"), .after = last_col()) 

# dyslipidemia ----

# we requested: 
# first recorded Dx for dyslipidemia

dyslip <- read_sas(paste0(data_folder, 
                          "raw_data_tables/Membership_EHR_mortality_data/",
                          "c10049_dyslip_20230630.sas7bdat")) 

# each subject has at most 1 row
length(unique(dyslip$SUBJID)) == nrow(dyslip)
# derive flag for prevalent Dx and age at first Dx

# check all subjects in our tte data are in the dislipidemia data: yes
# (not sure why the new CVD data has about 4000 more subjects though)
sum(tte_data$SUBJID %in% dyslip$SUBJID) == nrow(tte_data)

tte_data_add <- dyslip %>% 
  mutate(
    SUBJID = as.numeric(SUBJID),
    # calculate age at event 
    DYSLIP_DX_AGE = ifelse(
      as.Date(FIRST_DYSLIP_DX_DATE) == "1600-01-01", NA, 
      interval(as.Date("1900-01-01"), as.Date(FIRST_DYSLIP_DX_DATE)) / years(1))
  ) %>% 
  select(SUBJID, DYSLIP_DX_AGE) %>% 
  right_join(tte_data_add, by = "SUBJID") %>% 
  mutate(
    PREV_DYSLIP_FLAG = case_when(
      DYSLIP_DX_AGE < SURVEY_AGE ~ 1,
      TRUE ~ 0
    ), 
    # DYSLIP_DX_90plus_FLAG = case_when(
    #   DYSLIP_DX_AGE == 90 ~ 1,
    #   TRUE ~ 0
    # )
  ) %>% 
  relocate(contains("DYSLIP"), .after = last_col())

# save the EHR dx indicators ----

# saveRDS(
#   tte_data_add,
#   paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#          "tte_data_add.RDS")
# )

