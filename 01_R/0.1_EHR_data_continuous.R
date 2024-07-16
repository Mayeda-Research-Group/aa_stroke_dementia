
# This script prepares yearly median summaries of BMI, blood pressure, and 
# lipid labs for each patient starting from baseline (survey age). 
# BMI data was given in the form of yearly median of height and weight, which
# was used to calculated BMI. 
# All measurements (instead of yearly medians) of blood pressure and lipid labs 
# were given, which was then summarized. 

# Age at measurement was rounded by year, in line with the decision to round
# time-to-event variables by year as well. 

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
  select(SUBJID, SURVEY_AGE_r)

n_summary <- tte_data %>% 
  mutate(
    SURVEY_AGE_r = round(SURVEY_AGE), 
    END_AGE_r = round(MAIN_DEM_V1_END_AGE)
  ) %>% 
  select(SUBJID, ETHNICITY_REV, SURVEY_AGE_r, END_AGE_r) %>% 
  right_join(
    expand_grid(
      SUBJID = unique(n_summary$SUBJID),
      AGE_yr = 60:89
    ), 
    by = "SUBJID"
  ) %>%
  filter(AGE_yr >= SURVEY_AGE_r, AGE_yr <= END_AGE_r)



# height, weight, BMI -----------------------------------------------------

# This section derives baseline and time-varying BMI from EHR, 
# mainly for the stroke and dementia project, and I imagine this derivation 
# will be modified as the project goes along. 
# Not sure if we want to include this in the dataset that we send out. 

# ht and wt data requested are medians in each year of age 
ht_wt_data <- read_sas(paste0(data_folder, 
                              "raw_data_tables/Membership_EHR_mortality_data/",
                              "c10049_ht_wt.sas7bdat"))

length(unique(ht_wt_data$SUBJID)) # n = 191024

# filter for subjects that are in the tte dataset 
ht_wt_data <- ht_wt_data %>% filter(SUBJID %in% tte_data$SUBJID)
length(unique(ht_wt_data$SUBJID)) # n = 181498

# there are 3431 subjects with no ht or wt data from EHR
# we will impute these values, with the help of self-reported BMI info
nrow(tte_data) - length(unique(ht_wt_data$SUBJID)) 

bmi_data <- ht_wt_data %>% 
  mutate(SUBJID = as.numeric(SUBJID)) %>% 
  arrange(SUBJID, AGE) %>% 
  mutate(BMI = WT_MEDIAN / HT_MEDIAN^2 * 703) %>% 
  filter(!is.na(BMI)) %>% 
  select(SUBJID, AGE, BMI) 

# ht and wt data are also 90+ censored
# i.e. measurements taken at or after age 90 are not distinguishable 
# bmi_data %>% 
#   group_by(SUBJID, AGE) %>% 
#   mutate(n = n()) %>% 
#   filter(n > 1) %>% View()

# we will discard info 90+ 
bmi_data <- bmi_data %>% filter(AGE != 90)

nrow(bmi_data) # n = 1673185
bmi_data %>% distinct(SUBJID, AGE) %>% nrow() # n = 1673185
# there isn't more than 1 obs for each yr for each subject-- good!

# merge in survey age rounded by yr
bmi_data <- bmi_data %>% left_join(tte_data_survey_age, by = "SUBJID")

## baseline ----
bmi_baseline <- bmi_data %>% filter(AGE == SURVEY_AGE_r)

# tte_add will be the dataset containing the additional baseline EHR variables,
# to be merged with the analytical tte dataset
tte_add <- bmi_baseline %>% select(-SURVEY_AGE_r) %>% 
  right_join(tte_data_survey_age, by = "SUBJID")

## followup ----
bmi_followup <- bmi_data %>% filter(AGE >= SURVEY_AGE_r)

# saveRDS(bmi_followup, paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                              "bmi_followup.RDS"))


# blood pressure ----------------------------------------------------------

# derivation of baseline and time-varying BP from EHR is similar to BMI

bp_data <- read_sas(paste0(data_folder, 
                           "raw_data_tables/Membership_EHR_mortality_data/",
                           "c10049_bp.sas7bdat"))

length(unique(bp_data$SUBJID)) # n = 192534
# filter for subjects that are in the tte dataset 
bp_data <- bp_data %>% filter(SUBJID %in% tte_data$SUBJID)
length(unique(bp_data$SUBJID)) # n = 182854

# there are 2075 subjects with no bp data from EHR
nrow(tte_data) - length(unique(bp_data$SUBJID))

# there are "categorical" bp's in the dataset
# table(bp_data$CATEGORICAL_DBP, useNA = "no")
# table(bp_data$CATEGORICAL_SBP, useNA = "no")

bp_data <- bp_data %>% 
  mutate(
    SUBJID = as.numeric(SUBJID),
    # merge categorical bp's with continuous
    CONTINUOUS_DBP = ifelse(is.na(CONTINUOUS_DBP), CATEGORICAL_DBP, CONTINUOUS_DBP),
    CONTINUOUS_SBP = ifelse(is.na(CONTINUOUS_SBP), CATEGORICAL_SBP, CONTINUOUS_SBP),
    # calculate age at event 
    AGE = ifelse(as.Date(MEASURE_DATE) == "1600-01-01", 90, 
                 interval(as.Date("1900-01-01"), as.Date(MEASURE_DATE)) / years(1)),
    AGE_yr = round(AGE),
    FLAG_90plus = as.Date(MEASURE_DATE) == "1600-01-01"
  ) %>% 
  rename(DBP = CONTINUOUS_DBP, SBP = CONTINUOUS_SBP) %>% 
  select(-CATEGORICAL_DBP, -CATEGORICAL_SBP, -MEASURE_DATE, -ENCTYPE) %>% 
  arrange(SUBJID, AGE) 

# cor(bp_data$DBP, bp_data$SBP) # correlation = 0.408

# look at the number of measurements each subject has per year before age 90
n_summary <- bp_data %>% 
  filter(!FLAG_90plus) %>% 
  count(SUBJID, AGE_yr) %>% 
  rename(bp = n) %>% 
  right_join(n_summary, by = c("SUBJID", "AGE_yr")) 

bp_median <- bp_data %>% 
  filter(!FLAG_90plus) %>% # discard obs that are 90+ censored
  group_by(SUBJID, AGE_yr) %>% # summarise median bp's 
  summarise(
    DBP_median = median(DBP), 
    SBP_median = median(SBP)
  ) %>% 
  ungroup() %>% 
  rename(DBP = DBP_median, SBP = SBP_median) 

# merge in survey age rounded by yr
bp_median <- bp_median %>% left_join(tte_data_survey_age, by = "SUBJID")

## baseline ----
bp_baseline <- bp_median %>% filter(AGE_yr == SURVEY_AGE_r)

# add into tte_add 
tte_add <- bp_baseline %>% select(-AGE_yr, -SURVEY_AGE_r) %>% 
  right_join(tte_add, by = "SUBJID")

## followup ----
bp_followup <- bp_median %>% filter(AGE_yr >= SURVEY_AGE_r)

# saveRDS(bp_followup, paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                              "bp_followup.RDS"))

# lipid labs --------------------------------------------------------------
# the original lipid labs dataset is 
# ~/Box/Asian_Americans_dementia_data/raw_data_tables/Lab_data/c10049_labs_20230630.sas7bdat
# I broke this large dataset into 4 smaller datasets by LAB_TEST_NAME in SAS

# variables in these datasets are: 
## SUBJID
## LAB_DT -> convert into age at lab measurement
## LAB_TEST_NAME:
#     HDL, LDL, Total Cholesterol, Triglycerides
## LAB_CODE:
#     HDL_test_v1, v2
#     LDL_CLC_NS_test_v1, v2
#     TOT_CHOLES_test_v1
#     TRIGL_NS_test_v1, v2, v3
## MODIFIER_HIGH, MODIFIER_LOW
#     high: EQ, LE, LT; low: EQ, GE, GT
## NORMAL_HIGH_C, NORMAL_LOW_C
## RESULT, RESULT_UNIT (all "MG/DL")

## HDL ----
hdl <- read_sas(paste0(data_folder, "raw_data_tables/Lab_data/", "hdl.sas7bdat")) 

hdl <- hdl %>% 
  mutate(
    LAB_AGE = ifelse(
      as.Date(LAB_DT) == "1600-01-01", 90, 
      interval(as.Date("1900-01-01"), as.Date(LAB_DT)) / years(1)), 
    LAB_90plus = as.Date(LAB_DT) == "1600-01-01", 
    RESULT_NUM = case_when(
      RESULT == ">102" ~ 102,
      RESULT == "<=4" ~ 4, 
      TRUE ~ as.numeric(RESULT)
    )
  )

# the warning "NAs introduced by coercion" comes from as.numeric(RESULT)
# since it is fully evaluated, regardless of prior case_when conditions.
# check to make sure there is no NA after mutating
hdl %>% filter(is.na(RESULT_NUM)) %>% nrow()

table(hdl$RESULT %in% c(">102", "<=4"), hdl$LAB_CODE)
# these values only show up in HDL test V1

# compare the two test versions
table(hdl$LAB_CODE)
table(hdl$LAB_CODE) %>% prop.table()
# there are very few V2 test results

ggplot(hdl, aes(x = RESULT_NUM, group = LAB_CODE)) + 
  geom_density(aes(color = LAB_CODE)) + 
  labs(subtitle = "HDL")
# looks like v2 values are generally smaller

# per July 12 2023, will keep both test versions for HDL and other lipid labs

hdl_sub <- hdl %>% 
  filter(
    # subset to subjects in our analytical dataset
    SUBJID %in% tte_data$SUBJID, 
    # remove labs done after 90
    LAB_90plus == FALSE
  ) %>% 
  mutate(SUBJID = as.numeric(SUBJID), LAB_AGE_yr = round(LAB_AGE)) %>% 
  select(SUBJID, LAB_CODE, LAB_TEST_NAME, 
         LAB_AGE, LAB_AGE_yr, LAB_90plus, RESULT_NUM)

# look at the number of measurements each subject has per year before age 90
n_summary <- hdl_sub %>% 
  count(SUBJID, LAB_AGE_yr) %>% 
  rename(hdl = n, AGE_yr = LAB_AGE_yr) %>% 
  right_join(n_summary, by = c("SUBJID", "AGE_yr")) 
# lipid panel is administered as a set -- not going to do the same summary for 
# other lipid items

lipid_labs <- hdl_sub %>% 
  group_by(SUBJID, LAB_AGE_yr) %>% 
  summarise(HDL = median(RESULT_NUM)) %>% 
  ungroup()

## LDL ----
ldl <- read_sas(paste0(data_folder, "raw_data_tables/Lab_data/", "ldl.sas7bdat")) 

ldl <- ldl %>% 
  mutate(
    LAB_AGE = ifelse(
      as.Date(LAB_DT) == "1600-01-01", 90, 
      interval(as.Date("1900-01-01"), as.Date(LAB_DT)) / years(1)), 
    LAB_90plus = as.Date(LAB_DT) == "1600-01-01", 
    RESULT_NUM = case_when(
      RESULT == ">211" ~ 211,
      RESULT == "<=7" ~ 7,
      TRUE ~ as.numeric(RESULT)
    )
  ) 

# check missingness; there is none
ldl %>% filter(is.na(RESULT_NUM)) %>% nrow()

table(ldl$RESULT %in% c(">211", "<=7"), ldl$LAB_CODE)
# these values show up in both versions of the test

table(ldl$LAB_CODE)
table(ldl$LAB_CODE) %>% prop.table()
# there are very few V2 results

ggplot(ldl, aes(x = RESULT_NUM, group = LAB_CODE)) + 
  geom_density(aes(color = LAB_CODE)) + 
  labs(subtitle = "LDL")
# same as hdl; looks like v2 values are generally smaller
# will keep both versions 

ldl_sub <- ldl %>% 
  filter(
    # subset to subjects in our analytical dataset
    SUBJID %in% tte_data$SUBJID, 
    # remove labs done after 90
    LAB_90plus == FALSE
  ) %>% 
  mutate(SUBJID = as.numeric(SUBJID), LAB_AGE_yr = round(LAB_AGE)) %>% 
  select(SUBJID, LAB_CODE, LAB_TEST_NAME, 
         LAB_AGE, LAB_AGE_yr, LAB_90plus, RESULT_NUM)
  
lipid_labs <- ldl_sub %>% 
  group_by(SUBJID, LAB_AGE_yr) %>% 
  summarise(LDL = median(RESULT_NUM)) %>% 
  ungroup() %>% 
  full_join(lipid_labs, by = c("SUBJID", "LAB_AGE_yr"))

## total cholesterol ----
tot_choles <- read_sas(paste0(data_folder, "raw_data_tables/Lab_data/", "tot_choles.sas7bdat")) 

tot_choles <- tot_choles %>% 
  mutate(
    LAB_AGE = ifelse(
      as.Date(LAB_DT) == "1600-01-01", 90, 
      interval(as.Date("1900-01-01"), as.Date(LAB_DT)) / years(1)), 
    LAB_90plus = as.Date(LAB_DT) == "1600-01-01", 
    RESULT_NUM = case_when(
      RESULT == ">312" ~ 312,
      RESULT == "<=47" ~ 47,
      TRUE ~ as.numeric(RESULT)
    )
  )

# check missingness; there is none
tot_choles %>% filter(is.na(RESULT_NUM)) %>% nrow()

tot_choles_sub <- tot_choles %>% 
  filter(
    # subset to subjects in our analytical dataset
    SUBJID %in% tte_data$SUBJID, 
    # remove labs done after 90
    LAB_90plus == FALSE
  ) %>% 
  mutate(SUBJID = as.numeric(SUBJID), LAB_AGE_yr = round(LAB_AGE)) %>% 
  select(SUBJID, LAB_CODE, LAB_TEST_NAME, 
         LAB_AGE, LAB_AGE_yr, LAB_90plus, RESULT_NUM)

lipid_labs <- tot_choles_sub %>% 
  group_by(SUBJID, LAB_AGE_yr) %>% 
  summarise(TOT_CHOLES = median(RESULT_NUM)) %>% 
  ungroup() %>% 
  full_join(lipid_labs, by = c("SUBJID", "LAB_AGE_yr"))

## triglycerides ----
trigl <- read_sas(paste0(data_folder, "raw_data_tables/Lab_data/", "trigl.sas7bdat")) 

trigl <- trigl %>% 
  mutate(
    LAB_AGE = ifelse(
      as.Date(LAB_DT) == "1600-01-01", 90, 
      interval(as.Date("1900-01-01"), as.Date(LAB_DT)) / years(1)), 
    LAB_90plus = as.Date(LAB_DT) == "1600-01-01", 
    RESULT_NUM = case_when(
      RESULT == ">539" ~ 539,
      RESULT == "<=33" ~ 33,
      TRUE ~ as.numeric(RESULT)
    )
  )

# check missingness; there is none
trigl %>% filter(is.na(RESULT_NUM)) %>% nrow()

table(trigl$LAB_CODE)
table(trigl$LAB_CODE) %>% prop.table()

ggplot(trigl, aes(x = RESULT_NUM, group = LAB_CODE)) + 
  geom_density(aes(color = LAB_CODE)) + 
  labs(subtitle = "triglycerides")
# the 3 versions are quite similar in distribution
# keep all three versions 

trigl_sub <- trigl %>% 
  filter(
    # subset to subjects in our analytical dataset
    SUBJID %in% tte_data$SUBJID, 
    # remove labs done after 90
    LAB_90plus == FALSE
    # keep all three test versions
  ) %>% 
  mutate(SUBJID = as.numeric(SUBJID), LAB_AGE_yr = round(LAB_AGE)) %>% 
  select(SUBJID, LAB_CODE, LAB_TEST_NAME, 
         LAB_AGE, LAB_AGE_yr, LAB_90plus, RESULT_NUM) 
  
lipid_labs <- trigl_sub %>% 
  group_by(SUBJID, LAB_AGE_yr) %>% 
  summarise(TRIGL = median(RESULT_NUM)) %>% 
  ungroup() %>% 
  full_join(lipid_labs, by = c("SUBJID", "LAB_AGE_yr"))

## look at the summarised data ----

library(corrplot)
correlation <- cor(lipid_labs[, c("HDL", "LDL", "TOT_CHOLES", "TRIGL")], 
                   use = "pairwise.complete.obs") 
corrplot(correlation, method = "number", type = "lower")
corrplot(correlation, method = 'ellipse', type = "lower")

test <- lipid_labs %>% 
  mutate(TOT_CHOLES_calc = HDL + LDL + TRIGL/4, 
         LDL_calc = TOT_CHOLES - HDL - TRIGL/4, 
         HDL_LDL_ratio = HDL / LDL)

cor(test[, c("TOT_CHOLES", "TOT_CHOLES_calc")], use = "complete.obs")  
cor(test[, c("LDL", "LDL_calc")], use = "complete.obs")  

cor(test[, c("HDL", "LDL", "TOT_CHOLES", "TRIGL", "HDL_LDL_ratio")], 
    use = "pairwise.complete.obs") %>% 
  corrplot(., method = "number", diag = FALSE)

## save the final dataset ----
lipids_followup <- lipid_labs %>% 
  left_join(tte_data_survey_age, by = "SUBJID") %>% 
  filter(LAB_AGE_yr >= SURVEY_AGE_r)

# saveRDS(lipids_followup, paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                                 "lipids_followup.RDS"))

# save n_summary ----

# save(n_summary,
#      file = paste0(data_folder,
#                    "aa_stroke_dementia/final_datasets/",
#                    "summary_n_bp_lipid_measures.RData"))

