require(table1)
require(labelled)

# function to re-factor datasets
# and give meaningful labels for variables
 
t1_relabel <- function(df) {
  # df <- long_data_pre_MI %>% slice_head(by = subjid) %>%
    # mutate(fu_time = end_age_r - survey_age_r)
  tb1_df <- df %>% 
    mutate(
      ethnicity_rev = factor(ethnicity_rev, 
                             levels = c("White", "Chinese", "Filipino",
                                        "Japanese", "South Asian", 
                                        "Korean", "Vietnamese", "Pacific Islander", 
                                        "Other SE Asian", "Multiple")),
      female = ifelse(female == 1, "Yes", "No") %>% factor(levels = c("Yes", "No")), 
      current_smoker = current_smoker %>% factor(levels = c("Yes", "No")), 
      # across(
      #   c(starts_with("prev"), starts_with("inc")),
      #   function(x) factor(x, levels = c(1, 0), labels = c("Yes", "No"))
      # ),
      end_type = factor(
        end_type, 
        levels = c("DEMENTIA", "DEATH", "END OF MEMBERSHIP", "ADMIN CENSORED"),
        labels = c("Dementia", "Death", "End of membership", "Administratively censored")
      )
    )
  var_label(tb1_df) <- list(
    survey_age_r = "Baseline age, years [Mean (SD)]", 
    female = "Women", 
    education_4 = "Education attainment, n (%)",
    usaborn_rev = "US born, n (%)",
    maritalstatus = "Marital status, n (%)", 
    income_pp = "Household-adjusted income, dollars [mean (SD)]", 
    generalhealth_3 = "General health, n (%)",
    current_smoker = "Current smoker, n (%)",
    sr_bmi = "BMI: self-reported [mean (SD)]",
    bmi = "BMI at baseline [mean (SD)]",
    tot_choles = "Total cholesterol at baseline, mg/dL [mean (SD)]",
    sbp = "Systolic blood pressure at baseline, mmHg [mean (SD)]",
    
    diab_tbl1 = "Diabetes, n (%)", 
    htn_tbl1 = "Hypertension, n (%)",
    ami_tbl1 = "Acute myocardial infarction, n (%)",
    chf_tbl1 = "Congestive heart failure, n (%)",
    pvd_tbl1 = "Peripheral vascular disease, n (%)",
    ihd_tbl1 = "Ischemic heart disease, n (%)",
    dyslip_tbl1 = "Dyslipidemia, n (%)",
    cancer_tbl1 = "Cancer, n (%)",
    
    # prev_diab = "Prevalent diabetes, n (%)",
    # prev_htn = "Prevalent hypertension, n (%)",
    # prev_ami = "Prevalent acute myocardial infarction, n (%)",
    # prev_chf = "Prevalent congestive heart failure, n (%)", 
    # prev_pvd = "Prevalent peripheral vascular disease, n (%)", 
    # prev_ihd = "Prevalent ischemic heart disease, n (%)", 
    # prev_dyslip = "Prevalent dyslipidemia, n (%)", 
    # prev_cancer = "Prevalent cancer, n (%)", 
    # 
    # inc_diab = "Incident diabetes, n (%)",
    # inc_htn = "Incident hypertension, n (%)",
    # inc_ami = "Incident acute myocardial infarction, n (%)",
    # inc_chf = "Incident congestive heart failure, n (%)", 
    # inc_pvd = "Incident peripheral vascular disease, n (%)", 
    # inc_ihd = "Incident ischemic heart disease, n (%)", 
    # inc_dyslip = "Incident dyslipidemia, n (%)", 
    # inc_cancer = "Incident cancer, n (%)", 
    
    end_type = "End of follow-up event, n (%)", 
    fu_time = "Follow-up time, years [mean (SD)]"
  )
  
  return(tb1_df)
  
}

rndr <- function(x, ...) {
  y <- render.default(x, ...)
  levels <- names(y)
  y[!levels %in% c("Not a case", "No")]
}

rndr.na <- function(x, ...) {
  y <- render.missing.default(x)
  if (is.numeric(x)) y 
  else NULL
}

# rndr.cont <- function(x) {
#   with(
#     stats.default(x), 
#     c("",
#       "Mean (SD)" = sprintf("%s (%s)", round_pad(MEAN, 0), round_pad(SD, 0)))
#   )
# }
# 
# rndr.cat <- function(x) {
#   c(
#     "", sapply(stats.default(x), 
#                function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT)))
#   )
# }


# t1_relabel <- function(df) {
#   tb1_df <- df %>% 
#     mutate(
#       FEMALE = factor(FEMALE, levels = c(1, 0), labels = c("Yes", "No")),
#       
#       ASIAN = factor(ASIAN, levels = c(0, 1), labels = c("White", "Asian")), 
#       
#       ETHNICITY_REV = factor(ETHNICITY_REV, 
#                              # levels = c(9, 1:8, 10),
#                              # labels = c("White", 
#                              #            1"South Asian", 2"Chinese", 3"Japanese",
#                              #            4"Korean", 5"Filipino", 6"Vietnamese", 
#                              #            7"Other SE Asian", 8"Pacific Islander", 
#                              #            10"Multiple"
#                              # ), 
#                              levels = c(9, 2, 5, 3, 4, 8, 1, 6, 7, 10),
#                              labels = c("White", 
#                                         "Chinese", "Filipino", "Japanese", 
#                                         "Korean", "Pacific Islander", 
#                                         "South Asian", "Vietnamese", 
#                                         "Other SE Asian", "Multiple")
#       ),
#       
#       EDUCATION_REV = factor(EDUCATION_REV, levels = c(1:6), 
#                              labels = c("Grade school", "Some high school", 
#                                         "High school or GED", 
#                                         "Technical/trade/some college", 
#                                         "College", "Graduate school")
#       ), 
#       
#       # EDU_GE_COLLEGE = factor(EDU_GE_COLLEGE, levels = c(1, 0), 
#       #                         labels = c("Yes", "No")),
#       
#       USABORN_REV = factor(USABORN_REV, levels = c(1, 0), 
#                            labels = c("Born in the US", "Foreign born")),
#       
#       MARITALSTATUS = factor(MARITALSTATUS, levels = 1:4, 
#                              labels = c("Never Married", 
#                                         "Married or living as married",
#                                         "Separated/Divorced", 
#                                         "Widowed")),
#       
#       GENERALHEALTH = factor(GENERALHEALTH, levels = 1:5, 
#                              labels = c("Excellent", "Very Good", "Good", 
#                                         "Fair", "Poor")),
#       
#       SMOKING_STATUS = factor(SMOKING_STATUS, levels = 1:3, 
#                               labels = c("Never", "Former", "Current")), 
#       
#       # DIAB_DX5YR_FLAG = factor(DIAB_DX5YR_FLAG, levels = c(1, 0),
#       #                          labels = c("Yes", "No")),
#       # 
#       # DIAB_DX7YR_FLAG = factor(DIAB_DX7YR_FLAG, levels = c(1, 0),
#       #                          labels = c("Yes", "No")),
#       # 
#       # DIAB_DX9YR_FLAG = factor(DIAB_DX9YR_FLAG, levels = c(1, 0),
#       #                          labels = c("Yes", "No")),
#       # 
#       # HTN_DX5YR_FLAG = factor(HTN_DX5YR_FLAG, levels = c(1, 0), 
#       #                         labels = c("Yes", "No")),
#       # 
#       # HTN_DX7YR_FLAG = factor(HTN_DX7YR_FLAG, levels = c(1, 0), 
#       #                         labels = c("Yes", "No")),
#       # 
#       # HTN_DX9YR_FLAG = factor(HTN_DX9YR_FLAG, levels = c(1, 0), 
#       #                         labels = c("Yes", "No")),
#       
#       # FIRST_PREVSTROKE_FLAG = factor(FIRST_PREVSTROKE_FLAG, levels = c(1, 0), 
#       #                                labels = c("Yes", "No")), 
#       # FIRST_INCSTROKE_FLAG = factor(FIRST_INCSTROKE_FLAG, levels = c(1, 0), 
#       #                                labels = c("Yes", "No")), 
#       
#       # FIRST_PREV_STROKE_FLAG = factor(FIRST_PREV_STROKE_FLAG, levels = c(1, 0),
#       #                                labels = c("Yes", "No")),
#       # FIRST_INC_STROKE_FLAG = factor(FIRST_INC_STROKE_FLAG, levels = c(1, 0),
#       #                               labels = c("Yes", "No")),
#       # # incident stroke types
#       # FIRST_INC_ISCHEMIC = factor(FIRST_INC_ISCHEMIC, levels = c(1, 0),
#       #                             labels = c("Yes", "No")), 
#       # FIRST_INC_TIA = factor(FIRST_INC_TIA, levels = c(1, 0),
#       #                             labels = c("Yes", "No")), 
#       # FIRST_INC_HSTROKE = factor(FIRST_INC_HSTROKE, levels = c(1, 0),
#       #                        labels = c("Yes", "No")), 
#       
#       
#       MAIN_DEM_V1_END_TYPE = factor(MAIN_DEM_V1_END_TYPE, 
#                                     levels = c("DEMENTIA", "DEATH", 
#                                                "ADMIN CENSORED", "END OF MEMBERSHIP", 
#                                                "CENSORED 90+"),
#                                     labels = c("Dementia", "Death", 
#                                                "Administratively Censored", 
#                                                "End of Membership", 
#                                                "Censored 90+"))
#       
#     )
#   
#   var_label(tb1_df) <- list(
#     SURVEY_AGE = "Survey age [Mean (SD)]", 
#     FEMALE = "Female", 
#     EDUCATION_REV = "Education attainment", 
#     # EDU_GE_COLLEGE = "College degree or more",
#     # EDU_3 = "Education attainment: 3 categories",
#     USABORN_REV = "US born",
#     MARITALSTATUS = "Marital status", 
#     INCOME_PP = "Household size-adjusted income [Mean (SD)]",
#     GENERALHEALTH = "General health",
#     SMOKING_STATUS = "Smoking status", 
#     BASELINE_BMI = "Baseline BMI", 
#     SR_BMI = "Self-reported BMI",
#     
#     # BASELINE_DBP_1 = "Baseline diastolic blood pressure",
#     # BASELINE_SBP_1 = "Baseline systolic blood pressure",
#     # BASELINE_DBP_2 = "Baseline diastolic blood pressure",
#     # BASELINE_SBP_2 = "Baseline systolic blood pressure",
#     
#     # HTN_DX_AGE = "Age at first hypertension diagnosis", 
#     MAIN_DEM_V1_END_TYPE = "End of follow up event",
#     MAIN_DEM_V1_FU_TIME = "Follow up time [Mean (SD)]",
#     # DIAB_DX5YR_FLAG = "Diabetes exposure 5+ years pre-survey",
#     # DIAB_DX7YR_FLAG = "Diabetes exposure 7+ years pre-survey",
#     # DIAB_DX9YR_FLAG = "Diabetes exposure 9+ years pre-survey",
#     # HTN_DX5YR_FLAG = "Hypertension diagnosis 5+ years pre-survey",
#     # HTN_DX7YR_FLAG = "Hypertension diagnosis 7+ years pre-survey",
#     # HTN_DX9YR_FLAG = "Hypertension diagnosis 9+ years pre-survey",
#     # FIRST_PREVSTROKE_FLAG = "Prevalent Stroke",
#     # FIRST_INCSTROKE_FLAG = "Incident Stroke"
#     # FIRST_PREV_STROKE_FLAG = "Prevalent stroke",
#     # FIRST_INC_STROKE_FLAG = "Incident stroke",
#     # FIRST_INC_ISCHEMIC = "Incident ischemic stroke", 
#     # FIRST_INC_TIA = "Incident TIA", 
#     # FIRST_INC_HSTROKE = "Incident hemorrhagic stroke"
#     # EHR_HT_MEDIAN = "EHR Height [Mean (SD)]"
#   )
#   
#   return(tb1_df)
# }
