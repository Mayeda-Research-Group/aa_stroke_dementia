
# This script preforms multilevel multiple imputation for the 
# long-format analytic dataset. 

# set up packages and paths ----

if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load(
  "haven", "tidyverse", "here", 
  "mice", "miceadds", "lme4"
)

source(here("01_R", "path.R"))

# load data ---------------------------------------------------------------
data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

# long data with both baseline and time-varying covariates 
long_tte_data <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "long_tte_data_rounded_by_yr.RDS")
)

# hoffman paths
# data_path <- "/u/home/y/yixzhou/AA_stroke_dem/data/"
# long_tte_data <- readRDS(paste0(data_path, "long_tte_data_rounded_by_yr.RDS"))

# look at missingness -----------------------------------------------------

long_data_pre_MI <- long_tte_data %>% 
  select(
    # remove some variables that are mostly collinear with others
    -c(asian, edu_ge_college), 
    # remove tte variables
    -c(end_age_r, end_type, event_death, event_dem, event_end_mem), 
    # only keep one stroke defn: stroke_combined
    -c(stroke_isd_acvd, stroke_hstroke, stroke_combined_isd_hstroke)
  )

names(long_data_pre_MI)
glimpse(long_data_pre_MI)

# organize the variables to include in imputation
baseline_vars <- c(
  # these are complete: 
  "survey_age_r", 
  "female", "ethnicity_rev", 
  # these have missingness: 
  "usaborn_rev", "education_rev", 
  "maritalstatus",  "income", "sizeofhh", "income_pp", 
  "generalhealth", "smoking_status", "sr_bmi", 
  # EHR indicator does not have missingness: 
  "prev_ami"
)

# auxiliary baseline variables - with missingness
aux_vars <- c(
  "usabornfather_rev", "usabornmother_rev"
)

# time varying 
tv_vars <- c(
  # continuous - with missingness
  "hdl", "ldl", "trigl", "tot_choles", "sbp", "bmi", 
  # indicators - complete
  "diab", "htn", "inc_ami", "chf", "pvd", "cancer", "ihd", "dyslip", 
  # stroke - complete
  "stroke_combined"
)


NA_summary <- long_data_pre_MI %>% 
  select(all_of(baseline_vars), all_of(aux_vars), all_of(tv_vars)) %>% 
  apply(2, function(x) sum(is.na(x)) / length(x))

(NA_summary * 100) %>% sort()
# most missingness in the lipid labs

# collect variables with missingness
baseline_vars_w_NA <- NA_summary[baseline_vars][NA_summary[baseline_vars] != 0] %>% names()
tv_vars_w_NA <- NA_summary[tv_vars][NA_summary[tv_vars] != 0] %>% names()


# prepare the dataset for MI ----------------------------------------------

# rescaling parameters for INCOME_PP
mean <- mean(long_tte_data$income_pp, na.rm = TRUE)
sd <- sd(long_tte_data$income_pp, na.rm = TRUE)
# this is needed because mice sometimes gives warnings about this

long_data_pre_MI <- long_data_pre_MI %>% 
  select(
    subjid, fu_yr, all_of(baseline_vars), all_of(aux_vars), all_of(tv_vars)
  ) %>% 
  mutate(
    # need to set these character factors back to numeric factors
    # because for some reason miceadds does not like very long strings 
    # or strings with empty spaces in them (I think)
    across(
      c(ethnicity_rev, usaborn_rev, education_rev, maritalstatus, generalhealth, smoking_status), 
      function(x) as.numeric(x) %>% as.factor()
    ),
    # rescale income
    income_pp = (income_pp - mean)/sd
  )

# summary(long_tte_data)
# summary(long_data_pre_MI)


# set up imputation -------------------------------------------------------
# multilevel imputation

# initial predictor matrix and imputation methods
predM <- mice::make.predictorMatrix(data = long_data_pre_MI) 
impM <- mice::make.method(data = long_data_pre_MI) 


predM1 <- predM
predM1[, "subjid"] <- 0 # do not predict anything with SUBJID
predM1["subjid", ] <- 0 # this step is optional
predM1[tv_vars_w_NA, "subjid"] <- -2 # set nesting variable for the two tv vars
predM1[c("income", "sizeofhh"), "income_pp"] <- 0 # income_pp doesn't predict hhincome or sizeofhh
predM1[c("hdl", "tot_choles", "trigl"), "ldl"] <- 0 # ldl doesn't predict all other lipids

# predM1[c("SR_BMI", "EDUCATION_REV", "SBP_1", "BMI"), "SUBJID"] <- c(0, 0, -2, -2)

impM1 <- impM
# 2l methods are for time-varying variables
# 2l.only methods are for subject-level variables
# 2lonly.function is a generic method; we specify the exact methods to use later on
impM1[tv_vars_w_NA] <- "2l.pmm" 
impM1[c(baseline_vars_w_NA, aux_vars)] <- "2lonly.function" 
# impM1[c("BMI", "SBP_1","SR_BMI","EDUCATION_REV")] <- c("2l.pmm", "2l.pmm", 
#                                                        "2lonly.function", "2lonly.function" )

# define imputation functions
# imputationFunction <- list( "SR_BMI"="pmm", "EDUCATION_REV"="pmm" )
# use pmm for all 2lonly methods
imputationFunction <- rep("pmm", length(c(baseline_vars_w_NA, aux_vars))) %>% as.list()
names(imputationFunction) <- c(baseline_vars_w_NA, aux_vars)
imputationFunction

# define cluster variable, i.e. nested in SUBJID
# cluster_var <- list( "SR_BMI"="SUBJID", "EDUCATION_REV"="SUBJID" )
cluster_var <- rep("subjid", length(c(baseline_vars_w_NA, aux_vars))) %>% as.list()
names(cluster_var) <- c(baseline_vars_w_NA, aux_vars)
cluster_var


# save all objects required for MI ----------------------------------------

save(
  long_data_pre_MI, 
  predM1, 
  impM1, 
  imputationFunction, 
  cluster_var, 
  file = paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                "pre_MI_objects_Hoffman.RData")
)


# start imputation --------------------------------------------------------

# starting from here, I submitted 5 separate jobs (each m = 1, maxit = 5) to 
# run on Hoffman, and saved the mice objects. 
# the scripts used on Hoffman are 2.1 and 2.2. 

start <- Sys.time()
imp1 <- mice::mice(
  long_data_pre_MI,
  m = 1,
  predictorMatrix = predM1,
  method = impM1,
  maxit = 1, 
  imputationFunction = imputationFunction,
  cluster_var = cluster_var, 
  seed = 1357
)
end <- Sys.time()
end - start

# m = 1, maxit = 1: 18.3 min in total
# m = 1, maxit = 2: 36.8 min in total
# will need to run more

# saveRDS(
#   imp1,
#   file = paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                 "multilevel_imp_1_1357_08012023.RDS")
# )


# check imputation results ------------------------------------------------

# read in the 5 mi objects 

mi_objects <- list.files(paste0(data_folder, "aa_stroke_dementia/final_datasets/"), 
                         pattern = "multilevel_imp_2023-11-06", full.names = TRUE)

# combine the imputations
imp <- readRDS(mi_objects[1])
for (i in 2:length(mi_objects)) {
  imp_i <- readRDS(mi_objects[i])
  imp <- ibind(imp, imp_i)
}

# saveRDS(imp, 
#         paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                "combined_imps.RDS"))
#
# imp <- readRDS(paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                       "combined_imps.RDS"))

imp$loggedEvents
imp$m

# check missingness is gone
long_data_pre_MI %>% apply(2, function(x) sum(is.na(x)) / length(x))
complete(imp) %>% apply(2, function(x) sum(is.na(x)) / length(x))

complete(imp) %>% 
  select(subjid, all_of(baseline_vars_w_NA)) %>% 
  distinct() %>% 
  nrow() # should be the same as the number of subjects 
unique(long_tte_data$subjid) %>% length() # matches!

plot(imp, layout = c(4,5))
# nativity variables and ldl might require more iterations to converge.
densityplot(imp)
# can't run all the density plots all at once: 
# "Error: vector memory exhausted (limit reached?)"

# look at density plots by individual variables
densityplot(imp, ~ education_rev)
densityplot(imp, ~ sr_bmi)
densityplot(imp, ~ bmi)
densityplot(imp, ~ sbp)
densityplot(imp, ~ tot_choles) # very similar distributions
densityplot(imp, ~ hdl) # very similar distributions
densityplot(imp, ~ ldl) # largest difference in distributions among the 4 lipid labs
densityplot(imp, ~ trigl) # very similar distributions

summary(long_data_pre_MI)
summary(complete(imp, action = 'long'))


# prepare post MI dataset -------------------------------------------------

complete(imp, action = 1) %>% names()
long_tte_data$ethnicity_rev %>% table()

for (i in 0:imp$m){
  # i <- 0 # 0 : original data before imputation, with missingness
  complete(imp, action = i) %>% 
    mutate(
      imp = i, # add imputation index
      income_pp = income_pp * sd + mean, # scale back
      
      # give the factors back their meaningful labels
      ethnicity_rev = factor(ethnicity_rev, 
                             levels = c(9, 2, 5, 3, 4, 8, 1, 6, 7, 10),
                             labels = c("White", 
                                        "Chinese", "Filipino", "Japanese", 
                                        "Korean", "Pacific Islander", 
                                        "South Asian", "Vietnamese", 
                                        "Other SE Asian", "Multiple")), 
      usaborn_rev = factor(usaborn_rev, levels = c(1, 0),
                           labels = c("Born in the US", "Foreign born")),
      education_4 = case_when(
        education_rev %in% c(1,2) ~ "Grade school or some high school", 
        education_rev %in% c(3) ~ "High school or GED", 
        education_rev %in% c(4) ~ "Technical/trade/some college", 
        education_rev %in% c(5,6) ~ "College degree or more", 
      ) %>% 
        factor(levels = c(
          "Grade school or some high school", 
          "High school or GED", 
          "Technical/trade/some college", 
          "College degree or more"
          # "<= Grade 11", 
          # "Grade 12/high school diploma/GED",
          # "Technical/trade/vocational school/some college", 
          # "College degree"
        )), 
      
      education_rev = factor(education_rev, levels = c(1:6),
                             labels = c("Grade school", "Some high school",
                                        "High school or GED",
                                        "Technical/trade/some college",
                                        "College", "Graduate school")
      ),
      maritalstatus = factor(maritalstatus, levels = 1:4, 
                             labels = c("Never married", 
                                        "Married or living as married",
                                        "Separated/divorced", 
                                        "Widowed")),
      generalhealth_3 = case_when(
        generalhealth %in% c(1,2) ~ "Excellent or very good", 
        generalhealth %in% c(3) ~ "Good", 
        generalhealth %in% c(4,5) ~ "Fair or poor", 
      ) %>% 
        factor(levels = c("Excellent or very good", "Good", "Fair or poor")),
      generalhealth = factor(generalhealth, levels = 1:5, 
                             labels = c("Excellent", "Very Good", "Good", 
                                        "Fair", "Poor")),
      
      
      smoking_status = factor(smoking_status, levels = 1:3, 
                              labels = c("Never", "Former", "Current")), 
      current_smoker = ifelse(smoking_status == "Current", "Yes", "No")
    ) %>%
    # join in variables not included in imputation
    left_join(
      long_tte_data %>%
        select(subjid, fu_yr, asian, # EDU_GE_COLLEGE,
               # additional stroke defns (stroke_combined is included in MI)
               stroke_isd_acvd, stroke_hstroke, stroke_combined_isd_hstroke,
               # tte variables
               start, end_age_r, end_type,
               event_death, event_dem, event_end_mem),
      by = c("subjid", "fu_yr"), multiple = "all"
    ) %>% 
    saveRDS(
      file = paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                    "combined_imp_", i, ".RDS")
    )
}





# old code -----------------------------------------------------

names(long_tte_data)

baseline_vars <- c(
  "FEMALE", "ASIAN", "ETHNICITY_REV", "USABORN_REV", "EDUCATION_REV", 
  "EDU_GE_COLLEGE", "MARITALSTATUS", "GENERALHEALTH", "SMOKING_STATUS", 
  "SURVEY_AGE_r", "SR_BMI", "PREV_AMI"
)
tv_vars <- c(
  "FU_YR", "SBP_1", "BMI", "DIAB", "HTN", "INC_AMI", "CHF", "PVD", "STROKE"
)
aux_vars <- c(
  "USABORNFATHER_REV", "USABORNMOTHER_REV", "INCOME", "SIZEOFHH", "INCOME_PP"
)

c(baseline_vars, tv_vars, aux_vars) %>% length() 

test %>% 
  select(SUBJID, all_of(baseline_vars), all_of(aux_vars)) %>% 
  distinct() %>% 
  apply(2, function(x) sum(is.na(x)) / length(x))

test %>% 
  select(SUBJID, all_of(tv_vars)) %>% 
  apply(2, function(x) sum(is.na(x)) / length(x))


# set up methods and predictor matrix 


set.seed(1234)
sample_id <- unique(long_tte_data$SUBJID) %>% sample(size = 500)
test <- long_tte_data %>% 
  filter(SUBJID %in% sample_id) %>% 
  mutate(BMI = ifelse(BMI == 999, NA_real_, BMI), 
         SBP_1 = ifelse(SBP_1 == 999, NA_real_, SBP_1))

test_subset_tv_only <- test %>% 
  select(SUBJID, FU_YR, SURVEY_AGE_r, FEMALE, 
         # EDUCATION_REV + SR_BMI, 
         BMI, SBP_1, HTN, STROKE)

mice_long0_tv_only <- mice(test_subset_tv_only, maxit = 0)
meth <- mice_long0_tv_only$method
meth[c("BMI", "SBP_1")] <- "2l.norm"
pred_mat <- mice_long0_tv_only$predictorMatrix
pred_mat["BMI", ] <- c(-2, 0, 2, 2, 0, 2, 2, 2)
pred_mat["SBP_1",] <- c(-2, 0, 2, 2, 2, 0, 2, 2)

micelong_tv_only <- test_subset_tv_only %>% 
  mice(., meth = meth, pred = pred_mat, m = 1, maxit = 1, seed = 1234)

test_subset_tv_only %>% apply(2, function(x) sum(is.na(x)) / length(x))
complete(micelong_tv_only) %>% apply(2, function(x) sum(is.na(x)) / length(x))

mi_results <- tibble(
  data = "tv only", 
  l1_method = "2l.norm", 
  l2_method = "NA", 
  l2_predicts_l1 = "NA", 
  l1_results = "Both tv BMI and SBP are completely imputed.",
  l2_results = "NA", 
  # results = "l1 vars missingness is completely imputed", 
  comments = ""
)

test_subset <- test %>% 
  select(SUBJID, FU_YR, SURVEY_AGE_r, FEMALE, 
         EDUCATION_REV, SR_BMI, 
         BMI, SBP_1, HTN, STROKE) 

mice_long0 <- mice(test_subset, maxit = 0)

meth <- mice_long0$method
meth[c("EDUCATION_REV", "SR_BMI")] <- "2lonly.pmm"
meth[c("BMI", "SBP_1")] <- "2l.norm"
pred_mat <- mice_long0$predictorMatrix


pred_mat["EDUCATION_REV",] <- c(-2, 0, 1, 1, 0, 1, 0, 0, 0, 0)
pred_mat["SR_BMI",] <- c(-2, 0, 1, 1, 1, 0, 0, 0, 0, 0)

# use l2 to predict l1
pred_mat["BMI", ] <- c(-2, 0, 2, 2, 2, 2, 0, 2, 2, 2)
pred_mat["SBP_1",] <- c(-2, 0, 2, 2, 2, 2, 2, 0, 2, 2)
# do not use l2 to predict l1
pred_mat["BMI", ] <- c(-2, 0, 2, 2, 0, 0, 0, 2, 2, 2)
pred_mat["SBP_1",] <- c(-2, 0, 2, 2, 0, 0, 2, 0, 2, 2)

micelong <- test_subset %>% 
  mice(., meth = meth, pred = pred_mat, m = 2, maxit = 1, seed = 1234)

test_subset %>% apply(2, function(x) sum(is.na(x)) / length(x))
complete(micelong, 2) %>% apply(2, function(x) sum(is.na(x)) / length(x))

mi_results <- add_row(
  mi_results, 
  tibble(
    data = "subject-level and tv", 
    l1_method = "2l.norm", 
    l2_method = "2lonly.pmm", 
    l2_predicts_l1 = "Yes", 
    l1_results = "BMI is partially imputed, SBP is completed imputed.", 
    l2_results = "EDU is not imputed, SR BMI is partially imputed.", 
    # results = "l1: tv BMI is partially imputed, SBP is completed imputed. 
    #            l2: EDU is not imputed, SR BMI is partially imputed." , 
    comments = "Although imputation completes, I get warning messages: 'invalid factor level, NA generate'."
  )
)

mi_results <- add_row(
  mi_results, 
  tibble(
    data = "subject-level and tv", 
    l1_method = "2l.norm", 
    l2_method = "2lonly.pmm", 
    l2_predicts_l1 = "No", 
    l1_results = "Both tv BMI and SBP are completely imputed (probably similar to the tv-only data scenario).", 
    l2_results = "EDU is not imputed, SR BMI is partially imputed.", 
    # results = "l1: both tv BMI and SBP are completely imputed. 
    #            l2: EDU is not imputed, SR BMI is partially imputed." , 
    comments = "Same warning messages: 'invalid factor level, NA generate'."
  )
)

# try only imputing the L2 covariates

mice_long0 <- mice(test_subset %>% select(-BMI, -SBP_1), maxit = 0)

meth <- mice_long0$method
meth[c("EDUCATION_REV", "SR_BMI")] <- "2lonly.pmm"
pred_mat <- mice_long0$predictorMatrix

pred_mat["EDUCATION_REV",] <- c(-2, 0, 1, 1, 0, 1, 1, 1)
pred_mat["SR_BMI",] <- c(-2, 0, 1, 1, 1, 0, 1, 1)

pred_mat["EDUCATION_REV",] <- c(-2, 0, 1, 1, 0, 1, 0, 0)
pred_mat["SR_BMI",] <- c(-2, 0, 1, 1, 1, 0, 0, 0)
pred_mat[, "SUBJID"] <- -2
pred_mat[, "FU_YR"] <- 0

micelong <- test_subset %>% 
  mice(., meth = meth, pred = pred_mat, m = 2, maxit = 1, seed = 1234)

test_subset %>% apply(2, function(x) sum(is.na(x)) / length(x))
complete(micelong, 2) %>% apply(2, function(x) sum(is.na(x)) / length(x))






