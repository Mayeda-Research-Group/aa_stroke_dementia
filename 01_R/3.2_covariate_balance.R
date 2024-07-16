
# set up libraries and paths ----------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load(
  "haven", "tidyverse", "here", "openxlsx", 
  "survival", "splines", "Hmisc", "gridExtra"
)

# load confoundr pkg separately
# install.packages("devtools",dependencies=TRUE)
# library(devtools)
# install_github("jwjackson/confoundr",
#                dependencies=c("Depends","Imports"), 
#                build = TRUE, 
#                build_opts = c("--no-resave-data","--no-manual"))

library(confoundr)
# make.history.one() and make.history.two() do not run straight out of the pkg.
# I fixed the bug and included in the tv_covbal_functions.R
# along with some other helper functions
source(here("01_R", "tv_covbal_functions.R"))

source(here("01_R", "path.R"))

# load data -------------------------------------------------------------

data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

# load one imputation 
long_tte_data <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "combined_imp_1.RDS"))

long_tte_data <- long_tte_data %>% arrange(subjid, fu_yr)

# load weighting formulas -------------------------------------------------

weighting_formulas <- readRDS(paste0(
  data_folder, "aa_stroke_dementia/final_datasets/",
  "weighting_formulas.RDS"
))

# take a look
# weighting_formulas$stroke_combined$White

# set up weight generating functions --------------------------------------

generate_IPTW <- function(longdata, formula, return_model = FALSE) {
  # testing arguments
  # longdata <- long_data_sub
  # formula <- formulas$stroke
  
  # an example of the formula argument:
  # formula <- c(
  #   "STROKE ~ bs(FU_YR, 3)", # time term
  #   " + bs(SURVEY_AGE_r, 3) + FEMALE + USABORN_REV + EDUCATION_REV", # time fixed
  #   " + GENERALHEALTH + SMOKING_STATUS + PREV_AMI", # time fixed
  #   " + DIAB + HTN + INC_AMI + CHF + PVD + BMI + SBP_1" # time-varying
  # )
  # formula for the glm should be a vector of terms, where
  # the first element includes exposure as the outcome and the time term, 
  # and the following elements are additional covariate terms
  
  original_vars <- names(longdata)
  
  # create variable to flag paststroke 
  longdata <- longdata %>% 
    group_by(subjid) %>%
    mutate(
      paststroke = ifelse(stroke_combined == 1 & lag(stroke_combined) == 0, 0, stroke_combined),
      paststroke = ifelse(is.na(paststroke), 0, paststroke)
    ) %>%
    ungroup()
  
  # denominator
  stroke_mod <- glm(
    as.formula(paste0(formula, collapse = "")), # use the complete formula
    family = binomial(), # quasibinomial?
    data = subset(longdata, paststroke == 0))
  
  # numerator 
  stroke_mod_num <- glm(
    as.formula(formula[1]), # only use the time term
    family = binomial(), # quasibinomial?
    data = subset(longdata, paststroke == 0))
  
  # combine and calculate IPTW
  longdata <- longdata %>% 
    mutate(
      pred_stroke_denom = ifelse(
        paststroke == 1, 
        1, # the prob of having stroke is 1 for those with stroke already
        predict(stroke_mod, newdata = longdata, type = 'response')
      ), 
      pred_stroke_num = ifelse(
        paststroke == 1, 
        1, predict(stroke_mod_num, newdata = longdata, type = 'response')
      ), 
      stroke_denom = ifelse(stroke_combined == 1, pred_stroke_denom, 1 - pred_stroke_denom),
      stroke_num = ifelse(stroke_combined == 1, pred_stroke_num, 1 - pred_stroke_num)
    ) %>% 
    group_by(subjid) %>% 
    mutate(
      denom_cum = cumprod(stroke_denom), 
      num_cum = cumprod(stroke_num)
    ) %>% 
    ungroup() %>% 
    mutate(
      wt_stroke = 1 / denom_cum, # unstabilized 
      wt_stroke_stab = num_cum / denom_cum, # stabilized
      # apply truncation
      wt_stroke_trunc = ifelse(
        wt_stroke > quantile(wt_stroke, 0.99),
        quantile(wt_stroke, 0.99), wt_stroke
      ), 
      wt_stroke_stab_trunc = ifelse(
        wt_stroke_stab > quantile(wt_stroke_stab, 0.99), 
        quantile(wt_stroke_stab, 0.99), wt_stroke_stab
      )
    )
  
  # exclude intermediate vars from the output dataset 
  longdata <- longdata %>% select(all_of(original_vars), starts_with("wt_"))
  
  if (return_model) {
    return(list(longdata, stroke_mod))
  } else {
    return(longdata)
  }
  
}

generate_IPCW <- function(longdata, formula1, formula2, return_model = FALSE) {
  # testing arguments
  # longdata <- long_data_sub
  # formula1 <- formulas$death
  # formula2 <- formulas$endofMem
  # or more explicitly:
  # # for death as censoring event
  # formula1 <- c(
  #   "eventDeath ~ FU_YR + I(FU_YR ^ 2) + STROKE", # time term and treatment
  #   " + FEMALE + USABORN_REV + EDUCATION_REV", # time fixed
  #   " + GENERALHEALTH + SMOKING_STATUS + PREV_AMI", # time fixed
  #   " + DIAB + HTN + INC_AMI + CHF + PVD + BMI + SBP_1" # time-varying
  # )
  # # for end of membership as censoring event
  # formula2 <- c(
  #   "eventEndMem ~ STROKE", # time term and treatment
  #   " + FEMALE + USABORN_REV + EDUCATION_REV", # time fixed
  #   " + DIAB + HTN + PREV_AMI + INC_AMI + CHF + PVD + BMI + SBP_1" # time-varying
  # )
  
  original_vars <- names(longdata)
  
  ## death as censoring event ----
  # denominator
  plrFit_Death <- glm(as.formula(paste0(formula1, collapse = "")), 
                      data = longdata, family = binomial())
  
  # numerator for stabilized weights
  plrFit_Death_num <- glm(as.formula(formula1[1]), 
                          data = longdata, family = binomial())
  
  longdata <- longdata %>% 
    mutate(
      predDeath = 1 - predict(plrFit_Death, newdata = longdata, type = "response"), 
      predDeath_num = 1 - predict(plrFit_Death_num, newdata = longdata, type = "response")
    ) %>%
    group_by(subjid) %>% 
    mutate(
      denom_cum_death = cumprod(predDeath), 
      num_cum_death = cumprod(predDeath_num)
    ) %>% 
    ungroup()
  
  ## end of membership censoring ----
  # denominator
  plrFitC <- glm(as.formula(paste0(formula2, collapse = "")), 
                 data = longdata, family = binomial())
  
  # numerator
  plrFitCnum <- glm(as.formula(formula2[1]), 
                    data = longdata, family = binomial())
  
  longdata <- longdata %>% 
    mutate(
      predC = 1 - predict(plrFitC, newdata = longdata, type = "response"),
      predC_num = 1 - predict(plrFitCnum, newdata = longdata, type = "response")
    ) %>% 
    group_by(subjid) %>% 
    mutate(
      cumPredC = cumprod(predC), 
      cumPredC_num = cumprod(predC_num)
    ) %>% 
    ungroup()
  
  ## calculate weights ----
  longdata <- longdata %>% 
    mutate(
      # censoring due to end of membership
      wt_endmem = 1 / cumPredC, # unstabilized
      wt_endmem_stab = cumPredC_num / cumPredC, # stabilized
      
      # censoring due to death
      wt_death = 1 / denom_cum_death,
      wt_death_stab = num_cum_death / denom_cum_death,
      
      # combined censoring weights
      wt_cens = wt_endmem * wt_death, 
      wt_cens_stab = wt_endmem_stab * wt_death_stab,
      
      # truncate death weights
      # remember to remove na.rm = TRUE after imputation
      wt_death_trunc = ifelse(
        wt_death > quantile(wt_death, 0.99), 
        quantile(wt_death, 0.99), wt_death
      ), 
      wt_death_stab_trunc = ifelse(
        wt_death_stab > quantile(wt_death_stab, 0.99), 
        quantile(wt_death_stab, 0.99), wt_death_stab
      )
    )
  
  # exclude intermediate variables
  longdata <- longdata %>% select(all_of(original_vars), starts_with("wt_"))
  
  if (return_model) {
    return(list(longdata, plrFit_Death, plrFitC))
  } else {
    return(longdata)
  }
  
}

# check t-v balance by ethnicity ------------------------------------------
all_ethns <- names(weighting_formulas$stroke_isd_acvd)

wt_summary <- vector(mode = "list", length = length(all_ethns))
names(wt_summary) <- all_ethns

for (ethn in all_ethns) {
  # ethn <- all_ethns[2]
  
  formulas <- weighting_formulas[["stroke_combined"]][[ethn]]
  
  long_data_sub <- long_tte_data %>% filter(ethnicity_rev == ethn) %>% 
    mutate(new_id = subjid) # I don't know what I did this for? 
  
  long_data_sub <- generate_IPTW(long_data_sub, formulas$stroke)
  long_data_sub <- generate_IPCW(long_data_sub, formulas$death, formulas$endofMem)
  
  wt_summary[[ethn]] <- long_data_sub %>% 
    select(starts_with("wt_")) %>% 
    apply(2, function(x) summary(x) %>% round(3)) %>% 
    as_tibble() %>%
    mutate(ethnicity = ethn, 
           stat = c("min", "Q1", "median", "mean", "Q3", "max"), 
           .before = everything())
  
  # prep for running confoundr-based functions
  # dummy code all categorical variables we want to check
  dummy_vars <- model.matrix(
    ~ education_4 + generalhealth_3 + current_smoker, 
    data = long_data_sub
  ) 
  long_data_sub <- cbind(long_data_sub, dummy_vars[, -1])
  
  # choose variables that we want to check covariate balance for
  tv_covars <- c("diab", "htn", "inc_ami", "chf", "pvd", "bmi", "sbp", 
                 "cancer", "ihd", "dyslip", "tot_choles")
  static_covars <- c("survey_age_r", "female", colnames(dummy_vars)[-1], "prev_ami")
  # var_labels <- c("SurveyAge", "Male", "Edu:HS", "Edu:Tech", "Edu:College", 
  #                 "GeneralHealth:Good", "")

  stroke_check <- check_IPTW(
    longdata = long_data_sub, wt_to_test = "wt_stroke", 
    stroke_defn = "stroke_combined",
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = stroke_check$p_covbal + labs(subtitle = "unstabilized IPTW"), 
    filename = here("03_figs", "covbal_plots", paste0("IPTW_", "unstab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
  stroke_check <- check_IPTW(
    longdata = long_data_sub, wt_to_test = "wt_stroke_stab", 
    stroke_defn = "stroke_combined",
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = stroke_check$p_covbal + labs(subtitle = "stabilized IPTW"), 
    filename = here("03_figs", "covbal_plots", paste0("IPTW_", "stab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
  long_data_sub <- long_data_sub %>% 
    mutate(
      event_cens = ifelse(event_end_mem == 1 | event_death == 1, 1, 0),
      # wt_cens = wt_EndMem * wt_Death, 
      # wt_cens_stab = wt_EndMem_stab * wt_Death_stab
    )
  
  censor_check <- check_IPCW(
    longdata = long_data_sub, wt_to_test = "wt_cens", 
    stroke_defn = "stroke_combined",
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )

  ggsave(
    plot = censor_check$p_covbal + labs(subtitle = "unstabilized IPCW"),
    filename = here("03_figs", "covbal_plots", paste0("IPCW_", "unstab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
  censor_check <- check_IPCW(
    longdata = long_data_sub, wt_to_test = "wt_cens_stab", 
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = censor_check$p_covbal + labs(subtitle = "stabilized IPCW"),
    filename = here("03_figs", "covbal_plots", paste0("IPCW_", "stab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
}


# secondary analysis ------------------------------------------------------
# use ischemic stroke as the stroke defn

id_remove_isd <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "ids_exclude_isd_analysis.RDS")
)

long_tte_data_isd <- long_tte_data %>% 
  filter(!subjid %in% id_remove_isd)

wt_summary_isd <- vector(mode = "list", length = length(all_ethns))
names(wt_summary_isd) <- all_ethns

for (ethn in all_ethns) {
  ethn <- all_ethns[2]
  
  formulas <- weighting_formulas[["stroke_isd_acvd"]][[ethn]]
  
  long_data_sub <- long_tte_data_isd %>% filter(ethnicity_rev == ethn) %>% 
    mutate(new_id = subjid) # I don't know what I did this for? 
  
  long_data_sub <- generate_IPTW(long_data_sub, formulas$stroke)
  long_data_sub <- generate_IPCW(long_data_sub, formulas$death, formulas$endofMem)
  
  wt_summary_isd[[ethn]] <- long_data_sub %>% 
    select(starts_with("wt_")) %>% 
    apply(2, function(x) summary(x) %>% round(3)) %>% 
    as_tibble() %>%
    mutate(ethnicity = ethn, 
           stat = c("min", "Q1", "median", "mean", "Q3", "max"), 
           .before = everything())
  
  # prep for running confoundr-based functions
  # dummy code all categorical variables we want to check
  dummy_vars <- model.matrix(
    ~ education_4 + generalhealth_3 + current_smoker, 
    data = long_data_sub
  ) 
  long_data_sub <- cbind(long_data_sub, dummy_vars[, -1])
  
  # choose variables that we want to check covariate balance for
  tv_covars <- c("diab", "htn", "inc_ami", "chf", "pvd", "bmi", "sbp", 
                 "cancer", "ihd", "dyslip", "tot_choles")
  static_covars <- c("survey_age_r", "female", colnames(dummy_vars)[-1], "prev_ami")
  # var_labels <- c("SurveyAge", "Male", "Edu:HS", "Edu:Tech", "Edu:College", 
  #                 "GeneralHealth:Good", "")
  
  stroke_check <- check_IPTW(
    longdata = long_data_sub, wt_to_test = "wt_stroke", 
    stroke_defn = "stroke_isd_acvd",
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = stroke_check$p_covbal + labs(subtitle = "unstabilized IPTW"), 
    filename = here("03_figs", "covbal_plots_isd", paste0("IPTW_", "unstab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
  stroke_check <- check_IPTW(
    longdata = long_data_sub, wt_to_test = "wt_stroke_stab", 
    stroke_defn = "stroke_isd_acvd",
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = stroke_check$p_covbal + labs(subtitle = "stabilized IPTW"), 
    filename = here("03_figs", "covbal_plots_isd", paste0("IPTW_", "stab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
  long_data_sub <- long_data_sub %>% 
    mutate(
      event_cens = ifelse(event_end_mem == 1 | event_death == 1, 1, 0),
      # wt_cens = wt_EndMem * wt_Death, 
      # wt_cens_stab = wt_EndMem_stab * wt_Death_stab
    )
  
  censor_check <- check_IPCW(
    longdata = long_data_sub, wt_to_test = "wt_cens", 
    stroke_defn = "stroke_isd_acvd",
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = censor_check$p_covbal + labs(subtitle = "unstabilized IPCW"),
    filename = here("03_figs", "covbal_plots_isd", paste0("IPCW_", "unstab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
  censor_check <- check_IPCW(
    longdata = long_data_sub, wt_to_test = "wt_cens_stab", 
    # timepoints = c(0, 1, 4, 8, 12, 16)
    timepoints = c(0, 1, 4, 8)
  )
  
  ggsave(
    plot = censor_check$p_covbal + labs(subtitle = "stabilized IPCW"),
    filename = here("03_figs", "covbal_plots_isd", paste0("IPCW_", "stab_", ethn, ".png")),
    height = 8, width = 12, units = "in"
  )
  
}

# look at variable contribution ----
# only uses the combine stroke defn

library(flextable)
library(officer)

all_ethns <- names(weighting_formulas$stroke_combined)
glm_summary <- vector(mode = "list", length = length(all_ethns[c(1:4,7)]))
names(glm_summary) <- all_ethns[c(1:4,7)]

for (ethn in all_ethns[c(1:4,7)]) {
  # ethn <- all_ethns[2]
  
  formulas <- weighting_formulas[["stroke_combined"]][[ethn]]
  
  long_data_sub <- long_tte_data %>% filter(ethnicity_rev == ethn) %>% 
    mutate(new_id = subjid) # I don't know what I did this for? 
  
  long_data_IPTW <- generate_IPTW(long_data_sub, formulas$stroke, TRUE)
  long_data_IPCW <- generate_IPCW(long_data_sub, formulas$death, formulas$endofMem, TRUE)
  
  IPTW_mod <- long_data_IPTW[[2]] %>% broom::tidy() %>% 
    mutate(
      pval_stars = gtools::stars.pval(p.value),
      ci_lower = estimate - 1.96 * std.error, 
      ci_upper = estimate + 1.96 * std.error, 
      across(c(estimate, ci_lower, ci_upper), exp)
    ) %>% 
    select(term, estimate, ci_lower, ci_upper, p.value, pval_stars) %>% 
    mutate(weight = "IPTW")
    # rename_with(function(x) ifelse(x == "term", x, paste0("IPTW-", x)))
  IPCW_death_mod <- long_data_IPCW[[2]] %>% broom::tidy() %>% 
    mutate(
      pval_stars = gtools::stars.pval(p.value),
      ci_lower = estimate - 1.96 * std.error, 
      ci_upper = estimate + 1.96 * std.error, 
      across(c(estimate, ci_lower, ci_upper), exp)
    ) %>% 
    select(term, estimate, ci_lower, ci_upper, p.value, pval_stars) %>% 
    # rename_with(function(x) ifelse(x == "term", x, paste0("IPCWdeath-", x)))
    mutate(weight = "IPCWdeath")
  IPCW_mem_mod <- long_data_IPCW[[3]] %>% broom::tidy() %>% 
    mutate(
      pval_stars = gtools::stars.pval(p.value),
      ci_lower = estimate - 1.96 * std.error, 
      ci_upper = estimate + 1.96 * std.error, 
      across(c(estimate, ci_lower, ci_upper), exp)
    ) %>% 
    select(term, estimate, ci_lower, ci_upper, p.value, pval_stars) %>% 
    # rename_with(function(x) ifelse(x == "term", x, paste0("IPCWmem-", x)))
    mutate(weight = "IPCWmem")
  
  glm_summary[[ethn]] <- bind_rows(IPTW_mod, IPCW_death_mod, IPCW_mem_mod)
    # IPTW_mod %>% 
    # right_join(IPCW_death_mod, by = "term") %>% 
    # left_join(IPCW_mem_mod, by = "term") 

}

# output a wide summary table for each ethnicity
# the estimates and CIs are OR's 
lapply(
  glm_summary, 
  function(x) {
    # x <- glm_summary$White
    x %>% 
      pivot_wider(
        id_cols = term,
        names_from = weight,
        values_from = c(estimate, ci_lower, ci_upper, p.value, pval_stars)
      ) %>% 
      # mutate(
      #   IPTW = paste0(
      #     round(estimate_IPTW, 2), " (", round(ci_lower_IPTW, 2), ", ", round(ci_upper_IPTW, 2), ")"
      #   ),
      #   IPCWdeath = paste0(
      #     round(estimate_IPCWdeath, 2), " (", round(ci_lower_IPCWdeath, 2), ", ", round(ci_upper_IPCWdeath, 2), ")"
      #   ),  
      #   IPCWmem = paste0(
      #     round(estimate_IPCWmem, 2), " (", round(ci_lower_IPCWmem, 2), ", ", round(ci_upper_IPCWmem, 2), ")"
      #   )
      # ) %>% 
      select(term, ends_with("IPTW"), ends_with("IPCWdeath"), ends_with("IPCWmem")) 
  }
) %>% 
  write.xlsx(here("02_outputs", "weight_model_summary.xlsx"))




cov_order <- glm_summary$White$term %>% unique() %>% rev()
cov_label <- c(
  "Cancer", "Stroke", "Total cholesterol", "Dyslipidemia", 
  "Ischemic heart disease", "Systolic blood pressure", "BMI", 
  "Peripheral vascular disease", "Congestive heart failure", 
  "Incident acute myocardial infarction", "Hypertension", 
  "Diabetes", "Prevalent acute myocardial infarction", 
  "Current smoker", "General health: fair or poor", "General health: good", 
  "Education: college degree or more", "Education: technical/trade/some college", 
  "Education: high school or GED", "Female", 
  paste0("Splines term for baseline age - ", 4:1),
  paste0("Splines term for follow-up time - ", 4:1),
  paste0("Intercept")
)

plot_df <- bind_rows(glm_summary, .id = "ethnicity") %>%  
  filter(!str_detect(term, "fu_yr|survey_age_r|Intercept")) %>% 
  mutate(
    weight = factor(weight, levels = c("IPTW", "IPCWdeath", "IPCWmem")),
    term = factor(term, levels = cov_order, labels = cov_label), 
    pval_0.05 = ifelse(p.value <= 0.05, "p<=0.05", "p>0.05")
  )

plot_df %>% 
  ggplot(aes(x = estimate, y = term, color = pval_0.05)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") + 
  geom_point() + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.5) + 
  theme_bw() + 
  labs(x = "Odds ratio", y = "Covariate", color = NULL) + 
  facet_grid(rows = vars(ethnicity), cols = vars(weight))

ggsave(here("03_figs", "weight_models_OR.png"),
       height = 12, width = 9, units = "in")

# plot_df %>% 
#   ggplot(aes(x = term, y = estimate, color = pval_0.05)) + 
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.5) + 
#   theme_bw() + 
#   facet_grid(rows = vars(ethnicity), cols = vars(weight))


# glm_summary_wformat <- lapply(
#   glm_summary, function(x) {
#     # x <- glm_summary$White
#     x %>%
#       # select(-ends_with("p.value")) %>% 
#       mutate(
#         across(
#           c(ends_with("estimate"), ends_with("ci_lower"), ends_with("ci_upper")), 
#           function(x) format(round(x, 2), nsmall = 2)
#         ),
#         IPTW = paste0(`IPTW-estimate`, " (", `IPTW-ci_lower`, " ,", `IPTW-ci_upper`, ")"), 
#         IPCWdeath = paste0(`IPCWdeath-estimate`, " (", `IPCWdeath-ci_lower`, " ,", `IPCWdeath-ci_upper`, ")"), 
#         IPCWmem = paste0(`IPCWmem-estimate`, " (", `IPCWmem-ci_lower`, " ,", `IPCWmem-ci_upper`, ")"), 
#       ) %>% 
#       flextable() %>% 
#       align(
#         align = c("left", rep("center", 18)), 
#         part = "all"
#       ) %>% 
#       padding(padding = 0, part = "all") %>% 
#       colformat_double(digits = 2) %>% 
#       separate_header(opts = "center-hspan", split = "-") %>% 
#       autofit() %>% 
#       fit_to_width(max_width = 10)
#   }
# )

glm_summary_wformat <- lapply(
  glm_summary, function(x) {
    # x <- glm_summary$White
    x %>%
      mutate(
        term = factor(term, levels = rev(cov_order), labels = rev(cov_label)), 
        across(
          c(estimate, ci_lower, ci_upper), 
          function(x) format(round(x, 2), nsmall = 2) %>% str_trim()
        ),
        est_ci = paste0(estimate, " (", ci_lower, ", ", ci_upper, ")")
       ) %>% 
      pivot_wider(
        id_cols = term, 
        names_from = weight, 
        values_from = est_ci
      ) %>% 
      flextable() %>% 
      align(
        align = c("left", rep("center", 3)), 
        part = "all"
      ) %>% 
      padding(padding = 0, part = "all") %>% 
      colformat_double(digits = 2) %>% 
      # separate_header(opts = "center-hspan", split = "-") %>% 
      autofit() %>% 
      fit_to_width(max_width = 10)
  }
)

save_as_docx(
  `White` = glm_summary_wformat$White, 
  `Chinese` = glm_summary_wformat$Chinese,
  `Filipino` = glm_summary_wformat$Filipino,
  `Japanese` = glm_summary_wformat$Japanese,
  `South Asian` = glm_summary_wformat$`South Asian`,
  path = here("02_outputs", "weight_model_summary.docx"),
  pr_section = prop_section(
    page_size = page_size(orient = "landscape")
  )
)

