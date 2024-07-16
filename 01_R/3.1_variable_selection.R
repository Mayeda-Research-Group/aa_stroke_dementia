
# set up libraries and paths ----------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load(
  "haven", "tidyverse", "here", "openxlsx", 
  "survival", "splines" # "Hmisc", "gridExtra"
)

source(here("01_R", "path.R"))

source(here("01_R", "helper_functions.R"))

# Load data -------------------------------------------------------------

data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

# load one imputation 
long_tte_data <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "combined_imp_1.RDS"))

# -------------------------------------------------------------------------
# 
# long_tte_data_last <- long_tte_data %>% 
#   arrange(SUBJID, FU_YR) %>% 
#   group_by(SUBJID) %>% 
#   slice_tail() %>% 
#   ungroup()
# 
# long_tte_data_last %>%
#   group_by(ETHNICITY_REV, FEMALE) %>% 
#   summarise(max_FU = max(FU_YR)) %>% View()

# temp <- long_tte_data_last %>% 
#   model.matrix(
#     ~ SURVEY_AGE + FEMALE + USABORN_REV + EDUCATION_REV + 
#       GENERALHEALTH + SMOKING_STATUS + PREV_AMI + 
#       DIAB + HTN + INC_AMI + CHF + PVD + BMI + SBP_1, data = .) %>% 
#   as_tibble()
# temp_stroke <- cbind(long_tte_data_last %>% select(SUBJID, ETHNICITY_REV, STROKE), temp[, -1])
# temp_death <- cbind(long_tte_data_last %>% select(SUBJID, ETHNICITY_REV, eventDeath), temp[, -1])
# temp_endofMem <- cbind(long_tte_data_last %>% select(SUBJID, ETHNICITY_REV, eventEndMem), temp[, -1])
# 
# temp_stroke_long <- temp_stroke %>% select(-SUBJID) %>% 
#   group_by(ETHNICITY_REV, STROKE) %>% 
#   summarise(across(everything(), mean)) %>% 
#   ungroup() %>% 
#   pivot_longer(
#     cols = -c(ETHNICITY_REV, STROKE),
#     names_to = "var", 
#     values_to = "value"
#   )
# 
# cont_vars <- c("SURVEY_AGE", "BMI", "SBP_1")
# temp_stroke_long %>% 
#   filter(!var %in% cont_vars) %>% 
#   ggplot(aes(x = var, y = value, group = STROKE)) + 
#   geom_point(aes(color = factor(STROKE))) + 
#   geom_hline(yintercept = c(0.1, 0.9), linetype = "dashed", color = "grey") + 
#   coord_flip() + 
#   theme_bw() + 
#   facet_wrap(~ ETHNICITY_REV) 





# time_term <- "bs(FU_YR, 3)"
# time_fixed_terms <- c(
#   " + bs(SURVEY_AGE_r, 3)", # baseline age
#   " + FEMALE + USABORN_REV + EDUCATION_REV", # demographics
#   " + GENERALHEALTH + SMOKING_STATUS + PREV_AMI" # health-related
# )
# time_varying_terms <- c(" + DIAB + HTN + INC_AMI + CHF + PVD", " + BMI + SBP_1")


# -------------------------------------------------------------------------

plot_km <- function(data = long_tte_data, stratify_by) {
  # stratify_by <- "education_4"
  formula <- paste0("Surv(time = start, time2 = fu_yr, event = event_dem) ~ ", 
                    stratify_by, " + ethnicity_rev")
  km_object <- survfit(
    as.formula(formula), data = data, cluster = subjid
  )
  
  p <- km_object %>% 
    broom::tidy() %>% 
    mutate(
      stratum = str_split_i(strata, ",", i = 1) %>% str_split_i("[^<]=", i = 2),
      # stroke = ifelse(str_detect(strata, "1"), "Yes", "No"), 
      ethn = str_split_i(strata, ",", i = 2) %>% str_split_i("[^<]=", i = 2) %>% str_trim(),
      cif = 1 - estimate, 
      cif.conf.low = 1 - conf.low, 
      cif.conf.high = 1 - conf.high, 
    ) %>% 
    ggplot(aes(time, cif)) +
    geom_line(aes(color = stratum), linewidth = 0.7) +
    geom_ribbon(aes(ymin = cif.conf.low, ymax = cif.conf.high, group = stratum), alpha = 0.1) +
    scale_x_continuous(
      # limits = c(0, 17),
      breaks = seq(0, 18, 3), minor_breaks = 0:18) +
    labs(y = "Cumulative Incidence", color = stratify_by) +
    # labs(title = title_text) + 
    theme_bw() + 
    facet_wrap(~ethn)
  
  ggsave(here("03_figs", "crude_km_stratified", paste0(stratify_by, ".png")), 
         plot = p)
  
  return(p)
}

plot_km(stratify_by = "stroke_combined")
plot_km(stratify_by = "stroke_isd_acvd")
plot_km(stratify_by = "stroke_hstroke")

# baseline covariates 
plot_km(stratify_by = "female") # female max FU is 13 because of CMHS
plot_km(stratify_by = "usaborn_rev") # don't include: no separation, small cell sizes
plot_km(stratify_by = "education_rev")
plot_km(stratify_by = "education_4")
plot_km(stratify_by = "generalhealth")
plot_km(stratify_by = "generalhealth_3")
plot_km(stratify_by = "smoking_status")
plot_km(stratify_by = "current_smoker")
plot_km(stratify_by = "prev_ami")

# time varying covariates
plot_km(stratify_by = "diab") # small separation; also depends on sample size
plot_km(stratify_by = "htn")
plot_km(stratify_by = "inc_ami") # does not include for SE Asians
plot_km(stratify_by = "chf")
plot_km(stratify_by = "pvd")
plot_km(stratify_by = "cancer") 
plot_km(stratify_by = "ihd")
plot_km(stratify_by = "dyslip")

# cannot check continuous variables directly: 
# baseline age, BMI, SBP, lipid labs (hdl, ldl, trigl, tot_choles)

# I'm going to bin these variables 

long_tte_data %>% 
  mutate(
    survey_age_cat = cut(survey_age_r, c(60, 70, 80, 90)), 
    survey_age_cat = str_sub(survey_age_cat, 2, 6) %>% str_replace(",", "-")
  ) %>% 
  plot_km(data = ., stratify_by = "survey_age_cat")

long_tte_data %>% 
  mutate(bmi_cat_25 = ifelse(bmi < 25, "<25", ">25"), 
         bmi_cat_30 = ifelse(bmi < 30, "<30", ">30"), ) %>% 
  plot_km(data = ., stratify_by = "bmi_cat_30")

long_tte_data %>% 
  mutate(sbp_cat_130 = ifelse(sbp < 130, "<130", ">130"),
         sbp_cat_120 = ifelse(sbp < 120, "<120", ">120")) %>% 
  plot_km(data = ., stratify_by = "sbp_cat_120")

long_tte_data %>% 
  mutate(hdl_50 = ifelse(hdl < 50, "<50", ">50")) %>% 
  plot_km(data = ., stratify_by = "hdl_50")

long_tte_data %>% 
  mutate(ldl_100 = ifelse(ldl < 100, "<100", ">100")) %>% 
  plot_km(data = ., stratify_by = "ldl_100")

long_tte_data %>% 
  mutate(trigl_100 = ifelse(trigl < 100, "<100", ">100")) %>% 
  plot_km(data = ., stratify_by = "trigl_100")

long_tte_data %>% 
  mutate(tot_choles_180 = ifelse(tot_choles < 180, "<180", ">180")) %>% 
  plot_km(data = ., stratify_by = "tot_choles_180")

long_tte_data %>% 
  mutate(
    hdl_ldl_ratio = hdl / ldl,
    hl_ratio_0.6 = ifelse(hdl_ldl_ratio < 0.6, "<0.6", ">0.6")
    ) %>%
  plot_km(data = ., stratify_by = "hl_ratio_0.6")


# long_tte_data_last %>% 
#   ggplot(aes(x = bmi, group = ethnicity_rev, color = ethnicity_rev)) + 
#   geom_density()

# long_tte_data %>% filter(ethnicity_rev == "Vietnamese") %>% View()


# set up final weighting models -------------------------------------------

time_term <- "ns(fu_yr, df = 4)" 
base_age_term <- "ns(survey_age_r, df = 4)"

cov_terms <- c(base_age_term, "female", "usaborn_rev", "education_4", 
               "generalhealth_3", "current_smoker", "prev_ami",
               "diab", "htn", "inc_ami", "chf", "pvd", "bmi", "sbp",
               "ihd", "cancer", "dyslip", "tot_choles")

ethns <- levels(long_tte_data$ethnicity_rev)


## weighting models for stroke and death -----------------------------------

weighting_models <- tibble(
  term = rep(cov_terms, length(ethns)), 
  ethnicity = rep(ethns, each = length(cov_terms)), 
  include = 0
)

weighting_models <- weighting_models %>% 
  mutate(
    include = case_when(
      term == "usaborn_rev" ~ 0, 
      ethnicity == "Other SE Asian" & 
        term %in% c("inc_ami", "chf", "pvd", "dyslip") ~ 0, 
      term == "cancer" ~ 0,
      TRUE ~ 1
    )
  ) 

# look at it to check
weighting_models %>% 
  pivot_wider(names_from = ethnicity, values_from = include) %>% 
  View()
  # write.xlsx(here("02_outputs", "stroke_weighting_variables.xlsx"))

stroke_death_terms <- lapply(ethns, function(x) {
  c(time_term, 
    weighting_models %>% 
      filter(ethnicity == x, include == 1) %>% 
      pull(term) %>% 
      paste0(collapse = "+") %>% 
      paste0("+", .)
  )
})

names(stroke_death_terms) <- ethns


## weighting model for end of membership ----------------------------------

endofMem_terms <- c(time_term, 
                    paste0(c("+", base_age_term, "+female+education_4+generalhealth_3"), 
                           collapse = ""))



## combine into one object -------------------------------------------------

stroke_defns <- long_tte_data %>% select(starts_with("stroke")) %>% names()
stroke_defns

weighting_formulas <- vector(mode = "list", length = length(stroke_defns))
names(weighting_formulas) <- stroke_defns


for (strk_term in stroke_defns) {
  weighting_formulas[[strk_term]] <- lapply(ethns, function(x) {
    list(
      stroke = c(paste0(strk_term, "~", stroke_death_terms[[x]][1]), 
                 stroke_death_terms[[x]][2]),
      death = c(paste0("event_death~", stroke_death_terms[[x]][1]), 
                paste0("+", strk_term, "+", "cancer", stroke_death_terms[[x]][2])),
      endofMem = c(paste0("event_end_mem~", endofMem_terms[1]), endofMem_terms[2])
    )
  })
  names(weighting_formulas[[strk_term]]) <- ethns
}

weighting_formulas[["stroke_combined"]][["White"]] # take a look

saveRDS(weighting_formulas, 
        file = paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                      "weighting_formulas.RDS"))


