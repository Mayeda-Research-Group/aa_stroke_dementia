# Set up libraries and paths ----------------------------------------------

if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load(
  "tidyverse", "here", 
  "survival", "survminer", 
  "table1", "openxlsx", "gtsummary"
  # "haven"
  # "mice", "miceadds", "lme4"
)

source(here("01_R", "path.R"))

# Load data -------------------------------------------------------------

data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

long_data_pre_MI <- readRDS(
  file = paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                "combined_imp_", 0, ".RDS")
)

long_data_MI_1 <- readRDS(
  file = paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                "combined_imp_", 1, ".RDS")
)

baseline_tte_data <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "baseline_tte_data_rounded_by_yr.RDS")
)

unique(long_data_pre_MI$subjid) %>% length()
nrow(baseline_tte_data)

# Table 1's ----------------------------------------------------------------

source(here("01_R", "function_tbl1_prep.R"))

## derive indicators for prevalent and incident comorbidities ----
# prevalence: with condition at baseline
# incidence: dx = 0 at baseline, dx becomes 1 during followup

prev_inc_comorb <- long_data_pre_MI %>% 
  slice_min(by = subjid, order_by = fu_yr) %>%
  select(subjid, diab, htn, chf, pvd, ihd, dyslip, cancer, prev_ami) %>% 
  rename_with(function(x) paste0("prev_", x), .cols = -c(subjid, prev_ami)) 

prev_inc_comorb <- long_data_pre_MI %>% 
  filter(fu_yr != 0) %>% 
  select(subjid, diab, htn, chf, pvd, ihd, dyslip, cancer, inc_ami) %>% 
  group_by(subjid) %>% 
  reframe(
    across(c(diab, htn, chf, pvd, ihd, dyslip, cancer, inc_ami), 
           function(x) ifelse(sum(x) == 0, 0, 1))
  ) %>% 
  rename_with(function(x) paste0("inc_", x), .cols = -c(subjid, inc_ami)) %>% 
  right_join(prev_inc_comorb, by = "subjid") 

prev_inc_comorb_long <- prev_inc_comorb %>% 
  pivot_longer(
    cols = -subjid, 
    names_pattern = "(.*)_(.*)",
    names_to = c("time", "condition"), 
    values_to = "dx"
  ) %>% 
  pivot_wider(
    id_cols = c(subjid, condition),
    names_from = time,
    values_from = dx
  ) 

# check 
prev_inc_comorb_long %>% with(table(prev, inc))
prev_inc_comorb_long %>% filter(prev == 1, inc == 0) %>% View()
# all the rows where a subject has prev condition but not inc condition 
# are from AMI
# inc_ami is derived regardless of prev_ami in the analytical dataset
# meaning inc_ami tracks incidence after baseline, regardless of prev condition
# in this summary, I will change its defn to fit with the other condtions
# i.e., prev: dx = 1 at baseline
#       inc: dx = 0 at baseline and dx becomes 1 during followup

prev_inc_comorb_wide <- prev_inc_comorb_long %>% 
  mutate(
    inc = ifelse(prev == 1, 0, inc), 
    dx = case_when(
      prev == 1 ~ "Prevalent",
      inc == 1 ~ "Incident",
      TRUE ~ "Not a case"
    ) %>% 
      factor(levels = c("Prevalent", "Incident", "Not a case"))
  ) %>% 
  select(subjid, condition, dx) %>% 
  pivot_wider(
    id_cols = subjid, 
    names_from = condition, 
    values_from = dx
  ) %>%
  rename_with(function(x) paste0(x, "_tbl1"), .cols = -subjid)




## pre MI ------------------------------------------------------------------

glimpse(long_data_pre_MI)

# these are the basic baseline characteristics
tb1_pre_MI <- long_data_pre_MI %>% 
  slice_min(by = subjid, order_by = fu_yr) %>% 
  mutate(fu_time = end_age_r - survey_age_r) %>%
  left_join(prev_inc_comorb_wide, by = "subjid") %>% 
  t1_relabel() %>% 
  table1(
    ~ survey_age_r + female + 
      usaborn_rev + education_4 + maritalstatus + income_pp + 
      generalhealth_3 + current_smoker + 
      bmi + tot_choles + sbp +
      diab_tbl1 + htn_tbl1 + ami_tbl1 + chf_tbl1 + pvd_tbl1 + ihd_tbl1 + 
      dyslip_tbl1 + cancer_tbl1 + 
      end_type + fu_time 
    | ethnicity_rev, 
    render.continuous = c(.= "Mean (SD)"),
    render.categorical = rndr,
    # render.missing = NULL,
    render.missing = rndr.na, 
    rounding.fn = round_pad,
    digits.pct = 0,
    digits = 0,
    data = .
  )




## post MI -----------------------------------------------------------------

# for one imputed dataset
tb1_post_MI_1 <- long_data_MI_1 %>% 
  slice_min(by = subjid, order_by = fu_yr) %>% 
  mutate(fu_time = end_age_r - survey_age_r) %>%
  left_join(prev_inc_comorb_wide, by = "subjid") %>% 
  t1_relabel() %>% 
  table1(
    ~ survey_age_r + female + 
      usaborn_rev + education_4 + maritalstatus + income_pp + 
      generalhealth_3 + current_smoker + 
      bmi + tot_choles + sbp +
      diab_tbl1 + htn_tbl1 + ami_tbl1 + chf_tbl1 + pvd_tbl1 + ihd_tbl1 + 
      dyslip_tbl1 + cancer_tbl1 + 
      end_type + fu_time 
    | ethnicity_rev, 
    render.continuous = c(.= "Mean (SD)"),
    render.categorical = rndr,
    render.missing = rndr.na,
    rounding.fn = round_pad,
    digits.pct = 0,
    digits = 0,
    data = .
  )

# save the tables
write.xlsx(
  list("pre_MI" = tb1_pre_MI, "post_MI_1" = tb1_post_MI_1), 
  file = here("02_outputs", "table_1_pre_and_post_imp.xlsx")
)


# Figure e1 Percentage of missingness over FU -----------------------------
p_missing_by_yr <- long_data_pre_MI %>% 
  group_by(fu_yr) %>% 
  summarise(
    across(c(bmi, sbp, tot_choles), 
           function(x) sum(is.na(x)) / n(), 
           .names = "perc_missing_{.col}"
    )
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = starts_with("perc_missing_"),
    names_to = "variable", 
    names_prefix = "perc_missing_",
    values_to = "perc_missing"
  ) %>% 
  mutate(
    variable = case_when(
      variable == "bmi" ~ "BMI", 
      variable == "sbp" ~ "Systolic blood pressure", 
      variable == "tot_choles" ~ "Total cholesterol"
    )
  ) %>% 
  ggplot(aes(x = fu_yr, y = perc_missing, 
             group = variable, color = variable)) + 
  geom_line() + 
  scale_x_continuous(limits = c(0, 10)) + 
  scale_color_discrete(breaks = c("Total cholesterol", "BMI", "Systolic blood pressure")) + 
  theme_bw() + 
  labs(
    color = "Time-varying variable", 
    x = "Follow-up year", y = "Proportion of Missingness"
  )

ggsave(p_missing_by_yr, 
       filename = here("03_figs", "figure_e1_p_missing_by_yr.png"),
       height = 4, width = 7, units = "in")


# stroke tte summary --------------------------------------------------------

baseline_tte_data <- baseline_tte_data %>% 
  janitor::clean_names() %>% 
  mutate(
    # add incident stroke flags 
    combined_stroke_inc = ifelse(!is.na(combined_stroke_r), 1, 0), 
    isd_acvd_inc = ifelse(!is.na(isd_acvd_r), 1, 0),
    hstroke_noties_inc = ifelse(!is.na(hstroke_noties_r), 1, 0),
    
    # label ethnicity
    ethnicity_rev = factor(
      ethnicity_rev, levels = c(9, 2, 5, 3, 1, 4, 6, 8, 7, 10),
      labels = c("White",
                 "Chinese", "Filipino", "Japanese",
                 "South Asian", "Korean", "Vietnamese", "Pacific Islander",
                 "Other SE Asian", "Multiple")
      
    )
  )

# set up summary
stroke_summary <- baseline_tte_data %>% 
  group_by(ethnicity_rev) %>% 
  summarise(
    N_total = n(), 
    N_combined_stroke = sum(combined_stroke_inc),
    N_isd_acvd = sum(isd_acvd_inc),
    N_hstroke_noties = sum(hstroke_noties_inc)
  ) %>% 
  ungroup()

# get median time (and IQR) to stroke by stroke type
baseline_tte_data <- baseline_tte_data %>% 
  mutate(
    # exact event ages
    across(
      c(combined_stroke, isd_acvd, hstroke_noties),
      function(x) ifelse(!is.na(x), x - survey_age, end_age - survey_age), 
      .names = "fu_{.col}"
    ),
    # rounded event ages
    across(
      c(combined_stroke_r, isd_acvd_r, hstroke_noties_r),
      function(x) ifelse(!is.na(x), x - survey_age_r, end_age_r - survey_age_r), 
      .names = "fu_{.col}"
    )
  )

stroke_summary <- baseline_tte_data %>% 
  filter(combined_stroke_inc == 1) %>% 
  group_by(ethnicity_rev) %>% 
  summarise(
    median = median(fu_combined_stroke),
    q25 = quantile(fu_combined_stroke, probs = 0.25),
    q75 = quantile(fu_combined_stroke, probs = 0.75),
    median_r = median(fu_combined_stroke_r), 
    q25_r = quantile(fu_combined_stroke_r, probs = 0.25), 
    q75_r = quantile(fu_combined_stroke_r, probs = 0.75)
  ) %>% 
  mutate(
    across(c(median:q75_r), function(x) {round(x, 1)} ),
    combined_tte = paste0(median, " (", q25, ", ", q75, ")"),
    combined_tte_r = paste0(median_r, " (", q25_r, ", ", q75_r, ")")
  ) %>% 
  select(ethnicity_rev, combined_tte, combined_tte_r) %>% 
  left_join(stroke_summary, by = "ethnicity_rev")

stroke_summary <- baseline_tte_data %>% 
  filter(isd_acvd_inc == 1) %>% 
  group_by(ethnicity_rev) %>% 
  summarise(
    median = median(fu_combined_stroke),
    q25 = quantile(fu_combined_stroke, probs = 0.25),
    q75 = quantile(fu_combined_stroke, probs = 0.75),
    median_r = median(fu_isd_acvd_r), 
    q25_r = quantile(fu_isd_acvd_r, probs = 0.25), 
    q75_r = quantile(fu_isd_acvd_r, probs = 0.75)
  ) %>% 
  mutate(
    across(c(median:q75_r), function(x) {round(x, 1)} ),
    isd_acvd_tte = paste0(median, " (", q25, ", ", q75, ")"), 
    isd_acvd_tte_r = paste0(median_r, " (", q25_r, ", ", q75_r, ")")
  ) %>% 
  select(ethnicity_rev, isd_acvd_tte, isd_acvd_tte_r) %>% 
  left_join(stroke_summary, by = "ethnicity_rev")

stroke_summary <- baseline_tte_data %>% 
  filter(hstroke_noties_inc == 1) %>% 
  group_by(ethnicity_rev) %>% 
  summarise(
    median = median(fu_combined_stroke),
    q25 = quantile(fu_combined_stroke, probs = 0.25),
    q75 = quantile(fu_combined_stroke, probs = 0.75),
    median_r = median(fu_hstroke_noties_r), 
    q25_r = quantile(fu_hstroke_noties_r, probs = 0.25), 
    q75_r = quantile(fu_hstroke_noties_r, probs = 0.75)
  ) %>% 
  mutate(
    across(c(median:q75_r), function(x) {round(x, 1)} ),
    hstroke_tte = paste0(median, " (", q25, ", ", q75, ")"),
    hstroke_tte_r = paste0(median_r, " (", q25_r, ", ", q75_r, ")")
  ) %>% 
  select(ethnicity_rev, hstroke_tte, hstroke_tte_r) %>% 
  left_join(stroke_summary, by = "ethnicity_rev")


## Table e5 cause-specific cumulative incidence of stroke ---------------------------
# for each stroke defn, at FU yr 10
# cause-specific means we consider death as competing event
# and admin-censored and end of membership as censoring event
# this way the denominator stays the same over time

### combined stroke ----
cs_risk_combined_stroke <- baseline_tte_data %>% 
  select(subjid, ethnicity_rev, fu_combined_stroke, combined_stroke_inc, end_type) %>% 
  mutate(
    stroke_end = case_when(
      combined_stroke_inc == 0 & 
        end_type %in% c("ADMIN CENSORED", "END OF MEMBERSHIP") ~ 0, # censored
      combined_stroke_inc == 0 & 
        end_type %in% c("DEATH") ~ 2, # death, competing event
      combined_stroke_inc == 1 ~ 1 # stroke
    ) %>% factor(levels = 0:2, labels = c("censored", "stroke", "death"))
  ) %>% 
  survfit(Surv(fu_combined_stroke, stroke_end) ~ ethnicity_rev, data = .) %>% 
  broom::tidy() %>% 
  filter(state == "stroke") %>%
  filter(time >= 10) %>% 
  group_by(strata) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), function(x) round(x * 100, 1)), 
    cs_risk = paste0(estimate, " (", conf.low, ",", conf.high, ")"), 
    ethnicity_rev = str_split_i(strata, "=", 2)
  ) %>% 
  select(time, cs_risk, ethnicity_rev)

## by age group at baseline
cs_risk_combined_stroke_agegrp <- baseline_tte_data %>% 
  select(subjid, survey_age, 
         ethnicity_rev, fu_combined_stroke, combined_stroke_inc, end_type) %>% 
  mutate(
    survey_age_grp = case_when(
      survey_age < 70 ~ "<70", 
      survey_age >= 70 & survey_age < 80 ~ "70-80", 
      survey_age >= 80 ~ ">=80"
    ), 
    stroke_end = case_when(
      combined_stroke_inc == 0 & 
        end_type %in% c("ADMIN CENSORED", "END OF MEMBERSHIP") ~ 0, # censored
      combined_stroke_inc == 0 & 
        end_type %in% c("DEATH") ~ 2, # death, competing event
      combined_stroke_inc == 1 ~ 1 # stroke
    ) %>% factor(levels = 0:2, labels = c("censored", "stroke", "death"))
  ) %>% 
  survfit(Surv(fu_combined_stroke, stroke_end) ~ ethnicity_rev + strata(survey_age_grp), data = .) %>% 
  broom::tidy()

## plot 
cs_risk_combined_stroke_agegrp %>% 
  filter(state == "stroke") %>% 
  separate_wider_delim(strata, delim = ",", names = c("ethnicity_rev", "age_grp")) %>% 
  mutate(
    ethnicity_rev = str_sub(ethnicity_rev, start = 15),
    age_grp = str_sub(age_grp, start = 25) %>% str_trim() %>% 
      factor(levels = c(">=80", "70-80", "<70")),
    across(c(estimate, conf.low, conf.high), function(x) x * 100), 
  ) %>% 
  filter(
    ethnicity_rev %in% c("Chinese", "Filipino", "Japanese", "South Asian", "White")
  ) %>% 
  ggplot(aes(x = time, y = estimate, group = age_grp)) + 
  geom_line(aes(color = age_grp)) + 
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = age_grp), 
    alpha = 0.2, color = NA
  ) + 
  scale_x_continuous(limits = c(0, 10)) + 
  labs(
    x = "Follow-up time (years)", y = "Cumulative incidence (%)", 
    color = "Baseline age", fill = "Baseline age"
  ) + 
  facet_wrap(~ ethnicity_rev) + 
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.2)
  )

ggsave(
  filename = here("03_figs", "cause-spec_inc_combined_stroke.png"),
  height = 4, width = 7, units = "in"
)

### ischemic stroke (including acvd) ----
cs_risk_isd_acvd <- baseline_tte_data %>% 
  select(subjid, ethnicity_rev, fu_isd_acvd, isd_acvd_inc, end_type) %>% 
  mutate(
    stroke_end = case_when(
      isd_acvd_inc == 0 & 
        end_type %in% c("ADMIN CENSORED", "END OF MEMBERSHIP") ~ 0, # censored
      isd_acvd_inc == 0 & 
        end_type %in% c("DEATH") ~ 2, # death, competing event
      isd_acvd_inc == 1 ~ 1 # stroke
    ) %>% factor(levels = 0:2, labels = c("censored", "stroke", "death"))
  ) %>% 
  survfit(Surv(fu_isd_acvd, stroke_end) ~ ethnicity_rev, data = .) %>% 
  broom::tidy() %>% 
  filter(state == "stroke") %>%
  filter(time >= 10) %>% 
  group_by(strata) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), function(x) round(x * 100, 1)), 
    cs_risk = paste0(estimate, " (", conf.low, ",", conf.high, ")"), 
    ethnicity_rev = str_split_i(strata, "=", 2)
  ) %>% 
  select(time, cs_risk, ethnicity_rev) 

### hemorrhagic stroke ----
cs_risk_hstroke <- baseline_tte_data %>% 
  select(subjid, ethnicity_rev, fu_hstroke_noties, hstroke_noties_inc, end_type) %>% 
  mutate(
    stroke_end = case_when(
      hstroke_noties_inc == 0 & 
        end_type %in% c("ADMIN CENSORED", "END OF MEMBERSHIP") ~ 0, # censored
      hstroke_noties_inc == 0 & 
        end_type %in% c("DEATH") ~ 2, # death, competing event
      hstroke_noties_inc == 1 ~ 1 # stroke
    ) %>% factor(levels = 0:2, labels = c("censored", "stroke", "death"))
  ) %>% 
  survfit(Surv(fu_hstroke_noties, stroke_end) ~ ethnicity_rev, data = .) %>% 
  broom::tidy() %>%
  filter(state == "stroke") %>%
  filter(time >= 10) %>% 
  group_by(strata) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), function(x) round(x * 100, 1)), 
    cs_risk = paste0(estimate, " (", conf.low, ",", conf.high, ")"), 
    ethnicity_rev = str_split_i(strata, "=", 2)
  ) %>% 
  select(time, cs_risk, ethnicity_rev) 


### format and save ---------------------------

# list(
#   "combined_stroke" = cs_risk_combined_stroke,
#   "isd_acvd" = cs_risk_isd_acvd,
#   "hstroke" = cs_risk_hstroke
# ) %>% 
#   bind_rows(.id = "stroke_defn") %>% 
#   select(-time) %>% 
#   pivot_wider(
#     id_cols = ethnicity_rev,
#     names_from = stroke_defn,
#     names_prefix = "cs_risk_",
#     values_from = cs_risk
#   ) %>% 
#   right_join(stroke_summary) %>% 
#   # reorder the columns
#   select(
#     ethnicity_rev, N_total,
#     N_combined_stroke, combined_tte, combined_tte_r, cs_risk_combined_stroke, 
#     N_isd_acvd, isd_acvd_tte, isd_acvd_tte_r, cs_risk_isd_acvd, 
#     N_hstroke_noties, hstroke_tte, hstroke_tte_r, cs_risk_hstroke
#   ) %>% 
#   write.xlsx(here("02_outputs", "table_2_stroke_summary.xlsx"))

list(
  "combined_stroke" = cs_risk_combined_stroke,
  "isd_acvd" = cs_risk_isd_acvd,
  "hstroke" = cs_risk_hstroke
) %>% 
  bind_rows(.id = "stroke_defn") %>% 
  select(-time) %>% 
  pivot_wider(
    id_cols = ethnicity_rev,
    names_from = stroke_defn,
    names_prefix = "cs_risk_",
    values_from = cs_risk
  ) %>% 
  right_join(stroke_summary) %>% 
  # reorder the columns
  select(
    ethnicity_rev, N_total,
    N_combined_stroke, cs_risk_combined_stroke, 
    N_isd_acvd, cs_risk_isd_acvd, 
    N_hstroke_noties, cs_risk_hstroke
  ) %>% 
  arrange(desc(N_total)) %>% 
  slice(lead(row_number(), default = 1)) %>% 
  mutate(ethnicity_rev = paste0(ethnicity_rev, " (", N_total, ")")) %>% 
  select(-N_total) %>% 
  mutate(perc_hstroke_noties = round(N_hstroke_noties / N_combined_stroke * 100, 0)) %>% 
  write.xlsx(here("02_outputs", "table_e5_stroke_summary_shorter.xlsx"))

# Table e6 dementia and death summary -----------------------------------------------

summarize_dem_death <- function(stroke_type, to_round) {

  # # choose which stroke defn to stratify on
  # stroke_type <- "combined_stroke" 
  # # or
  # stroke_type <- "isd_acvd"
  # stroke_type <- "hstroke_noties"
  # 
  # # choose whether to use exact or rounded event ages
  # to_round <- FALSE
  # # or
  # # to_round <- TRUE 
  
  # set variable names
  end <- paste0("end_age", ifelse(to_round, "_r", ""))
  survey <- paste0("survey_age", ifelse(to_round, "_r", ""))
  stroke_age <- paste0(stroke_type, ifelse(to_round, "_r", ""))
  stroke_inc <- case_when(
    stroke_type == "combined_stroke" ~ "combined_stroke_inc", 
    stroke_type == "isd_acvd" ~ "isd_acvd_inc", 
    stroke_type == "hstroke_noties" ~ "hstroke_noties_inc", 
  )
  
  # among those with inc dementia only, get median time and IQR to inc dementia
  dem_tte <- baseline_tte_data %>% 
    filter(end_type == "DEMENTIA") %>% 
    mutate(fu_time = get(end) - get(survey))
  
  dem_death_summary <- dem_tte %>% 
    group_by(ethnicity_rev) %>% 
    tally() %>% 
    rename("N of dementia cases" = n)
  
  median_dem <- tbl_survfit(
    survfit(Surv(fu_time, end_type == "DEMENTIA") ~ ethnicity_rev, data = dem_tte),
    probs = c(0.5, 0.25, 0.75),
    statistic = "{estimate}"
  ) %>% 
    as_tibble(col_labels = FALSE) %>% 
    mutate(median_IQR = paste0(stat_1, " (", stat_2, ", ", stat_3, ")")) %>% 
    select(label, median_IQR)
  
  dem_death_summary <- median_dem[-1, ] %>% 
    as.tibble() %>% 
    setNames(c("ethnicity_rev", "Median time (IQR) to dementia")) %>% 
    right_join(dem_death_summary) %>% 
    relocate(`Median time (IQR) to dementia`, .after = everything())
  
  # among dementia cases after stroke, get median time and IQR to inc dementia
  dem_after_stroke <- baseline_tte_data %>% 
    filter(
      end_type == "DEMENTIA",
      get(stroke_inc) == 1, 
      get(stroke_age) < get(end) 
      # using rounded age here would match with analysis, 
      # but that also means some granularity is lost here, 
      # for those who developed both in the same year
    )
  
  dem_death_summary <- dem_after_stroke %>% 
    group_by(ethnicity_rev, .drop = FALSE) %>% tally() %>% 
    rename("N dementia cases after stroke" = n) %>% 
    right_join(dem_death_summary) %>% 
    relocate(`N dementia cases after stroke`, .after = everything())

  median_dem_after_stroke <- tbl_survfit(
    survfit(Surv(get(end) - get(stroke_age), 
                 end_type == "DEMENTIA") ~ ethnicity_rev, data = dem_after_stroke), 
    probs = c(0.5, 0.25, 0.75),
    statistic = "{estimate}"
  ) %>% 
    as_tibble(col_labels = FALSE) %>% 
    mutate(median_IQR = paste0(stat_1, " (", stat_2, ", ", stat_3, ")")) %>% 
    select(label, median_IQR)
  
  dem_death_summary <- median_dem_after_stroke[-1, ] %>% 
    as.tibble() %>% 
    setNames(c("ethnicity_rev", "Median time (IQR) to dementia after stroke")) %>% 
    right_join(dem_death_summary) %>% 
    relocate(`Median time (IQR) to dementia after stroke`, .after = everything())
  
  # among death cases after (i.e. with) stroke, get median time and IQR to death
  death_after_stroke <- baseline_tte_data %>%  
    filter(end_type == "DEATH", 
           get(stroke_inc) == 1,
           # subset to death cases strictly after stroke
           get(stroke_age) < get(end))

  dem_death_summary <- death_after_stroke %>% 
    group_by(ethnicity_rev, .drop = FALSE) %>% tally() %>% 
    rename("N deaths after stroke" = n) %>% 
    right_join(dem_death_summary) %>% 
    relocate(`N deaths after stroke`, .after = everything())
  
  median_death_after_stroke <- tbl_survfit(
    survfit(Surv(get(end) - get(stroke_age), 
                 end_type == "DEATH") ~ ethnicity_rev, data = death_after_stroke), 
    probs = c(0.5, 0.25, 0.75),
    statistic = "{estimate}"
  ) %>% 
    as_tibble(col_labels = FALSE) %>% 
    mutate(median_IQR = paste0(stat_1, " (", stat_2, ", ", stat_3, ")")) %>% 
    select(label, median_IQR)
  
  dem_death_summary <- median_death_after_stroke[-1, ] %>% 
    as.tibble() %>% 
    setNames(c("ethnicity_rev", "Median time (IQR) to death after stroke")) %>% 
    right_join(dem_death_summary) %>% 
    relocate(`Median time (IQR) to death after stroke`, .after = everything())
  
  # # among death cases before (i.e. without) stroke, get median time and IQR to death
  # death_before_stroke <- baseline_tte_data %>% 
  #   filter(end_type == "DEATH", get(stroke_inc) == 0)
  # 
  # dem_death_summary <- death_before_stroke %>% 
  #   group_by(ethnicity_rev, .drop = FALSE) %>% tally() %>% 
  #   rename("N deaths before stroke" = n) %>% 
  #   right_join(dem_death_summary) %>% 
  #   relocate(`N deaths before stroke`, .after = everything())
  # 
  # median_death_before_stroke <- tbl_survfit(
  #   survfit(Surv(get(end) - get(survey), end_type == "DEATH") ~ ethnicity_rev, 
  #           data = death_before_stroke), 
  #   probs = c(0.5, 0.25, 0.75),
  #   statistic = "{estimate}"
  # ) %>% 
  #   as_tibble(col_labels = FALSE) %>% 
  #   mutate(median_IQR = paste0(stat_1, " (", stat_2, ", ", stat_3, ")")) %>% 
  #   select(label, median_IQR)
  # 
  # dem_death_summary <- median_death_before_stroke[-1, ] %>% 
  #   as.tibble() %>% 
  #   setNames(c("ethnicity_rev", "Median time (IQR) to death before stroke")) %>% 
  #   right_join(dem_death_summary) %>% 
  #   relocate(`Median time (IQR) to death before stroke`, .after = everything())
  
  return(dem_death_summary)
  
}

list(
  "combined_stroke exact" = 
    summarize_dem_death(stroke_type = "combined_stroke", to_round = FALSE),
  "combined_stroke rounded" = 
    summarize_dem_death(stroke_type = "combined_stroke", to_round = TRUE),
  "isd_acvd exact" = 
    summarize_dem_death(stroke_type = "isd_acvd", to_round = FALSE),
  "isd_acvd rounded" = 
    summarize_dem_death(stroke_type = "isd_acvd", to_round = TRUE),
  "hstroke_noties exact" = 
    summarize_dem_death(stroke_type = "hstroke_noties", to_round = FALSE),
  "hstroke_noties rounded" = 
    summarize_dem_death(stroke_type = "hstroke_noties", to_round = TRUE)
) %>% 
  write.xlsx(
    file = here("02_outputs", "table_e6_dem_death_summary.xlsx")
  )

# number of repeated measurements summary ----

load(paste0(data_folder,
            "aa_stroke_dementia/final_datasets/",
            "summary_n_bp_lipid_measures.RData"))

n_summary <- n_summary %>% 
  rename_all(tolower) %>% 
  right_join(baseline_tte_data %>% select(subjid), by = "subjid") %>% 
  mutate(across(c(bp, hdl), function(x) replace_na(x, 0))) 

n_median <- n_summary %>% 
  group_by(subjid) %>%
  summarise(across(c(bp, hdl), median)) %>% 
  ungroup() %>% 
  left_join(baseline_tte_data %>% select(subjid, ethnicity_rev), by = "subjid")

# proportion of subjects whose median number of measurements per year is greater than 1
n_median %>% 
  group_by(ethnicity_rev) %>% 
  summarise(across(c(bp, hdl), function(x) sum(x > 1) / n()))
# >80% (except for other SE Asian) across all ethnicities for blood pressure
# 12 - 25% for lipid panel: in fact, most people only have 1 lipid panel per year

n_median %>% 
  ggplot(aes(x = hdl)) + 
  geom_density() + 
  facet_wrap(~ethnicity_rev) + 
  lims(x = c(0, 4))

