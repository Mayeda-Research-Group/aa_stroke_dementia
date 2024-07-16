# Set up libraries and paths ----------------------------------------------

if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load(
  "tidyverse", "here", 
  "survival", "survminer", 
  "table1", "openxlsx", "gtsummary",
  "haven"
  # "mice", "miceadds", "lme4"
)

source(here("01_R", "path.R"))

data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")


# stroke: ischemic and hemorrhagic ----------------------------------------
# check how many subjects had dx of ischemic stroke and hemorrhagic stroke 
# occurring on the same date

# this wide dataset has the exact age at stroke dx (all three definitions)
baseline_tte_data <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "baseline_tte_data_rounded_by_yr.RDS")
)

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

# the resulting dataset generated from Script 0.3
stroke_wide_minage <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "stroke.RDS")
)

# only look at those in the analytic dataset
stroke_wide_minage <- baseline_tte_data %>% 
  select(subjid, ethnicity_rev, ends_with("_inc")) %>% 
  left_join(stroke_wide_minage, by = "subjid") %>% 
  filter(ethnicity_rev %in% c("Chinese", "Filipino", "Japanese", "South Asian", "White"))

stroke_wide_minage <- stroke_wide_minage %>% 
  rowwise() %>% 
  mutate(
    ever_stroke = ifelse(sum(is.na(hstroke), is.na(isd), is.na(acvd)) == 3, 0, 1)
  )

# combined stroke accounts for isd, hstroke, and acvd
stroke_wide_minage %>% with(table(combined_stroke_inc, ever_stroke, useNA = "ifany"))

# in the paper, we use combined stroke as the main defn
# ischemic stroke is defined by isd_acvd
# hemorrhagic stroke is defined by hstroke_noties

# relative to the actual stroke dx, how do these defns differ?

# these counts are in Table e-5
stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1) %>% 
  with(table(!is.na(isd_acvd), !is.na(hstroke_noties), ethnicity_rev, useNA = "ifany"))

# which type is used to define combined_stroke? 
stroke_wide_minage <- stroke_wide_minage %>% 
  # filter(combined_stroke_inc == 1) %>%
  # filter(!is.na(isd_acvd), !is.na(hstroke_noties)) %>% 
  mutate(
    which_used = case_when(
      is.na(isd) & is.na(hstroke) & is.na(acvd) ~ "no stroke", 
      !is.na(isd) ~ "isd",
      is.na(isd) & !is.na(hstroke) ~ "hstroke",
      is.na(isd) & is.na(hstroke) & !is.na(acvd) ~ "acvd"
    )
  ) 

stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1) %>%
  with(table(which_used, useNA = "ifany"))

stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1) %>% 
  with(table(which_used, isd_acvd_inc, ethnicity_rev, useNA = "ifany"))

stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1, ethnicity_rev == "White") %>% 
  with(table(isd_acvd_inc, which_used, useNA = "ifany"))

stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1, ethnicity_rev == "White") %>% 
  with(table(hstroke_noties_inc, which_used, useNA = "ifany"))

stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1) %>% 
  filter(hstroke_noties_inc == 1, isd_acvd_inc == 1) %>% 
  with(table(which_used, useNA = "ifany"))
  # View()

# look at these people who have both isd_acvd and hstroke_noties
# (i.e. the overlap in table e-5)
# 107 used hstroke to define combined stroke
# because these people only had acvd but not isd 
# 1489 used isd to define combined stroke
# because they all had isd 

# in the process of defining hstroke_noties, how many subjects 
# had hstroke and isd on the same day and therefore did not count towards
# hstroke_noties

stroke_wide_minage <- stroke_wide_minage %>% 
  mutate(
    # definition of hstroke_noties
    # hstroke_noties = case_when(
    #   is.na(isd) & is.na(hstroke) ~ NA_real_,
    #   (!is.na(isd) & !is.na(hstroke)) & isd == hstroke ~ NA_real_,
    #   TRUE ~ hstroke
    # )
    excluded_from_hstroke_noties = case_when(
      is.na(isd) & is.na(hstroke) ~ "neither isd nor hstroke",
      (!is.na(isd) & !is.na(hstroke)) & isd == hstroke ~ "excluded from hstroke_noties for having both isd and hstroke on same day",
      !is.na(hstroke) ~ "hstroke_noties", 
      TRUE ~ "no hstroke"
    )
  ) 

stroke_wide_minage %>% with(table(excluded_from_hstroke_noties, hstroke_noties_inc))
# among those who we define as not having hstroke_noties
# a marjority is due to the fact that they do not have hstroke
# a small percentage (n = 1040) is excluded from hstroke_noties because
# they had isd and hstroke on the same day

# break down by ethnicity
hstroke_ties <- stroke_wide_minage %>% with(table(excluded_from_hstroke_noties, ethnicity_rev))
hstroke_ties[, 1:5] 

stroke_wide_minage %>% 
  filter(combined_stroke_inc == 1) %>% 
  group_by(ethnicity_rev) %>% 
  reframe(
    n_combined_stroke = n(), 
    n_hstroke_noties = sum(hstroke_noties_inc),
    n_excluded_for_ties = sum(excluded_from_hstroke_noties == 
                                "excluded from hstroke_noties for having both isd and hstroke on same day")
  ) %>% 
  ungroup() %>% 
  mutate(
    perc_excluded = n_excluded_for_ties / n_combined_stroke * 100,
    ethnicity_rev = factor(ethnicity_rev, levels = c("Chinese", "Filipino", "Japanese", "South Asian", "White"))
  ) %>% 
  arrange(ethnicity_rev) %>% 
  write.xlsx(here("02_outputs", "n_ties_excluded_from_hstroke_noties.xlsx"))


