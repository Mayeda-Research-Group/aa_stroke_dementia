# This script derives the three definitions of stroke incidence 
# which is used in later analysis as time-varying exposure. 

# combined_stroke is used as the main defn of exposure
# while isd_acvd is used in the sensitivity analysis. 

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

stroke_data <- read_sas(paste0(data_folder,
                               "raw_data_tables/Membership_EHR_mortality_data/",
                               "c10049_all_obs_dx.sas7bdat"))

# basic cleaning
stroke_sub <- stroke_data %>% 
  mutate(
    SUBJID = as.numeric(SUBJID),
    # calculate age at event
    STROKE_AGE = ifelse(
      as.Date(DX_DATE) == "1600-01-01", 90,
      interval(as.Date("1900-01-01"), as.Date(DX_DATE)) / years(1)
    )
  ) %>% 
  # exclude subjects not in RPGEH
  filter(SUBJID %in% tte_data$SUBJID) %>% 
  select(SUBJID, STROKE_AGE, everything(), -DX_DATE) %>% 
  arrange(SUBJID, STROKE_AGE)


stroke_sub <- stroke_sub %>% janitor::clean_names()


# Min age of stroke subtypes ----------------------------------------------

## GOAL: to create variables that have the min_age of diagnosis for each stroke subtype


# convert to long format all stroke subtype variables
stroke_long <- stroke_sub %>% 
  pivot_longer(
    cols = c(3:7),
    names_to = "stroke_subtype",
    values_to = "indicator")

# filter rows that do have a stroke subtype

stroke_long <- stroke_long %>% 
  filter(!is.na(indicator))

# summarize the min age of stroke, for each id and each subtype

stroke_long_minage <- stroke_long %>% 
  group_by(subjid, stroke_subtype) %>% 
  summarize(min_age = min(stroke_age)) %>% 
  ungroup()

## convert to a wide format

stroke_wide_minage <- stroke_long_minage %>% 
  pivot_wider(
    names_from = stroke_subtype,
    values_from = min_age)

# saveRDS(stroke_wide_minage, 
#         paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#                "stroke_wide_minage.RDS"))

# Create stroke subtypes --------------------------------------------------


# 1. Create a combined category for ACVD and ischemic stroke,
# but prioritize ischemic stroke time, not the min between these 2

stroke_wide_minage <- stroke_wide_minage %>% 
  mutate(
    isd_acvd = case_when(
      !is.na(isd) ~ isd,
      is.na(isd) & !is.na(acvd) ~ acvd
    )
  )

# Previous code (below) defined the min time between ACVD and isd, the overlap between
# definitions is really good

# stroke_wide_minage <- stroke_wide_minage %>%
#   mutate(
#     isd_acvd2 = case_when(
#       !is.na(isd) & !is.na(acvd) ~ pmin(acvd, isd),
#       !is.na(isd) & is.na(acvd) ~ isd,
#       is.na(isd) & !is.na(acvd) ~ acvd,
#       ))
# 
# stroke_wide_minage %>%
#   ggplot(aes(isd_acvd2)) +
#   geom_density(alpha = 0.2, fill =
#                   "red") +
#   geom_density(aes(stroke_wide_minage$isd_acvd),
#                  alpha = 0.2, fill = "blue")


# 2. For hstroke subtype, if their min age of hstroke is equal to the min age of 
# isd, then prioritize isd and not hstroke, this should not affect any variables
# that combine hstroke and ischemic

stroke_wide_minage %>% count(!is.na(hstroke))

stroke_wide_minage %>% count(!is.na(isd), !is.na(hstroke)) 

stroke_wide_minage %>% count(isd == hstroke)  

stroke_wide_minage <- stroke_wide_minage %>% 
  mutate(
    hstroke_noties = case_when(
      is.na(isd) & is.na(hstroke) ~ NA_real_,
      (!is.na(isd) & !is.na(hstroke)) & isd == hstroke ~ NA_real_,
      TRUE ~ hstroke
    )
  )

stroke_wide_minage %>% count(!is.na(hstroke), !is.na(hstroke_noties))

stroke_wide_minage %>% count(!is.na(hstroke_noties))

# 3. Create a combined stroke, no TIA and prioritize hierarchy of ISD > HStroke > ACVD

stroke_wide_minage <- stroke_wide_minage %>% 
  mutate(
    combined_stroke = case_when(
      !is.na(isd) ~ isd,
      is.na(isd) & !is.na(hstroke) ~ hstroke,
      is.na(isd) & is.na(hstroke) ~ acvd
    )
  )

# Previous code (below) defined the min time between ACVD and isd and hstroke,
# the overlap between definitions is really good

# stroke_wide_minage <- stroke_wide_minage %>%
#   mutate(
#     combined_stroke2 = case_when(
#       !is.na(hstroke) & !is.na(isd_acvd) ~ pmin(hstroke, isd_acvd),
#       !is.na(hstroke) & is.na(isd_acvd) ~ hstroke,
#       is.na(hstroke) & !is.na(isd_acvd) ~ isd_acvd
#     )
#   )
# # Comparing if there is large difference the 2 definitions, not at all
# stroke_wide_minage %>%
#   ggplot(aes(combined_stroke2)) +
#   geom_density(alpha = 0.2, fill =
#                   "red") +
#   geom_density(aes(stroke_wide_minage$combined_stroke),
#                  alpha = 0.2, fill = "blue")

## 4. Create a combined stroke that excludes ACVD
stroke_wide_minage <- stroke_wide_minage %>% 
  mutate(
    combined_isd_hstroke = case_when(
      !is.na(isd) ~ isd,
      is.na(isd) & !is.na(hstroke) ~ hstroke)
  )

stroke_wide_minage %>% count(!is.na(combined_isd_hstroke))

## save file

saveRDS(stroke_wide_minage, paste0(data_folder, "aa_stroke_dementia/final_datasets/",
                                 "stroke.RDS"))

# Descriptive information -------------------------------------------------


stroke_wide_minage %>%
  count(!is.na(combined_stroke)) # 34598 combined stroke cases

stroke_wide_minage %>%
  count(!is.na(combined_isd_hstroke)) # 31930 stroke cases

stroke_wide_minage %>%
  count(!is.na(isd_acvd)) # 30101 isd & acvd cases

stroke_wide_minage %>%
  count(!is.na(isd)) # 27012 only isd cases

stroke_wide_minage %>%
  count(!is.na(hstroke_noties)) # 7597 hstrokes

stroke_wide_minage %>% 
  ggplot(aes(isd_acvd)) +
  geom_density(alpha = 0.5, fill = "red") +
  geom_density(aes(hstroke_noties), alpha = 0.5, fill = "blue")
