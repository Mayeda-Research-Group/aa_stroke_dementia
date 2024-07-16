# overall description: 
# take one imputed copy of the data
# iterate over the ethnicities by 
# isolating one ethnicity and running bootstrap sampling 100 times
# collect (1) KM results
# and (2) warnings and running time by ethnicity and imputation

# load libraries ----------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(survival)
library(splines)
library(broom)
# library(here) 

# set up paths ------------------------------------------------------------

## paths for running on own computer ----
# source(here("01_R", "path.R"))
# data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")
# 
# path_to_output <- paste0(
#   data_folder, "aa_stroke_dementia/final_datasets/",
#   "results_wt_add/"
# )
# 
# weighting_formulas <- readRDS(
#   paste0(data_folder, "aa_stroke_dementia/final_datasets/",
#          "weighting_formulas.RDS")
# )
# 
# # load functions
# source(here("01_R", "helper_functions.R"))

## paths for running on Hoffman ----
project_folder <- paste0("/u/home/y/yixzhou/AA_stroke_dem/")
data_folder <- paste0("/u/home/y/yixzhou/AA_stroke_dem/data/")
path_to_output <- paste0("/u/home/y/yixzhou/AA_stroke_dem/results/")

# load formulas and helper functions
weighting_formulas <- readRDS(paste0(data_folder, "weighting_formulas.RDS"))
source(paste0(project_folder, "helper_functions.R"))

# Hoffman setup -----------------------------------------------------------

# Read in the arguments listed in the:
# R CMD BATCH --no-save --no-restore "--args scenario_num=$SGE_TASK_ID"  
## expression:

args = (commandArgs(TRUE))

# Check to see if arguments are passed and set default values if not.
# If so, parse the arguments. (I only have one argument here.)
if (length(args) == 0) {
  print("No arguments supplied.")
  ##supply default values
  scenario_num <- 1
} else {
  for (i in 1:length(args)) {
    eval(parse(text = args[[i]])) # parse each argument (only have 1 here)
  }
}


# interpret the scenario --------------------------------------------------

# set up the type of model and the imputations using scenario_num
# scenario_num <- 1

scenario_lookup <- read_csv(
  # here("01_R", "bootstrap_scenarios_full.csv") # laptop
  paste0(data_folder, "bootstrap_scenarios_full.csv") # Hoffman
)

print(nrow(scenario_lookup))

# also make sure that scenario_num is not greater than 10
if (scenario_num > nrow(scenario_lookup)) {
  print("out of bound senario number")
  # supply default value
  scenario_num <- 1
}

# print values just to make sure:
print(scenario_num)

set.seed(scenario_num * 1000)

scenario <- scenario_lookup[scenario_num, ]
model <- scenario$model
imp <- scenario$imp
boot_i_start <- scenario$boot_i_start
boot_i_end <- scenario$boot_i_end
stroke_defn <- scenario$stroke_defn

print(model)
print(imp)
print(boot_i_start)
print(boot_i_end)
print(stroke_defn)



# bootstrapping function ----

# KM_boot calculates one line of statistics for one ethnicity and one imputation
# (1) ethnicity and model (crude or adjusted with IPTW and IPCW) - 2 trackers
# (2) CIF at Times 0 to 18 for the no stroke group - 19 statistics
# (3) CIF at Times 1 to 18 for the stroke group - 18 statistics
# (4) auxiliary variables from bootstrapping: 
#     t = index of bootstrap sample, imp = index of imputed dataset used
#     warning = indicator for potential glm warning 


# need a new resampling function that
# (1) samples IDs with replacement
# (2) construct the long data with these selected IDs (with possible repeats of IDs)
# (3) assign new ID's to make repeated subjects distinct



KM_boot <- function(data, formulas, 
                    model_type = c("crude", "weighted"), 
                    # crude: run crude KM and AJ curve
                    # weighted: run two KM and AJ curves, 
                    #           adjusted with IPTW only and both IPTW and IPCW
                    bootstrap = TRUE
                    # FALSE: obtain point estimate
                    # TRUE: run bootstrap resampling
                    ) {
  # # testing arguments
  # ethn <- "Filipino"
  # data <- input_data # input data should be filtered to a certain ethnicity
  # formulas <- weighting_formulas[["stroke_combined"]][[ethn]]
  # model_type <- "crude"
  
  # extract the stroke defn from the formula 
  # so that we know which defn we are working with 
  stroke_defn <- str_split_i(formulas$stroke[1], "~", 1)
  
  # the indices argument is set up for the boot function,
  # which samples subject ID's. 
  if (bootstrap) {
    indices <- sample(1:length(unique(data$subjid)), length(unique(data$subjid)), 
                      replace = TRUE)
  } else {
    indices <- 1:length(unique(data$subjid))
  }

  boot_ids <- unique(data$subjid)[indices] 
  
  # this step makes sure that subjects are sampled with replacement,
  # i.e. one subject can show up multiple times in the sampled dataset.
  # a new ID is created to distinguish between these repeats. 
  boot_data <- tibble(
    subjid = boot_ids, 
    new_id = 1:length(boot_ids)
  ) %>% 
    left_join(data, by = "subjid", relationship = "many-to-many") %>% 
    select(-subjid)
  
  # fit KM models and clean output
  if (model_type == "crude") {
    ## direct effect
    KM_output <- survfit(
      Surv(time = start, time2 = fu_yr, event = event_dem) ~ get(stroke_defn) + ethnicity_rev,
      data = boot_data, cluster = new_id
    ) %>%
      clean_KM(model_type = model_type, check = TRUE)
    
    ## total effect
    AJ_output <- survfit(
      Surv(time = start, time2 = fu_yr, 
           event = dem_death, type = 'mstate') ~ get(stroke_defn) + ethnicity_rev,
      data = boot_data,
      cluster = new_id,
      id = new_id
    ) %>% clean_AJ(model_type = model_type, check = TRUE)
    
    names(KM_output) <- c(names(KM_output)[1:2], 
                          paste0("KM_", names(KM_output)[-c(1,2)]))
    names(AJ_output) <- c(names(AJ_output)[1:2], 
                          paste0("AJ_", names(AJ_output)[-c(1,2)]))
    
    output <- c(KM_output, AJ_output[-c(1,2)])
    
  } else if (model_type == "weighted") {
    # generate weights
    boot_data <- generate_IPTW(boot_data, formulas$stroke)
    boot_data <- generate_IPCW(boot_data, formulas$death, formulas$endofMem)
    
    ## direct effect
    # weighted with IPTW only
    KM_output_1 <- boot_data %>% 
      survfit(
        Surv(time = start, time2 = fu_yr, event = event_dem) ~ get(stroke_defn) + ethnicity_rev,
        weights = wt_stroke_stab, data = ., cluster = new_id
      ) %>%
      clean_KM(model_type = "IPTW", check = TRUE)
    
    # weighted with both IPTW and IPCW
    KM_output_2 <- boot_data %>% 
      mutate(wt_both = wt_stroke_stab * wt_cens) %>% 
      survfit(
        Surv(time = start, time2 = fu_yr, event = event_dem) ~ get(stroke_defn) + ethnicity_rev,
        weights = wt_both, data = ., cluster = new_id
      ) %>%
      clean_KM(model_type = "IPTW + IPCW", check = TRUE)
    
    KM_output <- rbind(KM_output_1, KM_output_2)
    
    ## total effect
    AJ_output_1 <- boot_data %>% 
      survfit(
        Surv(time = start, time2 = fu_yr, 
             event = dem_death, type = 'mstate') ~ get(stroke_defn) + ethnicity_rev,
        data = .,
        cluster = new_id, id = new_id,
        weights = wt_stroke_stab
      ) %>% clean_AJ(model_type = "IPTW", check = TRUE)
    
    AJ_output_2 <- boot_data %>% 
      mutate(wt_both_AJ = wt_stroke_stab * wt_endmem) %>% 
      survfit(
        Surv(time = start, time2 = fu_yr, 
             event = dem_death, type = 'mstate') ~ get(stroke_defn) + ethnicity_rev,
        data = .,
        cluster = new_id,
        id = new_id,
        weights = wt_both_AJ
      ) %>% clean_AJ(model_type = "IPTW + IPCW", check = TRUE)
    
    AJ_output <- rbind(AJ_output_1, AJ_output_2)
    
    colnames(KM_output) <- c(colnames(KM_output)[1:2], 
                             paste0("KM_", colnames(KM_output)[-c(1,2)]))
    colnames(AJ_output) <- c(colnames(AJ_output)[1:2], 
                             paste0("AJ_", colnames(AJ_output)[-c(1,2)]))
    
    output <- cbind(KM_output, AJ_output[, -c(1,2)])
    
  }
  
  return(output)
}



# start bootstrap ---------------------------------------------------------

ethns_list <- names(weighting_formulas[[stroke_defn]])
ethns_list <- ethns_list[c(2:10, 1)]

# load one imputation
long_tte_data <- readRDS(
  # paste0(data_folder, "aa_stroke_dementia/final_datasets/",
  #        "combined_imp_", imp, ".RDS") # desktop
  paste0(data_folder, "combined_imp_", imp, ".RDS") # Hoffman
)

# new event definition for Aalen Johansen estimator
long_tte_data <- long_tte_data %>% 
  mutate(
    dem_death = case_when(
      event_dem == 1 ~ 1, # dementia
      event_death == 1 ~ 2, # death as a competing event
      TRUE ~ 0
    )
  )

for (ethn in ethns_list) {
  # ethn <- ethns_list[3]
  print(ethn)
  
  start <- Sys.time() # record starting time
  
  w_stroke_d <- w_stroke_n <- numeric()
  w_death_d <- w_death_n <- numeric()
  w_endofMem_d <- w_endofMem_n <- numeric()
  
  # subset to the specific ethnicity in the i-th imputed RPGEH dataset
  input_data <- long_tte_data %>% filter(ethnicity_rev == ethn)
  
  # run bootstrapping
  for (i_boot in boot_i_start:boot_i_end) {
    print(i_boot)
    # i_boot <- 0 # point estimate, using full dataset, no resampling
    # i_boot <- 1 # and so on: bootstrap
    res_boot_i <- KM_boot(input_data, 
                          weighting_formulas[[stroke_defn]][[ethn]], 
                          model,
                          bootstrap = i_boot != 0)
    
    # clean the format
    if (model == "crude") {
      res_boot_i <- c(res_boot_i, 
                      imp = imp, i_boot = i_boot, 
                      scenario_num = scenario_num,
                      stroke_defn = stroke_defn) %>% t()
      
    } else {
      res_boot_i <- cbind(res_boot_i, imp, i_boot, scenario_num, stroke_defn)
    }
    
    # collect output for a specific ethnicity in a given scenario
    if (i_boot == boot_i_start) {
      out <- res_boot_i
    } else {
      out <- rbind(out, res_boot_i)
    }
    
    # # only create new file when i_boot = index of first bootstrap
    # # append_ind <- i_boot != boot_i_start
    # append_ind <- file.exists(
    #   paste0(path_to_output, 
    #          "bootstraps_", model, 
    #          "_imp_", imp, 
    #          # "_scenario_", scenario_num, 
    #          "_", ethn, ".csv")
    # )
    # 
    # write_csv(
    #   res_boot_i %>% as.data.frame(), 
    #   append = append_ind, 
    #   file = paste0(path_to_output, 
    #                 "bootstraps_", model, 
    #                 "_imp_", imp, 
    #                 # "_scenario_", scenario_num, 
    #                 "_", ethn, ".csv")
    # )
  }
  
  end <- Sys.time() # record ending time
  
  
  # save warning indicator for each bootstrap
  w_index <- cbind(
    imp = imp, 
    ethnicity = ethn, 
    model = model, 
    i_boot = boot_i_start:boot_i_end, 
    scenario_num = scenario_num,
    w_stroke_d, w_stroke_n,
    w_death_d, w_death_n,
    w_endofMem_d, w_endofMem_n
  ) %>% as_tibble()
  # and join it with bootstrap output
  out <- out %>% as.data.frame() %>% 
    left_join(w_index, by = c("imp", "i_boot", "scenario_num"))
  
  # save the bootstraps
  write_csv(
    out,
    # only append when it's not the first ethnicity in the loop
    append = ethn != ethns_list[1], 
    file = paste0(path_to_output, 
                  "bootstraps_", model, 
                  "_", stroke_defn, 
                  "_scenario_", scenario_num, ".csv")
  )
  
  # save the number of warnings and duration of the run for review
  w_summary <- tibble(
    imp = imp, 
    ethnicity = ethn, 
    model = model, 
    scenario = scenario_num,
    n_w_stroke_d = sum(w_stroke_d), 
    n_w_stroke_n = sum(w_stroke_n), 
    n_w_death_d = sum(w_death_d), 
    n_w_death_n = sum(w_death_n), 
    n_w_endofMem_d = sum(w_endofMem_d), 
    n_w_endofMem_n = sum(w_endofMem_n), 
    duration = difftime(end, start, units = 'mins')
  )

  # write_csv(
  #   w_index, 
  #   # only create file with it's the first imputed dataset and the first ethnicity
  #   # append = !(imp == 1 & ethn == ethns_list[1]),
  #   append = file.exists(paste0(path_to_output, "warning_index_", model, ".csv")),
  #   file = paste0(path_to_output, "warning_index_", model, ".csv")
  # )
  
  write_csv(
    w_summary,
    # only create new file when it's the first ethnicity and i = 1
    # append = !(imp == 1 & ethn == ethns_list[1]),
    append = file.exists(paste0(path_to_output, "warning_summary_", model, ".csv")),
    file = paste0(path_to_output, "warning_summary_", model, ".csv")
  )
  
  
}


