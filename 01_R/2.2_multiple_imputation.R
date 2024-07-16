
# set up packages and paths ----
library(tidyverse)
library(mice)
library(miceadds) # this is crucial!!
library(lme4) # also needed for multilevel imputation

# parse the argument ----
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
# print values just to make sure:
print(scenario_num)
seed <- scenario_num * 1000


# load the objects for MI ----

# paths on laptop
# data_folder <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/"
# load(paste0(data_folder, "aa_stroke_dementia/final_datasets/", 
#             "pre_MI_objects_Hoffman.RData"))

# paths on Hoffman
data_folder <- "/u/home/y/yixzhou/AA_stroke_dem/data/"
load(paste0(data_folder, "pre_MI_objects_Hoffman.RData"))


# run MI ----

start <- Sys.time()
imp1 <- mice::mice(
  long_data_pre_MI,
  m = 1,
  predictorMatrix = predM1,
  method = impM1,
  maxit = 1,
  imputationFunction = imputationFunction,
  cluster_var = cluster_var,
  seed = seed, 
)
end <- Sys.time()

date <- str_sub(start, end = 10)
runtime <- tibble(scenario = scenario_num, time = difftime(end, start, units = 'mins'))

# save the imputed data ----

write_csv(runtime, 
          file = paste0(data_folder, "MI_runtime.csv"), 
          append = scenario_num != 1)

saveRDS(imp1, 
        file = paste0(data_folder, 
                      "multilevel_imp_", date, "_seed_", seed, ".RDS"))


