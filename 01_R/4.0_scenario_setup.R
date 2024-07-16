library(tidyverse)
library(here)


# initial run -------------------------------------------------------------

n_bootstraps <- 1000

# crude models
scenarios_crude <- expand_grid(
  model = c("crude"),
  imp = 1:5,
  boot_i = c(paste0("0-", n_bootstraps + 100)),
  stroke_defn = c("stroke_combined", "stroke_isd_acvd")
) %>% 
  separate(boot_i, into = c("boot_i_start", "boot_i_end"), sep = "-") %>% 
  arrange(stroke_defn)

# weighted models
n_per_job <- 100

scenarios_weighted <- expand_grid(
  model = c("weighted"), 
  imp = 1:5, 
  boot_i = c(paste0("0-", n_per_job), 
             rep(paste0("1-", n_per_job), n_bootstraps / n_per_job)),
  stroke_defn = c("stroke_combined", "stroke_isd_acvd")
) %>% 
  separate(boot_i, into = c("boot_i_start", "boot_i_end"), sep = "-") %>% 
  arrange(stroke_defn)
  

# this is used for the first successful run 
# but only the crude models run completely
scenario_lookup <- rbind(scenarios_crude, scenarios_weighted)
  
write_csv(
  scenario_lookup,
  here("01_R", "bootstrap_scenarios_full.csv")
)

# # new job submission for only weighted jobs
# write_csv(
#   scenarios_weighted,
#   here("01_R", "bootstrap_scenarios.csv")
# )

# scenario_lookup <- scenario_lookup %>% 
#   mutate(boot_i_end = 2)


# # additional run to make up the missing bootstraps ------------------------
# 
# scenarios_add <- tibble(
#   model = rep("weighted", 4), 
#   imp = c(1, 4, 4, 5), 
#   boot_i_start = rep(1, 4), 
#   boot_i_end = c(50, 50, 10, 10), 
#   ethn = c("South Asian", "South Asian", "White", "White")
# )
# 
# write_csv(
#   scenarios_add,
#   here("01_R", "bootstrap_scenarios_add.csv")
# )



