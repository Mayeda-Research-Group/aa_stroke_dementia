# load libraries ----------------------------------------------------------

library(tidyverse)
library(openxlsx)
# library(survival)
# library(splines)
# library(broom)
library(here)

# read in results ------------------------------------------------------------

source(here("01_R", "path.R"))

scenarios_full <- read_csv(here("01_R", "bootstrap_scenarios_full.csv"))

results_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/", 
                         "aa_stroke_dementia/final_datasets/results/")

crude_results <- list.files(
  results_folder, 
  # pattern = "bootstraps_crude_stroke_combined",
  # pattern = "bootstraps_crude_stroke_isd_acvd",
  pattern = "bootstraps_crude",
  full.names = TRUE
) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  filter(ethnicity %in% c("White", "Chinese", "Filipino", "Japanese", "South Asian"))

weighted_results <- list.files(
  results_folder, 
  # pattern = "bootstraps_weighted_stroke_combined",
  # pattern = "bootstraps_weighted_stroke_isd_acvd",
  pattern = "bootstraps_weighted",
  full.names = TRUE
) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  filter(ethnicity %in% c("White", "Chinese", "Filipino", "Japanese", "South Asian"))


# check results -----------------------------------------------------------

crude_results %>% 
  group_by(imp, stroke_defn, ethnicity, Model) %>% 
  count() %>% View()

weighted_results %>% 
  group_by(imp, stroke_defn, ethnicity, Model) %>% 
  count() %>% View()

# I'm not sure why some scenarios did not run for White
scenarios_run <- weighted_results %>% 
  filter(ethnicity == "White") %>% 
  distinct(scenario_num)

scenarios_missing <- scenarios_full[c(11:120)[!c(11:120) %in% scenarios_run$scenario_num], ]
# I'm going to make it 50 bootstraps for each job and double the number of jobs
scenarios_missing <- rbind(scenarios_missing, scenarios_missing) %>% 
  mutate(boot_i_end = boot_i_end/2)
# need to rerun these bootstraps

# write_csv(scenarios_missing, 
#           here("01_R", "bootstrap_scenarios_missing.csv"))


# read in missing runs ----------------------------------------------------

results_missingruns <- paste0(path_to_box, "Asian_Americans_dementia_data/", 
                              "aa_stroke_dementia/final_datasets/results_missingruns/")

weighted_results_missingruns <- list.files(
  results_missingruns, 
  pattern = "bootstraps_weighted",
  full.names = TRUE
) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) 

# imp 4 has two point estimates (i_boot = 0)
weighted_results_missingruns %>% 
  group_by(imp, scenario_num, stroke_defn, ethnicity, Model) %>% 
  count() %>% View()

# make sure to only keep one of them
weighted_results_missingruns <- weighted_results_missingruns %>% 
  filter(!(imp == 4 & scenario_num == 24 & i_boot == 0)) 

weighted_results <- rbind(
  weighted_results, weighted_results_missingruns
) 

# check again to make sure we have enough bootstraps
weighted_results %>% 
  group_by(imp, stroke_defn, ethnicity, Model) %>% 
  count() %>% View()
# looks good


# clean up results --------------------------------------------------------

weighted_results <- weighted_results %>% 
  # remove bootstraps with glm errors
  filter(
    w_stroke_d == 0, w_stroke_n == 0, 
    w_death_d == 0, w_death_n == 0, 
    w_endofMem_d == 0, w_endofMem_n == 0
  ) %>% 
  select(-starts_with("w_"), -model) %>% 
  # slice the point estimate (i_boot = 0) and the first 1000 bootstraps 
  # for each stroke defn, ethnicity, and imputed dataset
  group_by(stroke_defn, imp, Ethnicity, Model) %>% 
  slice_head(n = 1001)


# calculate RR RD ---------------------------------------------------------

crude_results <- crude_results %>% 
  ungroup() %>% 
  mutate(
    KM_RR_10 = KM_Stroke_1_time_10 / KM_Stroke_0_time_10,
    KM_RD_10 = (KM_Stroke_1_time_10 - KM_Stroke_0_time_10) * 100,
    AJ_RR_10 = AJ_Stroke_1_time_10 / AJ_Stroke_0_time_10,
    AJ_RD_10 = (AJ_Stroke_1_time_10 - AJ_Stroke_0_time_10) * 100
  ) %>% 
  select(-scenario_num)


weighted_results <- weighted_results %>%
  mutate(
    KM_RR_10 = KM_Stroke_1_time_10 / KM_Stroke_0_time_10,
    KM_RD_10 = (KM_Stroke_1_time_10 - KM_Stroke_0_time_10) * 100,
    AJ_RR_10 = AJ_Stroke_1_time_10 / AJ_Stroke_0_time_10,
    AJ_RD_10 = (AJ_Stroke_1_time_10 - AJ_Stroke_0_time_10) * 100
  ) %>% 
  select(-scenario_num)

for (yr in c(2,4,6,8)) {
  # yr <- "2"
  
  crude_results <- crude_results %>% 
    mutate(
      !! paste0("KM_RR_", yr) := get(paste0("KM_Stroke_1_time_", yr)) / get(paste0("KM_Stroke_0_time_", yr)),
      !! paste0("KM_RD_", yr) := (get(paste0("KM_Stroke_1_time_", yr)) - get(paste0("KM_Stroke_0_time_", yr))) * 100,
      !! paste0("AJ_RR_", yr) := get(paste0("AJ_Stroke_1_time_", yr)) / get(paste0("AJ_Stroke_0_time_", yr)),
      !! paste0("AJ_RD_", yr) := (get(paste0("AJ_Stroke_1_time_", yr)) - get(paste0("AJ_Stroke_0_time_", yr))) *100
    ) 
  
  weighted_results <- weighted_results %>% 
    mutate(
      !! paste0("KM_RR_", yr) := get(paste0("KM_Stroke_1_time_", yr)) / get(paste0("KM_Stroke_0_time_", yr)),
      !! paste0("KM_RD_", yr) := (get(paste0("KM_Stroke_1_time_", yr)) - get(paste0("KM_Stroke_0_time_", yr))) *100,
      !! paste0("AJ_RR_", yr) := get(paste0("AJ_Stroke_1_time_", yr)) / get(paste0("AJ_Stroke_0_time_", yr)),
      !! paste0("AJ_RD_", yr) := (get(paste0("AJ_Stroke_1_time_", yr)) - get(paste0("AJ_Stroke_0_time_", yr))) *100
    ) 
}

# calculate point est and CI ----------------------------------------------

ethns <- unique(crude_results$Ethnicity)

point_est <- rbind(
  crude_results %>% 
    filter(i_boot == 0, imp == 1) %>% 
    select(-imp, -i_boot, -ethnicity, -model),
  weighted_results %>% 
    filter(i_boot == 0) %>% 
    group_by(stroke_defn, Ethnicity, Model) %>% 
    summarise(across(c(starts_with("KM"), starts_with("AJ")), mean)) %>% 
    ungroup() 
)

crude_CI <- crude_results %>% 
  filter(i_boot != 0) %>% 
  group_by(stroke_defn, Ethnicity, Model) %>% 
  reframe(
    across(
      c(starts_with("KM"), starts_with("AJ")), 
      function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)
    )
  ) %>% 
  mutate(bound = rep(c("lower", "upper"), 2 * length(ethns)), .before = everything()) 

weighted_CI <- weighted_results %>% 
  filter(i_boot != 0) %>% 
  group_by(stroke_defn, Ethnicity, Model) %>% 
  reframe(
    across(
      c(starts_with("KM"), starts_with("AJ")), 
      function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)
    )
  ) %>% 
  mutate(bound = rep(c("lower", "upper"), 4 * length(ethns)), .before = everything())


CI <- rbind(crude_CI, weighted_CI)

# plot CIF curves ---------------------------------------------------------

curve_df <- point_est %>% 
  select(-contains("RD"), -contains("RR")) %>% 
  pivot_longer(
    cols = c(starts_with("KM"), starts_with("AJ")),
    names_to = c("Type", "Stroke", "Time"),
    names_pattern = "(.*)_Stroke_(.*)_time_(.*)",
    values_to = "CIF"
  ) %>%
  mutate(Time = as.numeric(Time))


curve_df <- CI %>% 
  select(-contains("RD"), -contains("RR")) %>% 
  pivot_longer(
    cols = contains("_Stroke_"),
    names_to = c("Type", "Stroke", "Time"), 
    names_pattern = "(.*)_Stroke_(.*)_time_(.*)",
    values_to = "CIF"
  ) %>% 
  pivot_wider(
    names_from = bound,
    values_from = CIF
  ) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  right_join(curve_df)

curve_df <- curve_df %>% 
  mutate(Type = ifelse(Type == "KM", "Kaplan-Meier", "Aalen-Johansen"))

curve_df <- curve_df %>% 
  mutate(across(c(CIF, lower, upper), function(x) x * 100))

# one plot for each ethnicity
for (ethn in ethns) {
  # ethn <- ethns[4]
  curve_df %>%
    filter(
      Ethnicity == ethn, stroke_defn == "stroke_combined", 
      Model %in% c("crude", "IPTW + IPCW")
    ) %>%
    filter(Time <= 10) %>%
    mutate(
      Type = as_factor(Type),
      Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
      Model = factor(Model, levels = c("crude", "IPTW + IPCW"), labels = c("crude", "adjusted"))
    ) %>%
    ggplot(aes(x = Time, group = Stroke)) +
    geom_line(aes(y = CIF, color = Stroke)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
    scale_color_discrete(guide = guide_legend(reverse = TRUE)) + 
    scale_y_continuous(limits = c(0, 80)) + 
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
    facet_wrap(~ Type + Model) +
    theme_bw() +
    labs(
      x = "Follow-up time (years)",
      y = "Cumulative probability (%)",
      color = "Stroke"
    )
  
  ggsave(
    file = here("03_figs", "cif_curves", paste0("cif_curves_combined_stroke_", ethn, ".png")),
    height = 5, width = 6, units = "in"
  )
}

# one plot for each ethnicity
for (ethn in ethns) {
  curve_df %>%
    filter(
      Ethnicity == ethn, stroke_defn == "stroke_isd_acvd", 
      Model %in% c("crude", "IPTW + IPCW")
    ) %>%
    filter(Time <= 10) %>%
    mutate(
      Type = as_factor(Type),
      Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
      Model = factor(Model, levels = c("crude", "IPTW + IPCW"), labels = c("crude", "adjusted"))
    ) %>%
    ggplot(aes(x = Time, group = Stroke)) +
    geom_line(aes(y = CIF, color = Stroke)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
    scale_color_discrete(guide = guide_legend(reverse = TRUE)) + 
    scale_y_continuous(limits = c(0, 80)) + 
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
    facet_wrap(~ Type + Model) +
    theme_bw() +
    labs(
      x = "Follow-up time (years)",
      y = "Cumulative probability (%)",
      color = "Stroke"
    )
  
  ggsave(
    file = here("03_figs", "cif_curves", paste0("cif_curves_isd_", ethn, ".png")),
    height = 5, width = 6, units = "in"
  )
}

# collect RR/RD at yr 10 --------------------------------------------------

RR_RD_results <- CI %>% 
  select(Ethnicity, Model, stroke_defn, contains("RR"), contains("RD"), bound) %>% 
  pivot_wider(
    names_from = bound,
    values_from = c(contains("RR"), contains("RD"))
  ) %>% 
  right_join(
    point_est %>% 
      select(Ethnicity, Model, stroke_defn, contains("RR"), contains("RD"))
  ) %>% 
  filter(Model != "IPTW") %>%
  mutate(Model = ifelse(Model == "crude", "Crude", Model))

# Save raw numbers for figures

saveRDS(RR_RD_results, here::here("02_outputs", "raw_results.rds"))
# saveRDS(RR_RD_results, here::here("02_outputs", "raw_results_additional_yrs.rds"))

# write results table
RR_RD_results %>% 
  mutate(
    across(-c(stroke_defn, Ethnicity, Model), 
           function(x) format(round(x, 1), trim = TRUE, nsmall = 1)),
    
    KM_RR_10_final = paste0(KM_RR_10, " (", KM_RR_10_lower, ",", KM_RR_10_upper, ")"), 
    KM_RD_10_final = paste0(KM_RD_10, " (", KM_RD_10_lower, ",", KM_RD_10_upper, ")"),
    
    AJ_RR_10_final = paste0(AJ_RR_10, " (", AJ_RR_10_lower, ",", AJ_RR_10_upper, ")"),
    AJ_RD_10_final = paste0(AJ_RD_10, " (", AJ_RD_10_lower, ",", AJ_RD_10_upper, ")")
    
  ) %>% 
  select(stroke_defn, Ethnicity, Model, ends_with("final")) %>% 
  arrange(stroke_defn, Ethnicity) %>%
  pivot_wider(
    id_cols = c(stroke_defn, Ethnicity),
    names_from = Model, 
    values_from = c(starts_with("KM"), starts_with("AJ"))
  ) %>% 
  write.xlsx(
    here("02_outputs", "RR_RD_results.xlsx")
  )

# 
# ## FIGURES --------------------------------------------------------------
# 
# # save the theme that will be used across figs
# tree_plot_theme <- theme_bw() + theme(
#       legend.position= c(0.85, 0.8),
#       legend.background = element_blank(),
#       legend.box.background = element_rect(color = "black"),
#       axis.title.y = element_text(vjust = 3),
#       panel.grid.minor.x = element_blank(),
#       axis.text.x = element_text(size = 11),
#       axis.text.y = element_text(size = 11),
#       strip.background = element_blank()
#     ) 
# 
# 
# # 1. Direct effect, RR ----------------------------------------------------
# 
# RR_RD_results %>% 
#   filter(stroke_defn == "stroke_combined") %>% 
#   select(Ethnicity, contains("KM_RR"), Model) %>% 
#   ggplot(
#     aes(
#       x = Ethnicity,
#       y = KM_RR_10,
#       group = Model,
#       ymin = KM_RR_10_lower,
#       ymax = KM_RR_10_upper,
#     )
#   ) +
#   geom_linerange(position = position_dodge(width = 0.4)) +
#   geom_point(aes(color = Model, fill = Model),
#              position = position_dodge(width = 0.4),
#              shape = 15,
#              alpha = 1,
#              show.legend = T) +
#   geom_errorbar(position = position_dodge(width = 0.4),
#                 width = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#0072B2")) +
#   geom_hline(aes(yintercept = 1),
#              linetype = "dashed") +
#   scale_y_log10(breaks = seq(1, 12, by = 2)) +
#   labs(y = "Risk Ratio at 10 years of follow-up",
#        x = NULL) +
#   tree_plot_theme +
#   theme(legend.position= c(0.8, 0.2))
# 
# ggsave(
#   file = here("03_figs", "RR_10yr_direct_effect.png"),
#   height = 5, width = 6, units = "in"
# )
# 
# # 2. Direct effect, RD ----------------------------------------------------
# 
# RR_RD_results %>%
#   filter(stroke_defn == "stroke_combined") %>% 
#   select(Ethnicity, contains("KM_RD"), Model) %>% 
#   ggplot(
#     aes(
#       x = Ethnicity,
#       y = KM_RD_10,
#       group = Model,
#       ymin = KM_RD_10_lower,
#       ymax = KM_RD_10_upper,
#     )
#   ) +
#   geom_linerange(position = position_dodge(width = 0.4)) +
#   geom_point(aes(color = Model, fill = Model),
#              position = position_dodge(width = 0.4),
#              shape = 15,
#              alpha = 1,
#              show.legend = T) +
#   geom_errorbar(position = position_dodge(width = 0.4),
#                 width = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#0072B2")) +
#   geom_hline(aes(yintercept = 0),
#              linetype = "dashed") +
#   labs(y = "Risk Difference at 10 years of follow-up (%)",
#        x = NULL) +
#   tree_plot_theme
#  
# ggsave(
#   file = here("03_figs", "RD_10yr_direct_effect.png"),
#   height = 5, width = 6, units = "in"
# )
# 
# # 3. Total effect, RR -----------------------------------------------------
# 
# RR_RD_results %>%
#   filter(stroke_defn == "stroke_combined") %>% 
#   select(Ethnicity, contains("AJ_RR"), Model) %>%
#   ggplot(
#     aes(
#       x = Ethnicity,
#       y = AJ_RR_10,
#       group = Model,
#       ymin = AJ_RR_10_lower,
#       ymax = AJ_RR_10_upper,
#     )
#   ) +
#   geom_linerange(position = position_dodge(width = 0.4)) +
#   geom_point(aes(color = Model, fill = Model),
#              position = position_dodge(width = 0.4),
#              shape = 15,
#              alpha = 1,
#              show.legend = T) +
#   geom_errorbar(position = position_dodge(width = 0.4),
#                 width = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#0072B2")) +
#   geom_hline(aes(yintercept = 1), linetype = "dashed") +
#   scale_y_log10(breaks = seq(0, 3, by = 0.2)) +
#   labs(y = "Risk Ratio at 10 years of follow-up",
#        x = NULL) +
#   tree_plot_theme +
#   theme(legend.position= c(0.8, 0.2))
# 
# ggsave(
#   file = here("03_figs", "RR_10yr_total_effect.png"),
#   height = 5, width = 6, units = "in"
# )
# 
# 
# # 4. Total Effect, RD --------------------------------------------------------
# 
# 
# RR_RD_results %>%
#   filter(stroke_defn == "stroke_combined") %>% 
#   select(Ethnicity, contains("AJ_RD"), Model) %>%
#   ggplot(
#     aes(
#       x = Ethnicity,
#       y = AJ_RD_10,
#       group = Model,
#       ymin = AJ_RD_10_lower,
#       ymax = AJ_RD_10_upper,
#     )
#   ) +
#   geom_linerange(position = position_dodge(width = 0.4)) +
#   geom_point(aes(color = Model, fill = Model),
#              position = position_dodge(width = 0.4),
#              shape = 15,
#              alpha = 1,
#              show.legend = T) +
#   geom_errorbar(position = position_dodge(width = 0.4),
#                 width = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#0072B2")) +
#   geom_hline(aes(yintercept = 0), linetype = "dashed") +
#   labs(y = "Risk Difference at 10 years of follow-up (%)",
#        x = NULL) +
#   tree_plot_theme
# 
# 
# ggsave(
#   file = here("03_figs", "RD_10yr_total_effect.png"),
#   height = 5, width = 6, units = "in"
# )

