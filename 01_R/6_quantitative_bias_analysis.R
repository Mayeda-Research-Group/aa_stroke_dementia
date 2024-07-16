if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load(
  "tidyverse", "here", "episensr", "survival" 
)

source(here("01_R", "path.R"))
options(scipen = 999)

# Load data -------------------------------------------------------------

data_folder <- paste0(path_to_box, "Asian_Americans_dementia_data/")

# load one imputation 
long_tte_data <- readRDS(
  paste0(data_folder, "aa_stroke_dementia/final_datasets/",
         "combined_imp_1.RDS"))

long_tte_data <- long_tte_data %>% arrange(subjid, fu_yr)

# want to create a wide dataset that has the indicator and yr of 
# stroke and dementia dx for each subject
wide_tte_data <- long_tte_data %>% distinct(subjid, ethnicity_rev, end_type)

subset <- long_tte_data %>% 
  select(subjid, ethnicity_rev,
         fu_yr, stroke_combined, event_dem, event_end_mem, event_death) 

wide_tte_data <- subset %>% 
  filter(stroke_combined == 1) %>% 
  group_by(subjid) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  rename(stroke_yr = fu_yr) %>% 
  select(subjid, stroke_yr) %>% 
  right_join(wide_tte_data, by = "subjid")

wide_tte_data <- subset %>% 
  filter(event_dem == 1) %>% 
  group_by(subjid) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  rename(dem_yr = fu_yr) %>% 
  select(subjid, dem_yr) %>% 
  right_join(wide_tte_data, by = "subjid")

wide_tte_data <- wide_tte_data %>% 
  mutate(stroke = ifelse(is.na(stroke_yr), 0, 1), 
         dem = ifelse(is.na(dem_yr), 0, 1))


# set up analysis ---------------------------------------------------------

# set up an example 2x2 table
wide_tte_data %>% 
  # subset to those who either have not had stroke at all over followup 
  # or those who developed stroke at the specified year of followup (exp_yr)
  filter(stroke_yr == 3 | is.na(stroke_yr)) %>%
  # filter(!(stroke_yr == 1 | is.na(stroke_yr))) %>%
  # count cases and exposures
  with(table(dem == 0, stroke == 0, useNA = "ifany"))

# in this setup, controls are those who never had stroke throughout followup.
# case definition is dementia or no dementia (including all types of censoring)

dem_mis <- function(data = wide_tte_data, exp_yr, ethn) {
  # data <- wide_tte_data
  # exp_yr <- 1
  # ethn <- "South Asian"
  
  if (ethn != "overall") { data <- data %>% filter(ethnicity_rev == ethn) }
  
  crosstab <- data %>% 
    # subset to those who either have not had stroke at all over followup 
    # or those who developed stroke at the specified year of followup (exp_yr)
    filter(stroke_yr == exp_yr | is.na(stroke_yr)) %>% 
    # count cases and exposures
    with(table(dem == 0, stroke == 0, useNA = "ifany"))
  
  colnames(crosstab) <- c("stroke", "no stroke")
  rownames(crosstab) <- c("dementia", "no dementia")
  
  # https://doi.org/10.1186/s12883-023-03361-w
  # Specificity of an EHR diagnosis of dementia was 99.0%, and sensitivity was 56.1%.
  
  # Specificity: P(Test = no disease | Truth = no disease)
  # Sensitivity: P(Test = disease | Truth = disease)
  
  # In dementia dx, we believe that specificity should be high for 
  # both the stroke arm and non-stroke arm. We set it at 0.99. 
  # We also believe that dementia dx is highly sensitive in the stroke arm 
  # because of post-stroke care, while in the non-stroke arm, sensitivity 
  # could be lower. Worst-case scenario is 0.5, i.e. 50% probability of 
  # getting a dementia diagnosis when the patient actually has dementia. 
  # So here we specify a range of sensitivity values for the unexposed 
  # (no-stroke) arm in our observed data. 
  
  sens_unexposed <- seq(0.50, 0.90, by = 0.05)
  
  for (sens_unexp in sens_unexposed) {
    
    res <- misclassification(
      crosstab,
      type = "outcome", # dementia
      # sens among those with exposure, sens among those without exposure
      # spec among those with exposure, spec among those without exposure
      bias_parms = c(0.99, sens_unexp, 0.99, 0.99),
      alpha = 0.05
    )
    
    if (sens_unexp == sens_unexposed[1]) {
      out <- rbind(res$obs.measures[, 1], t(res$adj.measures)) %>% 
        as_tibble() %>% 
        mutate(type = c("obs", "adj"), 
               sens_unexp = sens_unexp,
               stroke_yr = exp_yr) %>% 
        setNames(c("RR", "OR", "Type", "Sens_Unexposed", "Stroke_Yr"))
    } else {
      out <- t(res$adj.measures) %>% as_tibble() %>% 
        mutate(type = c("adj"), sens_unexp = sens_unexp, stroke_yr = exp_yr) %>% 
        setNames(c("RR", "OR", "Type", "Sens_Unexposed", "Stroke_Yr")) %>% 
        add_row(out, .)
    }
  }
  
  return(out)
  
}

# among the white group, compare subjects who developed stroke at yr 1 
# against those who never developed stroke throughout the entire followup:
dem_mis(exp_yr = 1, ethn = "White")

# When the sensitivity of dementia dx among the unexposed (no-stroke) is low
# in our data, it means more dementia cases are undiagnosed among the unexposed.
# If we are able to correct for that, the adjusted RR/OR should have a smaller 
# effect size than the original RR/OR, but the direction of the effect does not 
# change. 

# When the sensitivity of dementia dx among the unexposed (stroke) is high
# in our data, then it means we are able to observe almost all dementia cases, 
# and thus the RR/OR does not need significant adjustments. 

# We can look at other timepoints for stroke dx and among other ethnicities: 
dem_mis(exp_yr = 1, ethn = "Chinese")
dem_mis(exp_yr = 2, ethn = "Japanese")
dem_mis(exp_yr = 1, ethn = "South Asian")
# it gives an error for the South Asian group: 
# Parameters chosen lead to negative cell(s) in adjusted 2x2 table.
# I am guessing this is due to the small sample size
wide_tte_data %>% 
  filter(ethnicity_rev == "South Asian") %>% 
  # subset to those who either have not had stroke at all over followup 
  # or those who developed stroke at the specified year of followup (exp_yr)
  filter(stroke_yr == 5 | is.na(stroke_yr)) %>%
  # count cases and exposures
  with(table(dem == 0, stroke == 0, useNA = "ifany"))

# We can also plot the results by stroke yr: 
qba_white <- lapply(1:10, function(x) dem_mis(exp_yr = x, ethn = "White")) %>% 
  bind_rows() 

qba_white %>% 
  ggplot(
    aes(x = Stroke_Yr, y = RR, group = interaction(Type, Sens_Unexposed), 
        color = Sens_Unexposed, linetype = Type)) + 
  geom_line() + 
  theme_bw()

# do all the ethnicities, except South Asian
for (ethn in c("Chinese", "Filipino", "Japanese", "White")) {
  temp <- lapply(1:10, function(x) dem_mis(exp_yr = x, ethn = ethn)) %>% 
    bind_rows() %>% 
    mutate(ethn = ethn)
  
  if (ethn == "Chinese") {qba_all <- temp}
  else {qba_all <- bind_rows(qba_all, temp)}
}

# although the trend over yrs is bumpy, it still remains that
# as sens of dementia among no-stroke group decreases, the effect sizes get smaller
# but hardly cros to the other direction
qba_all %>% 
  ggplot(
    aes(x = Stroke_Yr, y = RR, group = interaction(Type, Sens_Unexposed), 
        color = Sens_Unexposed, linetype = Type)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~ ethn)

# prepare the table in supplement, choosing those who developed stroke at yr 5 
# as cases
plot_df <- qba_all %>% 
  filter(Stroke_Yr == 5) %>% 
  mutate(
    Type = ifelse(Type == "obs", "Observed", "Adjusted"),
    # # the observed effect size would be the same as the adjusted one if 
    # # sensitivity is 1
    # Sens_Unexposed = ifelse(Type == "Observed", 1, Sens_Unexposed)
    ethn = ifelse(ethn == "White", "Non-Latino White", ethn)
  ) 

ggplot() + 
  geom_point(
    data = plot_df %>% filter(Type == "Observed"), 
    aes(x = ethn, y = RR, shape = Type)
  ) + 
  geom_point(
    data = plot_df %>% filter(Type == "Adjusted"), 
    aes(x = ethn, y = RR, shape = Type, color = Sens_Unexposed)
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  scale_x_discrete(limits = rev(sort(unique(plot_df$ethn)))) +
  scale_y_continuous(breaks = seq(1, 3, 0.2), limits = c(0.9, 3)) + 
  scale_shape_manual(
    values = c("Observed" = 1, "Adjusted" = 19), 
    guide = guide_legend(reverse = TRUE, )
  ) + 
  scale_color_continuous(
    type = "viridis", trans = "reverse", 
    guide = guide_colorbar(reverse = TRUE, direction = "horizontal",
                           title.position = "top", order = 1)
  ) + 
  # lims(y = c(0.9, 3)) + 
  labs(x = "Ethnicity", y = "Risk ratio", 
       color = "Sensitivity of dementia\ndiagnosis among controls",
       type = "Type") + 
  guides(shape = guide_legend(order = 2)) +
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank()) + 
  coord_flip()


ggsave(
  filename = here("03_figs", "qba_RR_5yrcases.png"),
  height = 4, width = 8, unit = "in"
)

 
  

