library(tidyverse)
library(here)
source(here("01_R", "path.R"))

data <- readRDS(here("02_outputs", "raw_results.RDS"))

## FIGURES --------------------------------------------------------------
# 
# # save the theme that will be used across figs
# tree_plot_theme <- theme_bw() + theme(
#   legend.position= c(0.85, 0.8),
#   legend.background = element_blank(),
#   legend.box.background = element_rect(color = "black"),
#   axis.title.y = element_text(vjust = 3),
#   panel.grid.minor.x = element_blank(),
#   axis.text.x = element_text(size = 11),
#   axis.text.y = element_text(size = 11),
#   strip.background = element_blank()
# ) 
# 
# 
# # 1. Direct effect, RR ----------------------------------------------------
# 
# data %>% 
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


# MANUSCRIPT FIG with Facet--------------------------------------------------------------

## Risk ratio 

data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("KM_RR"), contains("AJ_RR")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted")) %>% 
  mutate(Model = as_factor(Model)) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") %>% 
  mutate(estimand = ifelse(estimand == "AJ", "Total effect", "Direct Effect")) %>% 
  ggplot(
    aes(
      x = Ethnicity,
      y = RR_10,
      group = Model,
      ymin = RR_10_lower,
      ymax = RR_10_upper,
    )
  ) +
  # geom_linerange(position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Model, fill = Model),
             position = position_dodge(width = 0.4),
             shape = 15,
             alpha = 1,
             show.legend = T) +
  geom_errorbar(
    aes(color = Model),
    position = position_dodge(width = 0.4),
                width = 0.1) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 1),
             linetype = "dashed") +
  scale_y_log10(breaks = seq(0.5, 12, by = 1.5)) +
  facet_wrap(.~estimand) +
  labs(y = "Risk Ratio at 10 years of follow-up",
       x = NULL) +
  theme_bw() + 
  theme(
    # legend.position= c(0.85, 0.8),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position= c(0.1, 0.15),
    strip.text = element_text(size = 11)
    # strip.background = element_blank()
  ) 

ggsave(
  file = here("03_figs", "RR_SCALE.png"),
  height = 5, width = 8 , units = "in"
)

## Risk difference

data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("KM_RD"), contains("AJ_RD")) %>%
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") %>% 
  mutate(estimand = ifelse(estimand == "AJ", "Total effect", "Direct Effect")) %>% 
  ggplot(
    aes(
      x = Ethnicity,
      y = RD_10,
      group = Model,
      ymin = RD_10_lower,
      ymax = RD_10_upper,
    )
  ) +
  # geom_linerange(position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Model, fill = Model),
             position = position_dodge(width = 0.4),
             shape = 15,
             alpha = 1,
             show.legend = T) +
  geom_errorbar(
    aes(color = Model),
    position = position_dodge(width = 0.4),
    width = 0.1) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 0),
             linetype = "dashed") +
  # scale_y_log10(breaks = seq(0.5, 12, by = 1.5)) +
  facet_wrap(.~estimand) +
  labs(y = "Risk Difference (%) at 10 years of follow-up",
       x = NULL) +
  theme_bw() + 
  theme(
    # legend.position= c(0.85, 0.8),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position= c(0.9, 0.85),
    strip.text = element_text(size = 11)
    # strip.background = element_blank()
  ) 

ggsave(
  file = here("03_figs", "RD_SCALE.png"),
  height = 5, width = 8 , units = "in"
)


# AAIC FIGURE -------------------------------------------------------------



data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("KM")) %>%
  filter(Model == "Crude") %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "KM_(RR|RD)_(.*)") %>% 
  mutate(null_value = ifelse(estimand == "RR", 1, 0)) %>% 
  mutate(estimand = ifelse(estimand == "RR", "Risk Ratio", "Risk Difference (%)")) %>% 
  ggplot(
    aes(
      x = Ethnicity,
      y = `10`,
      ymin = `10_lower`,
      ymax = `10_upper`,
    )
  ) +
  # geom_linerange(position = position_dodge(width = 0.4)) +
  geom_point(position = position_dodge(width = 0.4),
             shape = 15,
             alpha = 1,
             show.legend = T, color = "#0072B2") +
  geom_errorbar(
    position = position_dodge(width = 0.4),
    width = 0.1,
    color = "#0072B2") +
  # scale_color_manual(values = c("#E69F00", "#0072B2")) +
  # geom_hline(aes(yintercept = null_value),
  #            linetype = "dashed") +
  # scale_y_log10(breaks = seq(0.5, 12, by = 1.5)) +
  facet_wrap(.~ fct_rev(estimand), scales = "free") +
  labs(y = "Estimates at 10 years of follow-up",
       x = NULL) +
  theme_bw() + 
  theme(
    legend.position= "bottom",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    # legend.position= c(0.9, 0.85),
    strip.text = element_text(size = 11)
    # strip.background = element_blank()
  ) 

ggsave(
  file = here("03_figs", "plot_aaic.png"),
  height = 3.7, width = 8 , units = "in"
)
