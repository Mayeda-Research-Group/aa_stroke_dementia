library(tidyverse)
library(here)
library(openxlsx)
source(here("01_R", "path.R"))

ethn_order <- c(
  "Chinese", "Filipino", "Japanese", "South Asian", # ethnicities with further results
  # "Korean", "Vietnamese", "Pacific Islander", "Other SE Asian", "Multiple", # other ethnicities
  "Non-Latino White"
)

# Figure 1: cause-spec cumulative inc of stroke  ----
# by stroke type

stroke_inc <- read.xlsx(here("02_outputs", "table_e5_stroke_summary_shorter.xlsx"))

stroke_inc <- stroke_inc %>% 
  select(ethnicity_rev, starts_with("cs")) %>% 
  pivot_longer(
    cols = -1, 
    names_to = "stroke_type",
    names_prefix = "cs_risk_",
    values_to = "cuminc"
  ) %>% 
  separate_wider_delim(cuminc, delim = " ", names = c("pe", "CI")) %>% 
  separate_wider_delim(CI, delim = ",", names = c("lower", "upper")) %>% 
  mutate(
    ethnicity_rev = str_remove_all(ethnicity_rev, " \\(\\d*\\)"), 
    ethnicity_rev = ifelse(ethnicity_rev == "White", "Non-Latino White", ethnicity_rev), 
    stroke_type = factor(
      stroke_type,
      levels = c("hstroke", "isd_acvd", "combined_stroke"),
      labels = c(
        "First hemorrhagic stroke",
        "First ischemic stroke",
        "First stroke (ischemic or hemorrhagic)"
      )
    ),
    across(c(pe, lower, upper), parse_number)
  )


stroke_inc %>%
  filter(ethnicity_rev %in% ethn_order) %>% 
  ggplot(aes(x = ethnicity_rev, group = stroke_type, color = stroke_type)) +
  geom_bar(
    aes(y = pe, fill = stroke_type), alpha = 0.5, width = 0.5,
    stat = "identity", position = position_dodge()
  ) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), width = 0.4,
    position = position_dodge(0.5)
  ) + 
  scale_color_manual(values = c("#FFB000", "#648FFF", "#009E73"),
                     breaks = c('First stroke (ischemic or hemorrhagic)',
                                "First ischemic stroke",
                                "First hemorrhagic stroke")) + 
  scale_fill_manual(values = c("#FFB000", "#648FFF", "#009E73"),
                    breaks = c('First stroke (ischemic or hemorrhagic)',
                               "First ischemic stroke",
                               "First hemorrhagic stroke")) + 
  scale_x_discrete(limits = rev(ethn_order)) + 
  scale_y_continuous(breaks = seq(0, 20, by = 2)) + 
  coord_flip() +
  theme_light() + 
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    # panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  ) +
  guides(color = "none") + 
  labs(
    x = NULL,
    y = "Cumulative incidence of stroke (%)",
    fill = NULL
  )

ggsave(
  file = here::here("03_figs", "stroke_cs_cumulative_inc_barplot.png"),
  height = 5, width = 7, units = "in"
)

  
# Figure 2: RR direct effect ----

data <- readRDS(here("02_outputs", "raw_results_additional_yrs.RDS")) %>% 
  mutate(
    Ethnicity = ifelse(Ethnicity == "White", "Non-Latino White", Ethnicity) %>% 
      factor(levels = ethn_order)
  )

# RR (crude and adjusted) for direct effect (main stroke defn)

RR_plotdf <- data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("KM_RR")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted") %>% as_factor()) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)")

# adjust CI limits when going below a certain limit
RR_limit <- 10
RR_plotdf <- RR_plotdf %>% 
  mutate(
    arrow_pos = ifelse(RR_10_upper > RR_limit, RR_limit, NA), 
    RR_10_upper_l = ifelse(RR_10_upper <= RR_limit, RR_10_upper, NA), 
    line_nudge = case_when(
      Model == "Crude" ~ 0.1, 
      Model == "Adjusted" ~ -0.1
    ) 
  )

RR_plotdf %>% 
  ggplot(aes(x = Ethnicity, group = Model, color = Model)) +
  geom_point(
    aes(y = RR_10, color = Model), 
    position = position_nudge(x = RR_plotdf$line_nudge)
  ) + 
  geom_errorbar(
    aes(ymin = RR_10_lower, ymax = RR_10_upper_l), 
    position = position_nudge(x = RR_plotdf$line_nudge),
    width = 0.1
  ) + 
  geom_segment(
    aes(x = Ethnicity, xend = Ethnicity, y = RR_10_lower, yend = arrow_pos),
    position = position_nudge(x = RR_plotdf$line_nudge),
    arrow = arrow(length = unit(0.2, "cm")),
    show.legend = F
  ) + 
  geom_text(
    aes(x = Ethnicity, y = arrow_pos + 2, 
        label = paste0("UL=", round(RR_10_upper, 2))),
    size = 3, 
    position = position_nudge(x = RR_plotdf$line_nudge),
    show.legend = F
  ) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  scale_x_discrete(limits = rev(sort(unique(data$Ethnicity)))) + 
  scale_y_log10(breaks = seq(1, 10, by = 1.5), limits = c(0.9, 13)) + 
  coord_flip() + 
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk ratio at 10 years of follow-up", x = NULL)

ggsave(
  file = here("03_figs", "RR_direct_effect.png"),
  height = 4, width = 8.5 , units = "in"
)

# RR_plotdf %>% 
#   ggplot(aes(x = Ethnicity, y = RR_10, group = Model, color = Model, 
#              ymin = RR_10_lower, ymax = RR_10_upper)) +
#   # geom_linerange(position = position_dodge(width = 0.4)) +
#   geom_point(aes(fill = Model),
#              position = position_dodge(width = -0.4),
#              shape = 15, alpha = 1) +
#   geom_errorbar(position = position_dodge(width = -0.4), width = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#0072B2")) +
#   geom_hline(aes(yintercept = 1), linetype = "dashed") +
#   scale_x_discrete(limits = rev(sort(unique(data$Ethnicity)))) + 
#   scale_y_log10(breaks = seq(1, 12, by = 1.5), limits = c(0.9, 13)) + 
#   coord_flip() + 
#   labs(y = "Risk ratio at 10 years of follow-up", x = NULL) +
#   theme_bw() + 
#   theme(
#     legend.background = element_blank(),
#     legend.box.background = element_blank(),
#     axis.title.y = element_text(vjust = 3),
#     panel.grid.minor.x = element_blank(),
#     axis.text.x = element_text(size = 11),
#     axis.text.y = element_text(size = 11),
#     strip.text = element_text(size = 11)
#   ) 

## supplemental plot for a range of years of follow-up ----
RR_yr_plotdf <- RR_plotdf %>% 
  select(-arrow_pos, -RR_10_upper_l, -line_nudge) %>% 
  # append _pe to point estimate columns
  rename_with(.fn = function(x) ifelse(str_ends(x, "\\d"), paste0(x, "_pe"), x)) %>% 
  pivot_longer(
    cols = starts_with("RR"), 
    names_to = c("year", "type"), 
    names_pattern = "RR_(.*)_(.*)",
    values_to = "RR"
  ) %>% 
  pivot_wider(
    names_from = "type",
    values_from = "RR"
  ) %>% 
  mutate(year = as.numeric(year))

RR_yr_plotdf %>% 
  ggplot(aes(x = year, y = pe, color = Model, group = Model)) + 
  geom_point(position = position_dodge(width = 0.8)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    position = position_dodge(width = 0.8), 
    width = 0.4
  ) + 
  scale_x_continuous(breaks = seq(2,10,2)) + 
  facet_wrap(~ Ethnicity) + 
  coord_flip() + 
  theme_bw()

# the scales are bad, limit upper CI

RR_limit <- 40
RR_yr_plotdf <- RR_yr_plotdf %>% 
  mutate(
    arrow_pos = ifelse(upper > RR_limit, RR_limit, NA), 
    upper_l = ifelse(upper <= RR_limit, upper, NA), 
    line_nudge = case_when(
      Model == "Crude" ~ 0.3, 
      Model == "Adjusted" ~ -0.3
    )
  )

RR_yr_plotdf %>% 
  ggplot(aes(x = year, y = pe, group = Model, color = Model)) +
  facet_wrap(~ Ethnicity, ncol = 1) + 
  geom_point(
    position = position_nudge(x = RR_yr_plotdf$line_nudge)
  ) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper_l), 
    position = position_nudge(x = RR_yr_plotdf$line_nudge),
    width = 0.3
  ) + 
  geom_segment(
    aes(x = year, xend = year, y = lower, yend = arrow_pos),
    position = position_nudge(x = RR_yr_plotdf$line_nudge),
    arrow = arrow(length = unit(0.2, "cm")),
    show.legend = F
  ) + 
  geom_text(
    aes(x = year, y = arrow_pos + 3, 
        label = paste0("UL=", round(upper, 1))),
    size = 3, 
    position = position_nudge(x = RR_yr_plotdf$line_nudge),
    show.legend = F
  ) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  # scale_x_reverse() +
  scale_x_continuous(
    transform = "reverse",
    breaks = seq(10, 2, -2), limits = c(11, 1)
  ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(-2, 45)) + 
  # scale_y_log10(breaks = seq(1, 40, by = 5), limits = c(0.9, 45)) + 
  coord_flip() + 
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk ratio", x = "Year") 

ggsave(
  file = here("03_figs", "RR_direct_effect_by_yr.png"),
  height = 8, width = 6 , units = "in"
)


# Figure 3: RD direct effect ----

# RD (crude and adjusted) for direct effect (main stroke defn)

RD_plotdf <- data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("KM_RD")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted") %>% as_factor()) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") 

# adjust CI limits when going below a certain limit
RD_limit <- 60
RD_plotdf <- RD_plotdf %>% 
  mutate(
    arrow_pos = ifelse(RD_10_upper > RD_limit, RD_limit, NA), 
    RD_10_upper_l = ifelse(RD_10_upper <= RD_limit, RD_10_upper, NA), 
    line_nudge = case_when(
      Model == "Crude" ~ 0.1, 
      Model == "Adjusted" ~ -0.1
    ) 
  )

RD_plotdf %>% 
  ggplot(aes(x = Ethnicity, group = Model, color = Model)) +
  geom_point(
    aes(y = RD_10, color = Model), 
    position = position_nudge(x = RD_plotdf$line_nudge)
  ) + 
  geom_errorbar(
    aes(ymin = RD_10_lower, ymax = RD_10_upper_l), 
    position = position_nudge(x = RD_plotdf$line_nudge),
    width = 0.1
  ) + 
  geom_segment(
    aes(x = Ethnicity, xend = Ethnicity, y = RD_10_lower, yend = arrow_pos),
    position = position_nudge(x = RD_plotdf$line_nudge),
    arrow = arrow(length = unit(0.2, "cm")),
    show.legend = F
  ) + 
  geom_text(
    aes(x = Ethnicity, y = arrow_pos + 4, 
        label = paste0("UL=", round(RD_10_upper, 1))),
    size = 3, 
    position = position_nudge(x = RD_plotdf$line_nudge),
    show.legend = F
  ) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_discrete(limits = rev(sort(unique(data$Ethnicity)))) + 
  scale_y_continuous(limits = c(-3, 65), breaks = seq(0, 60, 10)) + 
  coord_flip() + 
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk difference at 10 years of follow-up", x = NULL)

ggsave(
  file = here("03_figs", "RD_direct_effect.png"),
  height = 4, width = 8.5 , units = "in"
)

# RD_plotdf %>% 
#   ggplot(aes(x = Ethnicity, y = RD_10, group = Model, color = Model, 
#              ymin = RD_10_lower, ymax = RD_10_upper)) +
#   # geom_linerange(position = position_dodge(width = 0.4)) +
#   geom_point(aes(fill = Model),
#              position = position_dodge(width = 0.4),
#              shape = 15, alpha = 1) +
#   geom_errorbar(position = position_dodge(width = 0.4), width = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#0072B2")) +
#   geom_hline(aes(yintercept = 0), linetype = "dashed") +
#   scale_x_discrete(limits = rev(sort(unique(data$Ethnicity)))) + 
#   scale_y_continuous(limits = c(-3, 80)) + 
#   coord_flip() + 
#   labs(y = "Risk Difference (%) at 10 years of follow-up",
#        x = NULL) +
#   theme_bw() + 
#   theme(
#     # legend.position= c(0.85, 0.8),
#     legend.background = element_blank(),
#     legend.box.background = element_blank(),
#     # legend.position = c(0.10, 0.15),
#     axis.title.y = element_text(vjust = 3),
#     panel.grid.minor.x = element_blank(),
#     axis.text.x = element_text(size = 11),
#     axis.text.y = element_text(size = 11),
#     strip.text = element_text(size = 11)
#     # strip.background = element_blank()
#   ) 



## supplemental plot for a range of years of follow-up ----
RD_yr_plotdf <- RD_plotdf %>% 
  select(-arrow_pos, -RD_10_upper_l, -line_nudge) %>% 
  # append _pe to point estimate columns
  rename_with(.fn = function(x) ifelse(str_ends(x, "\\d"), paste0(x, "_pe"), x)) %>% 
  pivot_longer(
    cols = starts_with("RD"), 
    names_to = c("year", "type"), 
    names_pattern = "RD_(.*)_(.*)",
    values_to = "RD"
  ) %>% 
  pivot_wider(
    names_from = "type",
    values_from = "RD"
  ) %>% 
  mutate(year = as.numeric(year))

RD_yr_plotdf %>% 
  mutate(Model = factor(Model, levels = c("Adjusted", "Crude"))) %>% 
  ggplot(aes(x = year, y = pe, color = Model, group = Model)) + 
  geom_point(position = position_dodge(width = 0.8)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    position = position_dodge(width = 0.8), 
    width = 0.4
  ) + 
  scale_color_manual(values = c("Crude" = "#E69F00", "Adjusted" = "#0072B2"),
                     breaks = c("Crude", "Adjusted")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    transform = "reverse",
    breaks = seq(10, 2, -2), limits = c(11, 1)
  ) +
  scale_y_continuous(breaks = seq(0, 80, 15), limits = c(-2, 75)) + 
  facet_wrap(~ Ethnicity, ncol = 1) + 
  coord_flip() + 
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
    labs(y = "Risk difference", x = "Year") 
  
ggsave(
  file = here("03_figs", "RD_direct_effect_by_yr.png"),
  height = 8, width = 6, units = "in"
)

# we can put arrows on CI's, but might not be too necessary

RD_limit <- 70
RD_yr_plotdf <- RD_yr_plotdf %>% 
  mutate(
    arrow_pos = ifelse(upper > RD_limit, RD_limit, NA), 
    upper_l = ifelse(upper <= RD_limit, upper, NA), 
    line_nudge = case_when(
      Model == "Crude" ~ 0.3, 
      Model == "Adjusted" ~ -0.3
    )
  )

RD_yr_plotdf %>% 
  ggplot(aes(x = year, y = pe, group = Model, color = Model)) +
  facet_wrap(~ Ethnicity, ncol = 1) + 
  geom_point(
    position = position_nudge(x = RD_yr_plotdf$line_nudge)
  ) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper_l), 
    position = position_nudge(x = RD_yr_plotdf$line_nudge),
    width = 0.3
  ) + 
  geom_segment(
    aes(x = year, xend = year, y = lower, yend = arrow_pos),
    position = position_nudge(x = RD_yr_plotdf$line_nudge),
    arrow = arrow(length = unit(0.2, "cm")),
    show.legend = F
  ) + 
  geom_text(
    aes(x = year, y = arrow_pos + 4, 
        label = paste0("UL=", round(upper, 1))),
    size = 3, 
    position = position_nudge(x = RR_yr_plotdf$line_nudge),
    show.legend = F
  ) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(breaks = seq(2, 10, 2), limits = c(1, 11)) + 
  # scale_y_continuous(breaks = seq(0, 80, 10), limits = c(-2, 75)) +
  # scale_y_log10(breaks = seq(1, 40, by = 5), limits = c(0.9, 45)) + 
  coord_flip() + 
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk difference (%)", x = "Year") 

# Supp Figure xx: RR total effect ----

# RR (crude and adjusted) for total effect (main stroke defn)

data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("AJ_RR")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted") %>% as_factor()) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") %>% 
  ggplot(aes(x = Ethnicity, y = RR_10, group = Model, color = Model, 
             ymin = RR_10_lower, ymax = RR_10_upper)) +
  # geom_linerange(position = position_dodge(width = 0.4)) +
  geom_point(aes(fill = Model),
             position = position_dodge(width = -0.4),
             shape = 15, alpha = 1) +
  geom_errorbar(position = position_dodge(width = -0.4), width = 0.1) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  scale_x_discrete(limits = rev(sort(unique(data$Ethnicity)))) + 
  # scale_y_log10(breaks = seq(1, 12, by = 1.5), limits = c(0.9, 13)) + 
  scale_y_log10(breaks = seq(0.2, 3, by = 0.4)) +
  coord_flip() + 
  labs(y = "Risk ratio at 10 years of follow-up", x = NULL) +
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    # legend.position = c(0.10, 0.15),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
    # strip.background = element_blank()
  ) 

ggsave(
  file = here("03_figs", "RR_total_effect.png"),
  height = 4, width = 8.5 , units = "in"
)

## over a range of years of followup ---- 

AJ_RR_yr_plotdf <- data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("AJ_RR")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted") %>% as_factor()) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") %>% 
  # append _pe to point estimate columns
  rename_with(.fn = function(x) ifelse(str_ends(x, "\\d"), paste0(x, "_pe"), x)) %>% 
  pivot_longer(
    cols = starts_with("RR"), 
    names_to = c("year", "type"), 
    names_pattern = "RR_(.*)_(.*)",
    values_to = "RR"
  ) %>% 
  pivot_wider(
    names_from = "type",
    values_from = "RR"
  ) %>% 
  mutate(year = as.numeric(year)) 

AJ_RR_yr_plotdf %>% 
  mutate(Model = factor(Model, levels = c("Adjusted", "Crude"))) %>% 
  ggplot(aes(x = year, y = pe, color = Model, group = Model)) + 
  geom_point(position = position_dodge(width = 0.8)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    position = position_dodge(width = 0.8), 
    width = 0.4
  ) + 
  facet_wrap(~ Ethnicity, ncol = 1) + 
  scale_color_manual(values = c("Crude" = "#E69F00", "Adjusted" = "#0072B2"),
                     breaks = c("Crude", "Adjusted")) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  scale_x_continuous(
    transform = "reverse",
    breaks = seq(10, 2, -2), limits = c(11, 1)
  ) +
  # scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(-2, 45)) + 
  coord_flip() + 
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk ratio", x = "Year") 

# the scales are bad, limit upper CI

RR_limit <- 3
AJ_RR_yr_plotdf <- AJ_RR_yr_plotdf %>% 
  mutate(
    arrow_pos = ifelse(upper > RR_limit, RR_limit, NA), 
    upper_l = ifelse(upper <= RR_limit, upper, NA), 
    line_nudge = case_when(
      Model == "Crude" ~ 0.3, 
      Model == "Adjusted" ~ -0.3
    )
  )

AJ_RR_yr_plotdf %>% 
  ggplot(aes(x = year, y = pe, group = Model, color = Model)) +
  facet_wrap(~ Ethnicity, ncol = 1) + 
  geom_point(
    position = position_nudge(x = AJ_RR_yr_plotdf$line_nudge)
  ) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper_l), 
    position = position_nudge(x = AJ_RR_yr_plotdf$line_nudge),
    width = 0.3
  ) + 
  geom_segment(
    aes(x = year, xend = year, y = lower, yend = arrow_pos),
    position = position_nudge(x = AJ_RR_yr_plotdf$line_nudge),
    arrow = arrow(length = unit(0.2, "cm")),
    show.legend = F
  ) + 
  geom_text(
    aes(x = year, y = arrow_pos + 0.2, 
        label = paste0("UL=", round(upper, 1))),
    size = 3, 
    position = position_nudge(x = AJ_RR_yr_plotdf$line_nudge),
    show.legend = F
  ) +
  facet_wrap(~ Ethnicity, ncol = 1) + 
  scale_color_manual(values = c("Crude" = "#E69F00", "Adjusted" = "#0072B2"),
                     breaks = c("Crude", "Adjusted")) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  scale_x_continuous(
    transform = "reverse",
    breaks = seq(10, 2, -2), limits = c(11, 1)
  ) +
  scale_y_continuous(limits = c(0, 3.24)) + 
  # scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(-2, 45)) + 
  coord_flip() + 
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk ratio", x = "Year") 

ggsave(
  file = here("03_figs", "RR_total_effect_by_yr.png"),
  height = 8, width = 6 , units = "in"
)

# Supp Figure xx: RD total effect ----

# RD (crude and adjusted) for total effect (main stroke defn)

data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("AJ_RD")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted") %>% as_factor()) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") %>% 
  ggplot(aes(x = Ethnicity, y = RD_10, group = Model, color = Model, 
             ymin = RD_10_lower, ymax = RD_10_upper)) +
  # geom_linerange(position = position_dodge(width = 0.4)) +
  geom_point(aes(fill = Model),
             position = position_dodge(width = -0.4),
             shape = 15, alpha = 1) +
  geom_errorbar(position = position_dodge(width = -0.4), width = 0.1) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_discrete(limits = rev(sort(unique(data$Ethnicity)))) + 
  scale_y_continuous(limits = c(-10, 15)) + 
  coord_flip() + 
  labs(y = "Risk difference (%) at 10 years of follow-up",
       x = NULL) +
  theme_bw() + 
  theme(
    # legend.position= c(0.85, 0.8),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    # legend.position = c(0.10, 0.15),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
    # strip.background = element_blank()
  ) 

ggsave(
  file = here("03_figs", "RD_total_effect.png"),
  height = 4, width = 8.5 , units = "in"
)

## over a range of years of followup ---- 

AJ_RD_yr_plotdf <- data %>% 
  filter(stroke_defn == "stroke_combined") %>% 
  select(Ethnicity, Model, contains("AJ_RD")) %>%
  mutate(Model = ifelse(Model == "Crude", "Crude", "Adjusted") %>% as_factor()) %>% 
  pivot_longer(-c(1:2),
               names_to =  c('estimand', '.value'),
               names_pattern = "^(KM|AJ)_(.*)") %>% 
  # append _pe to point estimate columns
  rename_with(.fn = function(x) ifelse(str_ends(x, "\\d"), paste0(x, "_pe"), x)) %>% 
  pivot_longer(
    cols = starts_with("RD"), 
    names_to = c("year", "type"), 
    names_pattern = "RD_(.*)_(.*)",
    values_to = "RD"
  ) %>%
  pivot_wider(
    names_from = "type",
    values_from = "RD"
  ) %>% 
  mutate(year = as.numeric(year)) 

AJ_RD_yr_plotdf %>% 
  mutate(Model = factor(Model, levels = c("Adjusted", "Crude"))) %>% 
  ggplot(aes(x = year, y = pe, color = Model, group = Model)) + 
  geom_point(position = position_dodge(width = 0.8)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    position = position_dodge(width = 0.8), 
    width = 0.4
  ) + 
  facet_wrap(~ Ethnicity, ncol = 1) + 
  scale_color_manual(values = c("Crude" = "#E69F00", "Adjusted" = "#0072B2"),
                     breaks = c("Crude", "Adjusted")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    transform = "reverse",
    breaks = seq(10, 2, -2), limits = c(11, 1)
  ) +
  # scale_y_continuous(breaks = seq(0, 80, 15), limits = c(-2, 75)) + 
  facet_wrap(~ Ethnicity, ncol = 1) + 
  coord_flip() + 
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  labs(y = "Risk difference", x = "Year")

ggsave(
  file = here("03_figs", "RD_total_effect_by_yr.png"),
  height = 8, width = 6 , units = "in"
)

