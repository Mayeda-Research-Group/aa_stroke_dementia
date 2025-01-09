# functions defined in this script are all somewhat related to the confoundr pkg

# fixing a function in confoundr
# makehistory.one <- function(input, id, times, group = NULL, exposure, name.history = "h") 
# {
#   input <- ungroup(input)
#   list.exposure <- paste(exposure, times, sep = "_")
#   if (is.null(id)) {
#     stop("ERROR: 'id' is missing. Please specify a unique identifier")
#   }
#   if (is.null(input)) {
#     stop("ERROR: 'input' dataframe is missing")
#   }
#   if (is.null(exposure)) {
#     stop("ERROR: root name for exposure is missing")
#   }
#   if (is.null(times)) {
#     stop("ERROR: indices for exposure measurement times is missing. Please specify a numeric vector of times")
#   }
#   list.exposure <- paste(exposure, times, sep = "_")
#   if (any(!list.exposure %in% names(input))) {
#     stop("ERROR: The exposure root name is misspelled, or some exposure measurements are missing from the input dataframe, or incorrect measurement times have been specified")
#   }
#   if (!is.character(exposure)) {
#     stop("ERROR: exposure must be specified as a string")
#   }
#   if (!is.null(group) && !is.character(group)) {
#     stop("ERROR: group must be specified as a string")
#   }
#   if (!is.null(name.history) && !is.character(name.history)) {
#     stop("ERROR: name.history must be specified as a string")
#   }
#   s_id <- sym(id)
#   input <- input %>% rename(ID = !!s_id)
#   if (!all(!input$ID %in% input$ID[duplicated(input$ID)])) {
#     stop("ERROR: id does not uniquely identify each observation (i.e. each row). Please specify a unique identifier.")
#   }
#   CumPaste = function(x, .sep = "") {
#     Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, 
#            accumulate = TRUE)
#   }
#   if (is.null(group)) {
#     input.temp <- input %>% ungroup() %>% 
#       select(ID, all_of(list.exposure)) %>% 
#       pivot_longer(all_of(list.exposure), 
#                    names_to = "exp.name.time", values_to = "exp.value") %>% 
#       separate(col = "exp.name.time", into = c("exp.name", "exp.time"), sep = "_") %>% 
#       mutate(exp.time = as.numeric(exp.time), 
#              exp.value = if_else(is.na(exp.value), NA, as.character(exp.value))) %>% 
#       group_by(ID) %>% 
#       arrange(ID, exp.time) %>% 
#       mutate(
#         his.name = name.history, 
#         his.time = exp.time, 
#         his.lag = if_else(exp.time == first(his.time, default = NA), "H", lag(exp.value)), 
#         his.value = CumPaste(his.lag)
#       ) %>% 
#       select(ID, all_of(c("his.name", "his.time", "his.value"))) %>% 
#       unite(col = "his.name.time", c(his.name, his.time), sep = "_") %>% 
#       pivot_wider(names_from = his.name.time, values_from = his.value)
#     
#     output <- input %>% left_join(input.temp, by = "ID") %>% 
#       rename(`:=`(!!s_id, ID)) %>% data.frame()
#     return(output)
#   } else if (!is.null(group)) {
#     s_group <- sym(group)
#     input.temp <- input %>% ungroup() %>% 
#       select(ID, all_of(list.exposure), !!s_group) %>% 
#       rename(GROUP = !!s_group) %>% 
#       pivot_longer(all_of(list.exposure) & !GROUP, 
#                    names_to = "exp.name.time", values_to = "exp.value") %>% 
#       separate(col = "exp.name.time", into = c("exp.name", "exp.time"), sep = "_") %>% 
#       mutate(
#         exp.time = as.numeric(exp.time), 
#         exp.value = ifelse(is.na(exp.value), NA, as.character(exp.value))
#       ) %>% 
#       group_by(ID) %>% 
#       arrange(ID, exp.time) %>% 
#       mutate(
#         his.name = name.history, 
#         his.time = exp.time, 
#         his.lag = if_else(his.time == first(his.time, default = NA), "H", lag(exp.value)), 
#         his.temp = CumPaste(his.lag), 
#         his.value = str_c(GROUP, his.temp)
#       ) %>% 
#       select(ID, c("his.name", "his.time", "his.value")) %>% 
#       unite(col = "his.name.time", c(his.name, his.time), sep = "_") %>% 
#       pivot_wider(names_from = his.name.time, values_from = his.value)
#     
#     output <- input %>% left_join(input.temp, by = "ID") %>% 
#       rename(`:=`(!!s_id, ID)) %>% data.frame()
#     return(output)
#   }
# }

makehistory.one <- function(input, id, times, group = NULL, exposure, name.history = "h") 
{
  input <- ungroup(input)
  list.exposure <- paste(exposure, times, sep = "_")
  
  s_id <- sym(id)
  input <- input %>% rename(ID = !!s_id)
  
  CumPaste = function(x, .sep = "") {
    Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, 
           accumulate = TRUE)
  }
  
  input.temp <- input %>% ungroup() %>% 
    select(ID, all_of(list.exposure)) %>% 
    pivot_longer(all_of(list.exposure), 
                 names_to = "exp.name.time", values_to = "exp.value") %>% 
    separate(col = "exp.name.time", into = c("exp.name", "exp.time"), sep = "_") %>% 
    mutate(exp.time = as.numeric(exp.time), 
           exp.value = if_else(is.na(exp.value), NA, as.character(exp.value))) %>% 
    group_by(ID) %>% 
    arrange(ID, exp.time) %>% 
    mutate(
      his.name = name.history, 
      his.time = exp.time, 
      his.lag = if_else(exp.time == first(his.time, default = NA), "H", lag(exp.value)), 
      his.value = CumPaste(his.lag)
    ) %>% 
    select(ID, all_of(c("his.name", "his.time", "his.value"))) %>% 
    unite(col = "his.name.time", c(his.name, his.time), sep = "_") %>% 
    pivot_wider(names_from = his.name.time, values_from = his.value)
  
  output <- input %>% left_join(input.temp, by = "ID") %>% 
    rename(`:=`(!!s_id, ID)) %>% data.frame()
  return(output)
  
}

# simplified version of the makeplot() function in confoundr
# plots SMD at all given timepoints (scope = all)
cov_bal_plot <- function(
    data, # summary data with SMD (output from balance())
    # give the covariates so that they can be ordered in the plot
    static.covariate, # = static_covars_edited,
    temporal.covariate, # = c("STROKE", tv_covars_edited), 
    static.covariate_labels, temporal.covariate.labels, 
    # plotting arguments
    label.exposure = "A",
    label.covariate = "C",
    lbound = -1.5,
    ubound = 1.5,
    ratio = 1.5,
    text.axis.title = 9,
    text.axis.y = 9,
    text.axis.x = 9,
    text.strip.y = 9,
    text.strip.x = 9,
    point.size = 0.75,
    zeroline.size = 0.1,
    refline.size = 0.1,
    refline.limit.a = -0.25,
    refline.limit.b = 0.25,
    panel.spacing.size = 0.75,
    axis.title = NULL,
    label.width = 10,
    legend.position = "bottom",
    text.legend = NULL
) { 
  # testing argument
  # data <- tbl_covbal
  # static.covariate <- static_vars
  # temporal.covariate <- tv_vars
  # static.covariate_labels <- static_vars_label
  # temporal.covariate.labels <- tv_vars_label
  # label.exposure <- "Stroke"
  # label.covariate <- "Covariate History"
  
  # lbound = -bound
  # ubound = bound
  # axis.title = fig_label

  
  # set up covariate balance df
  labelled.input <- data %>% 
    ungroup() %>% 
    mutate(
      # exposure = paste(label.exposure, "(", time.exposure, ")", sep = ""),
      exposure = paste(label.exposure, "at Year", time.exposure),
      # covariate = paste(label.covariate, "(", time.covariate, ")", sep = ""),
      covariate = paste(label.covariate, "at Year", time.covariate), 
      comparison = paste(exposure, " vs ", covariate, sep = ""), 
      # re-order the covariates based on the type
      name.cov = factor(name.cov, levels = c(static.covariate, temporal.covariate), 
                        labels = c(static.covariate_labels, temporal.covariate.labels))
    )
  values.exposure <- labelled.input %>% distinct(time.exposure, exposure)
  values.covariate <- labelled.input %>% distinct(time.covariate, covariate)
  values.comparison <- labelled.input %>% distinct(comparison, time.exposure, time.covariate)
  
  AscendOrderExposure <- arrange(values.exposure, time.exposure)
  AscendOrderCovariate <- arrange(values.covariate, time.covariate)
  DescendOrderExposure <- arrange(values.exposure, desc(time.exposure))
  DescendOrderCovariate <- arrange(values.covariate, desc(time.covariate))
  AscendOrderComparison <- arrange(values.comparison, time.exposure, time.covariate)
  
  labelled.input <- labelled.input %>%
    mutate(
      exposure = factor(exposure, levels = AscendOrderExposure$exposure),
      covariate = factor(covariate, levels = AscendOrderCovariate$covariate),
      rev.exposure = factor(exposure, levels = DescendOrderExposure$exposure),
      rev.covariate = factor(covariate, levels = DescendOrderCovariate$covariate),
      comparison = factor(comparison, levels = AscendOrderComparison$comparison)
    )
  
  # set up labels for number of subjects, total and exposed, at each time point
  counts_label <- labelled.input %>% filter(time.covariate == time.exposure) %>% 
    distinct(E, H, rev.exposure, covariate, wt, N, Nexp) %>% 
    pivot_wider(
      id_cols = c(E, H, rev.exposure, covariate), 
      names_from = wt, 
      values_from = c(N, Nexp)
    ) %>% 
    mutate(
      name.cov = factor(static.covariate[2], levels = c(static.covariate, temporal.covariate)),
      across(c(starts_with("N_"), starts_with("Nexp_")), round), 
      N_label = paste0("N=", N_unweighted, "/", N_weighted), 
      Nexp_label = paste0("N(", label.exposure, ")=", Nexp_unweighted, "/", Nexp_weighted), 
    ) 
  
  plot_df <- rbind(
    labelled.input %>% 
      mutate(label = NA) %>% 
      select(name.cov, rev.exposure, covariate, SMD, wt, label)
    # 
    # counts_label %>% 
    #   mutate(SMD = NA, wt = NA, label = paste0(N_label, "\n", Nexp_label)) %>% 
    #   select(name.cov, rev.exposure, covariate, SMD, wt, label)
    # 
  ) 
  
  # set up plot theme
  themes <- theme(
    aspect.ratio = ratio, 
    axis.title = element_text(size = text.axis.title, face = "bold"), 
    axis.text.y = element_text(size = text.axis.y, colour = "black", vjust = 0.33), 
    axis.text.x = element_text(size = text.axis.x, colour = "black"), 
    axis.ticks = element_blank(), strip.text.y = element_text(size = text.strip.y), 
    strip.text.x = element_text(size = text.strip.x), strip.background = element_blank(), 
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
    panel.background = element_blank(), panel.spacing = unit(panel.spacing.size, "lines"), 
    legend.position = legend.position, legend.key = element_rect(fill = NA, color = NA), 
    legend.title = element_text(size = text.legend), 
    legend.text = element_text(size = text.legend))
  
  
  final.plot <- ggplot(plot_df, aes(x = name.cov)) +
    geom_point(aes(y = SMD, color = wt), size = point.size, na.rm = TRUE) + 
    # geom_text(aes(y = 0, label = label), size = text.axis.x * 0.4) + 
    scale_x_discrete(limits = rev) +
    scale_color_discrete(na.translate = F) + 
    coord_flip() +
    ylim(lbound, ubound) +
    facet_grid(
      rev.exposure ~ covariate,
      labeller = label_wrap_gen(width = label.width, multi_line = TRUE)
    ) + 
    labs(x = axis.title, y = "Standardized Mean Difference", color = "") +
    geom_hline(
      yintercept = c(refline.limit.a, refline.limit.b),
      linetype = "dotted", colour = alpha("black", 1/3),
      linewidth = refline.size
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "solid", colour = alpha("black", 0.5),
      linewidth = zeroline.size
    ) +
    themes
  
  
  # temp.plot <- labelled.input %>% 
  #   # group_by(H, E) %>% 
  #   ggplot(aes(x = name.cov)) +
  #   geom_point(aes(y = SMD, color = wt), size = point.size, na.rm = TRUE) + 
  #   scale_x_discrete(limits = rev) +
  #   coord_flip() +
  #   ylim(lbound, ubound) +
  #   facet_grid(rev.exposure ~ covariate,
  #              labeller = label_wrap_gen(width = label.width, multi_line = TRUE))
  
  
  
  # final.plot <- temp.plot + 
  #   geom_text(
  #     data = counts_label,
  #     aes(x = name.cov, y = 0, label = N_label), size = text.axis.x * 0.4
  #   ) + 
  #   geom_text(
  #     data = counts_label, 
  #     aes(x = " ", y = 0, label = Nexp_label), size = text.axis.x * 0.4
  #   ) + 
  #   labs(x = axis.title, y = "Covariate", color = "") + 
  #   geom_hline(
  #     yintercept = c(refline.limit.a, refline.limit.b), 
  #     linetype = "dotted", colour = alpha("black", 1/3), 
  #     linewidth = refline.size
  #   ) + 
  #   geom_hline(
  #     yintercept = 0, 
  #     linetype = "solid", colour = alpha("black", 0.5), 
  #     linewidth = zeroline.size
  #   ) +
  #   themes
  
  return(final.plot)
}

# test the function
# cov_bal_plot(censor_check$tbl_covbal, 
#              static.covariate = static_covars_edited,
#              temporal.covariate = c("STROKE", tv_covars_edited),
#              label.exposure = "Cens"
#              )
# cov_bal_plot(stroke_check$tbl_covbal, 
#              static.covariate = static_covars_edited,
#              temporal.covariate = c("STROKE", tv_covars_edited), 
#              label.exposure = "Strk")


# wrapper function to check IPTW 
check_IPTW <- function(longdata, wt_to_test, timepoints, 
                       tv_vars = tv_covars, 
                       static_vars = static_covars, 
                       tv_vars_label = tv_covars_label,
                       static_vars_label = static_covars_label,
                       stroke_defn = "stroke_combined",
                       fig_label = ethn # ethnicity
) {
  # testing arguments
  # longdata <- long_data_sub
  # wt_to_test <- "wt_stroke"
  # timepoints <- c(0, 1, 4, 8)
  # tv_vars <- tv_covars
  # static_vars <- static_covars
  # tv_vars_label <- tv_covars_label
  # static_vars_label <- static_covars_label
  # stroke_defn <- "stroke_combined"
  # fig_label <- ethn
  
  wt_to_test <- wt_to_test %>% str_remove_all("_")
  tv_vars <- tv_vars %>% str_remove_all("_")
  static_vars <- static_vars %>% str_remove_all("_") %>% str_remove_all(" ")
  
  longdata <- longdata %>% 
    group_by(subjid) %>% 
    # only include up to when a subject first developed stroke
    mutate(include = ifelse(get(stroke_defn) == 1 & lag(get(stroke_defn) == 1), FALSE, TRUE), 
           include = ifelse(is.na(include), TRUE, include)) %>% 
    filter(include) %>% 
    select(-include)
  
  # remove "_" from variable names
  names(longdata) <- str_remove_all(names(longdata), "_") %>% str_remove_all(" ")
  stroke_defn <- str_remove_all(stroke_defn, "_") %>% str_remove_all(" ")
  
  # use confoundr from this point on
  # this function is fast and not a problem
  df_w <- widen(
    input = longdata,
    id = "subjid",
    time = "fuyr",
    exposure = stroke_defn,
    covariate = c(tv_vars, static_vars),
    weight.exposure = wt_to_test
  )
  
  # make exposure history variable 
  # this calls the fixed version of the function
  df_wh_one <- makehistory.one(
    input = df_w, 
    id = "subjid", 
    # times = 0:18, 
    times = timepoints, 
    exposure = stroke_defn, 
    name.history = "h"
  )
  
  # lengthen to the format for sequential exchangeability
  df_wh_one_l <- lengthen(
    input = df_wh_one,
    id = "subjid",
    diagnostic = 3, # the weighted diagnostic
    censoring = "no",
    exposure = stroke_defn, 
    weight.exposure = wt_to_test,
    static.covariate = static_vars,
    temporal.covariate = tv_vars, 
    # times.exposure = 0:18,
    # times.covariate = 0:18,
    times.exposure = timepoints,
    times.covariate = timepoints,
    history = "h"
  )
  
  # calculate covariate balance
  ## unweighted
  baltbl1 <- balance(
    input = df_wh_one_l %>% mutate(W = 1), # create W = 1 for unweighted
    diagnostic = 3,
    approach = "weight",
    weight.exposure = "W",
    censoring = "no",
    scope = "all",
    exposure = stroke_defn,
    history = "h",
    times.exposure = timepoints,
    times.covariate = timepoints,
    sd.ref = "yes"
  )
  ## weighted
  baltbl3 <- balance(
    input = df_wh_one_l,
    diagnostic = 3,
    approach = "weight",
    weight.exposure = wt_to_test,
    censoring = "no",
    scope = "all",
    exposure = stroke_defn,
    history = "h",
    times.exposure = timepoints,
    times.covariate = timepoints,
    sd.ref = "yes"
  )
  
  # combine the tables for output
  tbl_covbal <- rbind(
    baltbl1 %>% mutate(wt = "unweighted"), 
    baltbl3 %>% mutate(wt = "weighted")
  )
  
  # covariate balance plots 
  # p_covbal_unw <- makeplot(
  #   input = baltbl1,
  #   diagnostic = 1,
  #   approach = "none",
  #   censoring = "no",
  #   scope = "all",
  #   label.exposure = "Stroke",
  #   label.covariate = "Covariate",
  #   ratio = 1
  # ) + labs(title = "unweighted")
  # 
  # p_covbal_wt <- makeplot(
  #   input = baltbl3,
  #   diagnostic = 3,
  #   approach = "weight",
  #   censoring = "no",
  #   scope = "all",
  #   label.exposure = "Stroke",
  #   label.covariate = "Covariate",
  #   ratio = 1
  # ) + labs(title = "weighted")
  
  bound <- max(abs(tbl_covbal$SMD)) %>% 
    cut(breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) %>% 
    str_split_i(",", 2) %>% 
    str_remove("]") %>% 
    as.numeric()
  
  p_covbal <- cov_bal_plot(
    tbl_covbal,
    static.covariate = static_vars,
    temporal.covariate = tv_vars,
    static.covariate_labels = static_vars_label, 
    temporal.covariate.labels = tv_vars_label,
    label.exposure = "Stroke",
    label.covariate = "Covariate History",
    lbound = -bound,
    ubound = bound,
    ratio = 0.8, 
    axis.title = fig_label
  )
  
  return(
    list(
      tbl_covbal = tbl_covbal, 
      # p_covbal_unw = p_covbal_unw, 
      # p_covbal_wt = p_covbal_wt
      p_covbal = p_covbal
    )
  )
}

# wrapper function to check IPCW 
check_IPCW <- function(longdata, wt_to_test, timepoints,
                       # check_tv = check_tv, 
                       tv_vars = tv_covars, 
                       static_vars = static_covars, 
                       tv_vars_label = tv_covars_label,
                       static_vars_label = static_covars_label,
                       stroke_defn = "stroke_combined",
                       fig_label = ethn # ethnicity
) {
  # testing arguments
  # longdata <- long_data_sub
  # wt_to_test <- "wt_cens"
  # timepoints <- c(0, 1, 4, 8)
  # tv_vars <- tv_covars
  # stroke_defn <- "stroke_combined"
  # static_vars <- static_covars
  # tv_vars_label = tv_covars_label
  # static_vars_label = static_covars_label
  
  # tv_vars <- tv_covars %>% str_remove_all("_")
  # if (!is.null(static_vars)) {
  #   static_vars <- static_vars %>% str_remove_all("_")
  # }
  # wt_to_test <- wt_to_test %>% str_remove_all("_")
  # 
  # 
  # if (check_tv) {tv_vars <- c("STROKE", tv_vars)}
  
  wt_to_test <- wt_to_test %>% str_remove_all("_")
  tv_vars <- c(stroke_defn, tv_vars)
  tv_vars <- tv_vars %>% str_remove_all("_")
  
  tv_vars_label <- c("Stroke", tv_vars_label)
  
  static_vars <- static_vars %>% str_remove_all("_") %>% str_remove_all(" ")
  
  names(longdata) <- str_remove_all(names(longdata), "_") %>% str_remove_all(" ")
  
  df_w <- widen(
    input = longdata,
    id = "subjid",
    time = "fuyr",
    exposure = "eventcens",
    covariate = c(static_vars, tv_vars),
    weight.exposure = wt_to_test
  )
  
  df_wh_one <- makehistory.one(
    input = df_w,
    id = "subjid",
    exposure = "eventcens", # trt indicator (censoring is a type of trt)
    # times = 0:18, # time points, used as suffix
    times = timepoints, # time points, used as suffix
    name.history = "h"
  )
  
  df_wh_one_l <- lengthen(
    input = df_wh_one,
    id = "subjid",
    diagnostic = 3,
    censoring = "no",
    exposure = "eventcens", 
    weight.exposure = wt_to_test,
    static.covariate = static_vars,
    temporal.covariate = tv_vars, 
    # times.exposure = 0:18,
    # times.covariate = 0:18,
    times.exposure = timepoints,
    times.covariate = timepoints,
    history = "h"
  )
  
  # calculate covariate balance
  ## unweighted
  baltbl1 <- balance(
    input = df_wh_one_l %>% mutate(W = 1),
    diagnostic = 3,
    approach = "weight",
    weight.exposure = "W",
    censoring = "no",
    scope = "all",
    exposure = "eventcens",
    history = "h",
    times.exposure = timepoints,
    times.covariate = timepoints,
    sd.ref = "yes"
  )
  
  baltbl3 <- balance(
    input = df_wh_one_l,
    diagnostic = 3,
    approach = "weight",
    weight.exposure = wt_to_test,
    censoring = "no",
    scope = "all",
    exposure = "eventcens",
    history = "h",
    times.exposure = timepoints,
    times.covariate = timepoints,
    sd.ref = "yes"
  )
  
  # combine the tables for output
  tbl_covbal <- rbind(
    baltbl1 %>% mutate(wt = "unweighted"), 
    baltbl3 %>% mutate(wt = "weighted")
  )
  
  # p_covbal_unw <- makeplot(
  #   input = baltbl1,
  #   diagnostic = 1,
  #   approach = "none",
  #   censoring = "no",
  #   scope = "all",
  #   label.exposure = "Censoring",
  #   label.covariate = "Covariate",
  #   ratio = 1
  # ) + labs(title = "unweighted")
  # 
  # p_covbal_wt <- makeplot(
  #   input = baltbl3,
  #   diagnostic = 3,
  #   approach = "weight",
  #   censoring = "no",
  #   scope = "all",
  #   label.exposure = "Censoring",
  #   label.covariate = "Covariate",
  #   ratio = 1
  # ) + labs(title = "weighted")
  
  bound <- max(abs(tbl_covbal$SMD)) %>% 
    cut(breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) %>% 
    str_split_i(",", 2) %>% 
    str_remove("]") %>% 
    as.numeric()
  
  p_covbal <- cov_bal_plot(
    tbl_covbal,
    static.covariate = static_vars,
    temporal.covariate = tv_vars,
    static.covariate_labels = static_vars_label, 
    temporal.covariate.labels = tv_vars_label,
    label.exposure = "Censored", 
    label.covariate = "Covariate History",
    lbound = -bound,
    ubound = bound,
    ratio = 0.8, 
    axis.title = fig_label
  )
  
  return(
    list(
      tbl_covbal = tbl_covbal, 
      # p_covbal_unw = p_covbal_unw, 
      # p_covbal_wt = p_covbal_wt
      p_covbal = p_covbal
    )
  )
  
}
