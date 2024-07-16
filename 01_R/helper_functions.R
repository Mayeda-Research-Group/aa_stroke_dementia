# functions to help generate weights 
generate_IPTW <- function(longdata, formula) {
  # testing arguments
  # longdata <- boot_data
  # formula <- formulas$stroke
  # or explicitly: 
  # formula <- c(
  #   "stroke_combined~ns(fu_yr, df = 4)",                                                                                                              
  #   "+ns(survey_age_r, df = 4)+female+education_4+generalhealth_3+current_smoker+prev_ami",
  #   "+diab+htn+inc_ami+chf+pvd+bmi+sbp+ihd+cancer+dyslip+tot_choles"
  # )
  
  # formula for the glm should be a vector of terms, where
  # the first element includes exposure as the outcome and the time term, 
  # and the following elements are additional covariate terms
  
  original_vars <- names(longdata)
  
  # extract the stroke defn from the formula
  stroke_defn <- str_split_i(formula[1], "~", 1)
    
  # create variable to flag paststroke 
  longdata <- longdata %>% 
    group_by(new_id) %>%
    mutate(
      paststroke = ifelse(get(stroke_defn) == 1 & lag(get(stroke_defn)) == 0, 
                          0, get(stroke_defn)),
      paststroke = ifelse(is.na(paststroke), 0, paststroke)
    ) %>%
    ungroup()
  
  # tryCatch records if there is a glm warning and 
  # accumulates the number of warnings
  tryCatch(
    {
      stroke_mod <<- glm(
        as.formula(paste0(formula, collapse = "")), # use the complete formula
        family = binomial(), # quasibinomial?
        data = subset(longdata, paststroke == 0))
      w_stroke_d <<- c(w_stroke_d, 0)
    },
    warning = function(w){
      # n_w <<- n_w + 1
      w_stroke_d <<- c(w_stroke_d, 1)
      stroke_mod <<- glm(
        as.formula(paste0(formula, collapse = "")), # use the complete formula
        family = binomial(), # quasibinomial?
        data = subset(longdata, paststroke == 0))
    })
  
  tryCatch(
    {
      stroke_mod_num <<- glm(
        as.formula(formula[1]), # only use the time term
        family = binomial(), # quasibinomial?
        data = subset(longdata, paststroke == 0))
      w_stroke_n <<- c(w_stroke_n, 0)
    },
    warning = function(w){
      w_stroke_n <<- c(w_stroke_n, 1)
      stroke_mod_num <<- glm(
        as.formula(formula[1]), # only use the time term
        family = binomial(), # quasibinomial?
        data = subset(longdata, paststroke == 0))
    })
  
  # denominator
  # stroke_mod <- glm(
  #   as.formula(paste0(formula, collapse = "")), # use the complete formula
  #   family = binomial(), # quasibinomial?
  #   data = subset(longdata, paststroke == 0))
  
  # numerator 
  # stroke_mod_num <- glm(
  #   as.formula(formula[1]), # only use the time term
  #   family = binomial(), # quasibinomial?
  #   data = subset(longdata, paststroke == 0))
  
  # combine and calculate IPTW
  longdata <- longdata %>% 
    mutate(
      pred_stroke_denom = ifelse(
        paststroke == 1, 
        1, # the prob of having stroke is 1 for those with stroke already
        predict(stroke_mod, newdata = longdata, type = 'response')
      ), 
      pred_stroke_num = ifelse(
        paststroke == 1, 
        1, predict(stroke_mod_num, newdata = longdata, type = 'response')
      ), 
      stroke_denom = ifelse(get(stroke_defn) == 1, pred_stroke_denom, 1 - pred_stroke_denom),
      stroke_num = ifelse(get(stroke_defn) == 1, pred_stroke_num, 1 - pred_stroke_num)
    ) %>% 
    group_by(new_id) %>% 
    mutate(
      denom_cum = cumprod(stroke_denom), 
      num_cum = cumprod(stroke_num)
    ) %>% 
    ungroup() %>% 
    mutate(
      wt_stroke = 1 / denom_cum, # unstabilized 
      wt_stroke_stab = num_cum / denom_cum, # stabilized
      # apply truncation
      wt_stroke_trunc = ifelse(
        wt_stroke > quantile(wt_stroke, 0.99),
        quantile(wt_stroke, 0.99), wt_stroke
      ), 
      wt_stroke_stab_trunc = ifelse(
        wt_stroke_stab > quantile(wt_stroke_stab, 0.99), 
        quantile(wt_stroke_stab, 0.99), wt_stroke_stab
      )
    )
  
  # exclude intermediate vars from the output dataset 
  longdata <- longdata %>% select(all_of(original_vars), starts_with("wt_"))
  return(longdata)
}

generate_IPCW <- function(longdata, formula1, formula2) {
  # testing arguments
  # longdata <- boot_data
  # formula1 <- formulas$death
  # formula2 <- formulas$endofMem
  
  original_vars <- names(longdata)
  
  ## death as censoring event ----
  
  # denominator
  tryCatch(
    {
      plrFit_Death <<- glm(as.formula(paste0(formula1, collapse = "")), 
                           data = longdata, family = binomial())
      w_death_d <<- c(w_death_d, 0)
    },
    warning = function(w){
      w_death_d <<- c(w_death_d, 1)
      plrFit_Death <<- glm(as.formula(paste0(formula1, collapse = "")), 
                           data = longdata, family = binomial())
    })
  
  # numerator for stabilized weights
  tryCatch(
    {
      plrFit_Death_num <<- glm(as.formula(formula1[1]), 
                               data = longdata, family = binomial())
      w_death_n <<- c(w_death_n, 0)
    },
    warning = function(w){
      w_death_n <<- c(w_death_n, 1)
      plrFit_Death_num <<- glm(as.formula(formula1[1]), 
                               data = longdata, family = binomial())
    })
  
  longdata <- longdata %>% 
    mutate(
      predDeath = 1 - predict(plrFit_Death, newdata = longdata, type = "response"), 
      predDeath_num = 1 - predict(plrFit_Death_num, newdata = longdata, type = "response")
    ) %>%
    group_by(new_id) %>% 
    mutate(
      denom_cum_death = cumprod(predDeath), 
      num_cum_death = cumprod(predDeath_num)
    ) %>% 
    ungroup()
  
  ## end of membership censoring ----
  # denominator
  # plrFitC <- glm(as.formula(paste0(formula2, collapse = "")), 
  #                data = longdata, family = binomial())
  
  tryCatch(
    {
      plrFitC <<- glm(as.formula(paste0(formula2, collapse = "")), 
                     data = longdata, family = binomial())
      w_endofMem_d <<- c(w_endofMem_d, 0)
    },
    warning = function(w){
      w_endofMem_d <<- c(w_endofMem_d, 1)
      plrFitC <<- glm(as.formula(paste0(formula2, collapse = "")), 
                      data = longdata, family = binomial())
    })
  
  # numerator
  # plrFitCnum <- glm(as.formula(formula2[1]), 
  #                   data = longdata, family = binomial())
  
  tryCatch(
    {
      plrFitCnum <<- glm(as.formula(formula2[1]), 
                        data = longdata, family = binomial())
      w_endofMem_n <<- c(w_endofMem_n, 0)
    },
    warning = function(w){
      w_endofMem_n <<- c(w_endofMem_n, 1)
      plrFitCnum <<- glm(as.formula(formula2[1]), 
                         data = longdata, family = binomial())
    })
  
  longdata <- longdata %>% 
    mutate(
      predC = 1 - predict(plrFitC, newdata = longdata, type = "response"),
      predC_num = 1 - predict(plrFitCnum, newdata = longdata, type = "response")
    ) %>% 
    group_by(new_id) %>% 
    mutate(
      cumPredC = cumprod(predC), 
      cumPredC_num = cumprod(predC_num)
    ) %>% 
    ungroup()
  
  ## calculate weights ----
  longdata <- longdata %>% 
    mutate(
      # censoring due to end of membership
      wt_endmem = 1 / cumPredC, # unstabilized
      wt_endmem_stab = cumPredC_num / cumPredC, # stabilized
      # censoring due to death
      wt_death = 1 / denom_cum_death,
      wt_death_stab = num_cum_death / denom_cum_death,
      
      # combined censoring weights
      wt_cens = wt_endmem * wt_death,
      wt_cens_stab = wt_endmem_stab * wt_death_stab,
      
      # truncate death weights
      # remember to remove na.rm = TRUE after imputation
      wt_death_trunc = ifelse(
        wt_death > quantile(wt_death, 0.99), 
        quantile(wt_death, 0.99), wt_death
      ), 
      wt_death_stab_trunc = ifelse(
        wt_death_stab > quantile(wt_death_stab, 0.99), 
        quantile(wt_death_stab, 0.99), wt_death_stab
      )
    )
  
  # exclude intermediate variables
  longdata <- longdata %>% select(all_of(original_vars), starts_with("wt_"))
  return(longdata)
}


# function to plot KM curves
plot_km <- function(km_object, title_text) {
  p <- km_object %>% 
    broom::tidy() %>% 
    mutate(
      stroke = ifelse(str_detect(strata, "1"), "Yes", "No"), 
      ethn = str_sub(strata, 25), 
      cif = 1 - estimate, 
      cif.conf.low = 1 - conf.low, 
      cif.conf.high = 1 - conf.high, 
    ) %>% 
    ggplot(aes(time, cif)) +
    geom_line(aes(color = stroke), linewidth = 0.7) +
    geom_ribbon(aes(ymin = cif.conf.low, ymax = cif.conf.high, group = stroke), alpha = 0.1) +
    scale_x_continuous(
      # limits = c(0, 17),
      breaks = seq(0, 18, 3), minor_breaks = 0:18) +
    labs(title = title_text) + 
    theme_bw() + 
    facet_wrap(~ethn)
  return(p)
}

# function to clean up KM curves for bootstrap
clean_KM <- function(km_object, model_type, check = TRUE) {
  # km_object <- KM_output
  # model_type <- "crude"

  # km_object <- test
  temp <- km_object %>% 
    broom::tidy() %>% 
    mutate(
      CIF = 1 - estimate, 
      Stroke = str_split_i(strata, ",", 1) %>% str_sub(-1),
      Ethnicity = str_split_i(strata, ",", 2) %>% str_split_i("=", 2) %>% str_trim()
    ) %>% 
    select(time, CIF, Stroke, Ethnicity)
  
  ethn <- unique(temp$Ethnicity)
  temp <- temp %>% select(-Ethnicity)
  
  if (check) {
    # need some code here to fix scenario missing timepoints
    # first fix the first timepoint stratified by stroke
    # I'm assuming that at Time = 0 (no stroke) and Time = 1 (stroke), CIF = 0
    # might need to confirm though 
    temp_no_stroke <- temp %>% 
      filter(Stroke == 0) %>% 
      full_join(tibble(time = 0:18), by = "time") %>% 
      arrange(time) %>% 
      mutate(
        CIF = ifelse(time == 0 & is.na(CIF), 0, CIF),
        Stroke = 0
      ) %>% na.omit()
    temp_stroke <- temp %>% 
      filter(Stroke == 1) %>% 
      full_join(tibble(time = 1:18), by = "time") %>% 
      arrange(time) %>% 
      mutate(
        CIF = ifelse(time == 1 & is.na(CIF), 0, CIF),
        Stroke = 1
      ) %>% na.omit()
    temp <- rbind(temp_no_stroke, temp_stroke)
    curr_rows <- temp %>% distinct(time, Stroke) %>% nrow()
    
    # fix subsequent timepoints in a loop
    while (curr_rows < 37) {
      # this means that some timepoints are missing
      # carry forward the last value until no timepoints are missing
      
      temp_no_stroke <- temp %>% 
        filter(Stroke == 0) %>% 
        full_join(tibble(time = 0:18), by = "time") %>% 
        arrange(time) %>% 
        mutate(
          across(c(CIF, Stroke), function(x) {ifelse(is.na(x), lag(x), x)})
        )
      
      temp_stroke <- temp %>% 
        filter(Stroke == 1) %>% 
        full_join(tibble(time = 1:18), by = "time") %>% 
        arrange(time) %>% 
        mutate(
          across(c(CIF, Stroke), function(x) {ifelse(is.na(x), lag(x), x)})
        ) %>% na.omit()
      
      temp <- rbind(temp_no_stroke, temp_stroke)
      curr_rows <- temp %>% distinct(time, Stroke) %>% nrow()
      
    }
  }

  temp <- temp %>% 
    mutate(label = paste0("Stroke_", Stroke, "_time_", time))
  
  out <- c(ethn, model_type, temp$CIF)
  names(out) <- c("Ethnicity", "Model", temp$label)
  return(out)
}

clean_AJ <- function(aj_object, model_type, check = TRUE) {

  temp <- aj_object %>% broom::tidy() %>% 
    filter(state == "1") %>% 
    mutate(
      CIF = estimate, 
      Stroke = str_split_i(strata, ",", 1) %>% str_sub(-1),
      Ethnicity = str_split_i(strata, ",", 2) %>% str_split_i("=", 2) %>% str_trim()
    )  %>% 
    select(time, CIF, Stroke, Ethnicity)
  
  ethn <- unique(temp$Ethnicity)
  temp <- temp %>% select(-Ethnicity)
  
  if (check) {
    # need some code here to fix scenario missing timepoints
    # first fix the first timepoint stratified by stroke
    # I'm assuming that at Time = 0 (no stroke) and Time = 1 (stroke), CIF = 0
    # might need to confirm though 
    temp_no_stroke <- temp %>% 
      filter(Stroke == 0) %>% 
      full_join(tibble(time = 0:18), by = "time") %>% 
      arrange(time) %>% 
      mutate(
        CIF = ifelse(time == 0 & is.na(CIF), 0, CIF),
        Stroke = 0
      ) %>% na.omit()
    temp_stroke <- temp %>% 
      filter(Stroke == 1) %>% 
      full_join(tibble(time = 1:18), by = "time") %>% 
      arrange(time) %>% 
      mutate(
        CIF = ifelse(time == 1 & is.na(CIF), 0, CIF),
        Stroke = 1
      ) %>% na.omit()
    temp <- rbind(temp_no_stroke, temp_stroke)
    curr_rows <- temp %>% distinct(time, Stroke) %>% nrow()
    
    # fix subsequent timepoints in a loop
    while (curr_rows < 37) {
      # this means that some timepoints are missing
      # carry forward the last value until no timepoints are missing
      
      temp_no_stroke <- temp %>% 
        filter(Stroke == 0) %>% 
        full_join(tibble(time = 0:18), by = "time") %>% 
        arrange(time) %>% 
        mutate(
          across(c(CIF, Stroke), function(x) {ifelse(is.na(x), lag(x), x)})
        )
      
      temp_stroke <- temp %>% 
        filter(Stroke == 1) %>% 
        full_join(tibble(time = 1:18), by = "time") %>% 
        arrange(time) %>% 
        mutate(
          across(c(CIF, Stroke), function(x) {ifelse(is.na(x), lag(x), x)})
        ) %>% na.omit()
      
      temp <- rbind(temp_no_stroke, temp_stroke)
      curr_rows <- temp %>% distinct(time, Stroke) %>% nrow()
      
    }
  }
  
  temp <- temp %>% 
    mutate(label = paste0("Stroke_", Stroke, "_time_", time))
  
  out <- c(ethn, model_type, temp$CIF)
  names(out) <- c("Ethnicity", "Model", temp$label)
  return(out)
}


# calculate_RR_RD <- function(cleaned_km_object, time_of_int = 15, py_scale) {
#   # cleaned_km_object <- KM_crude
#   # time_of_int <- 15
#   
#   ethn <- unique(cleaned_km_object$Ethnicity) %>% unlist()
#   model <- unique(cleaned_km_object$model) %>% unlist()
#   
#   risks <- cleaned_km_object %>% filter(time == time_of_int)
#   RD <- (risks[2, "CIF"] - risks[1, "CIF"]) %>% unlist(use.names = FALSE)
#   RR <- (risks[2, "CIF"] / risks[1, "CIF"]) %>% unlist(use.names = FALSE)
#   
#   out <- c(ethn, model, RD, RR)
#   names(out) <- c("Ethnicity", "Model", "RD", "RR")
#   return(out)
# }


