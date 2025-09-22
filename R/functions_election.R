# Election Impact


correct_outliers_IQR <- function(data, # df might be misleading, since this parent function has also a df.
                                 column_name="pct_RDmargin_change", multiplier = 1.5) {
  
  # not data, return values
  values <- data[[column_name]]
  
  Q1 <- quantile(values, 0.25, na.rm = TRUE)
  Q3 <- quantile(values, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower_fence <- Q1 - multiplier * IQR_val
  upper_fence <- Q3 + multiplier * IQR_val
  
  # identify outliers
  values[values < lower_fence] <- lower_fence # not Q1!!
  values[values > upper_fence] <- upper_fence # not Q3!!
  
  lower_outliers <- which(data[[column_name]] < lower_fence)
  data[[column_name]][lower_outliers] <- lower_fence # not Q1
  
  upper_outliers <- which(data[[column_name]] > upper_fence)
  data[[column_name]][upper_outliers] <- upper_fence
  
  return(values) # not data
}


ks_test_fnc <- function(df, outcome="pct_rep_change"){
  
  # Extract the pct_rep_change values for treatment and control groups
  control <- df %>% 
    filter(treatment == 0) %>% 
    pull(outcome) %>% 
    na.omit()
  
  treatment <- df %>% 
    filter(treatment == 1) %>% 
    pull(outcome) %>% 
    na.omit()
  
  # Perform Kolmogorov-Smirnov test
  ks_test_result <- ks.test(control, treatment)
  return(ks_test_result)
}



med_data_prep1 <- function(df){
  df <- df %>%
    mutate(
      # Due to large number of outliers and too much disturbance, you might want to try to filter them out,
      # data_med <- data_med %>%
      #   filter(abs(pct_rep_change) <30)
      
      # or winsorize, which seems better # or do we have to focus on competitive districts???
      # When I do this, I have to specify this in paper!!
      # but in the first place, do we need winsorizing? winsorizing seems to lead to larger treatment effect.
      
      pct_rep_change_w = winsor(pct_rep_change, trim = 0.05),
      pct_RDmargin_change_w = winsor(pct_RDmargin_change, trim = 0.05),
      # this is binary, and no outlier!!
      # incumbent_change_w = winsor(incumbent_change, trim = 0.05),
      
      # z-score winsorizing
      pct_rep_change_zw = winsor(scale(pct_rep_change)[,1], trim = 0.05),  #[,1] leads to z score.
      pct_RDmargin_change_zw = winsor(scale(pct_RDmargin_change)[,1], trim = 0.05),
      # incumbent_change_zw = winsor(scale(incumbent_change)[,1], trim = 0.05),
      
      # iqr+cap
      # here, we don't need to use ., instead of df, since it is ok to use df in the argument and do not
      # reflect earlier part of this function
      pct_rep_change_iqr = correct_outliers_IQR(df,"pct_rep_change", multiplier = 1.5),
      pct_RDmargin_change_iqr = correct_outliers_IQR(df,"pct_RDmargin_change", multiplier = 1.5),
      # incumbent_change_iqr = correct_outliers_IQR(df,"incumbent_change", multiplier = 1.5),
      
      
      # Now, for visualization, factorize numberic data, while you can factorize anytime after this.
      # any_incident_med, defined below, is numeric now, not factor
      incident_window_med_fac = factor(incident_window_re,
                                       # labeling is beneficial for visualization
                                       , levels = c(0, 1),
                                       # To show in boxplots, it is not 0/1, but using charcters as below
                                       # labels = c("No incident", "Incident occurred")
                                       labels = c("Control", "Treatment")
      )) %>% 
    drop_na(incident_window_med_fac) %>% 
    mutate(treatment=incident_window_re) 
  # for simplicity, make treatment var. names the same as 2-year version
  
  return(df)
}


med_data_group <- function(df){
  df <- df %>%   
    group_by(district, election_year_next) %>%
    summarise(
      # identify treatment group
      
      # If you want to use 2-year-basis data, instead of incident_window, we should focus on incident
      # When you use monthly data, incident window would be your choice
      # incident_window_remed = if_else(sum(incident_window_remed, na.rm = TRUE) > 0, 1L, 0L), # binary, since as.integer is logic form
      
      any_incident_med = if_else(sum(incident, # incident is an appropriate var. to use for filtering
                                     na.rm = TRUE) > 0, 1L, 0L), # L in 1L is for specifying that it is an integer
      treatment = any_incident_med,
      # need to pick up election data
      # of course, you can choose mean(),but first() can be more simple if every info is the same
      # but, since now we bundle two years, annual var. might have two patterns.
      # So in that case, you cannnot use first func.
      
      pct_rep_before = mean(pct_rep_before, na.rm = TRUE),
      pct_rep_next = mean(pct_rep_next, na.rm = TRUE),
      pct_RDmargin_next= mean(pct_RDmargin_next, na.rm = TRUE), 
      pct_RDmargin_before= mean(pct_RDmargin_before, na.rm = TRUE),
      rep_incumbent_before= mean(rep_incumbent_before, na.rm = TRUE),
      rep_incumbent_next= mean(rep_incumbent_next, na.rm = TRUE),
      
      pct_rep_change = mean(pct_rep_change, na.rm = TRUE),
      pct_RDmargin_change = mean(pct_RDmargin_change, na.rm = TRUE),
      incumbent_change = mean(incumbent_change, na.rm = TRUE),
      
      # simple winsorizing
      pct_rep_change_w = mean(pct_rep_change_w, na.rm = TRUE),
      pct_RDmargin_change_w = mean(pct_RDmargin_change_w, na.rm = TRUE),
      # incumbent_change_w = mean(incumbent_change_w, na.rm = TRUE),
      
      # z-score and winsorizing
      pct_rep_change_zw = mean(pct_rep_change_zw, na.rm = TRUE),
      pct_RDmargin_change_zw = mean(pct_RDmargin_change_zw, na.rm = TRUE),
      # incumbent_change_zw = mean(incumbent_change_zw, na.rm = TRUE),
      
      # based on iqr
      pct_rep_change_iqr = mean(pct_rep_change_iqr, na.rm = TRUE),
      pct_RDmargin_change_iqr = mean(pct_RDmargin_change_iqr, na.rm = TRUE),
      # incumbent_change_iqr = mean(incumbent_change_iqr, na.rm = TRUE),
      
      
      # covariates and var.; you have to pick up all the covariates you will use
      
      # 3month-present variables will not be needed.
      # instead, use these "raw" var. total during the election cycle matters much more.
      anti_sum_donations = mean(anti_sum_donations, na.rm = TRUE),
      anti_log_sum=log1p(anti_sum_donations),
      anti_don_number = mean(anti_don_number, na.rm=TRUE),
      
      pro_sum_donations = mean(pro_sum_donations, na.rm = TRUE),
      pro_log_sum=log1p(pro_sum_donations),
      pro_don_number = mean(pro_don_number, na.rm=TRUE),
      
      # also, only when aggregating to 2year, would be reasonable to use gap var.
      # when not aggregated, different timing from both sides does not seem to make sense a lot!
      gap_sum_donations = pro_sum_donations-anti_sum_donations,
      
      # since we used log's avg before, log's gap might be better.
      gap_gaplog_sum=log1p(gap_sum_donations), # log of gap
      gap_loggap_sum=pro_log_sum-anti_log_sum, # gap of log. better
      gap_don_number = pro_don_number-anti_don_number,
      
      bachelor = mean(bachelor, na.rm = TRUE),
      black = mean(black, na.rm = TRUE),
      white = mean(white, na.rm = TRUE),
      # hispanic=mean(hispanic, na.rm = TRUE),
      unemployment = mean(unemployment, na.rm = TRUE),
      log_income = log(mean(income, na.rm = TRUE)), # avg's log
      gun_ratio_pct = mean(gun_ratio_pct, na.rm = TRUE),
      # no need; month to election
      
      
      # for DID
      id = first(id, na.rm = TRUE),
      
      competitive_factor = competitive_factor %>% 
        table() %>% 
        which.max() %>% 
        names()
      # if you want to ungroup, this is similar to ungroup; almost identical
      # .groups = "drop"
      ) %>% 
    ungroup()
  
  df <- df %>% 
    group_by(district) %>%
    arrange(election_year_next, .by_group = TRUE) %>%
    mutate(
      pro_log_sum_lag1  = dplyr::lag(pro_log_sum, 1),
      anti_log_sum_lag1 = dplyr::lag(anti_log_sum, 1)
    ) %>%
    ungroup()
  
  df <- df %>% 
    group_by(district) %>% 
    # for event study
    arrange(election_year_next, .by_group = TRUE) %>%
    
    # for the SS impact
    mutate(
      next_treat_year = if_else(treatment == 1, election_year_next, NA_real_)
    ) %>%
    tidyr::fill(next_treat_year, .direction = "up") %>%
    mutate(
      time_to_treatment =  election_year_next-next_treat_year,
      # now, this is -2, -4, -6....
      time_to_treatment_cycle = as.integer(time_to_treatment / 2)
    ) %>%
    
    # for the PAC impact; using binary variable
    mutate(
      pro_PAC_year_20k = if_else(pro_sum_donations > 20000/12, election_year_next, NA_real_),
      anti_PAC_year_20k = if_else(anti_sum_donations > 20000/12, election_year_next, NA_real_),
      pro_PAC_year_10k = if_else(pro_sum_donations > 10000/12, election_year_next, NA_real_),
      anti_PAC_year_10k = if_else(anti_sum_donations > 10000/12, election_year_next, NA_real_),
      pro_PAC_year_30k = if_else(pro_sum_donations > 30000/12, election_year_next, NA_real_),
      anti_PAC_year_30k = if_else(anti_sum_donations > 30000/12, election_year_next, NA_real_),
      
      # for prospective event study
      pro_PAC_year_20k_future=pro_PAC_year_20k,
      pro_PAC_year_30k_future=pro_PAC_year_30k,
      pro_PAC_year_10k_future=pro_PAC_year_10k,
      anti_PAC_year_20k_future=anti_PAC_year_20k,
      anti_PAC_year_30k_future=anti_PAC_year_30k,
      anti_PAC_year_10k_future=anti_PAC_year_10k
      
      
    ) %>%
    tidyr::fill(pro_PAC_year_20k, .direction = "up") %>%
    tidyr::fill(anti_PAC_year_20k, .direction = "up") %>%
    tidyr::fill(pro_PAC_year_10k, .direction = "up") %>%
    tidyr::fill(anti_PAC_year_10k, .direction = "up") %>%
    tidyr::fill(pro_PAC_year_30k, .direction = "up") %>%
    tidyr::fill(anti_PAC_year_30k, .direction = "up") %>%
    tidyr::fill(pro_PAC_year_20k_future, .direction = "down") %>%
    tidyr::fill(anti_PAC_year_20k_future, .direction = "down") %>%
    tidyr::fill(pro_PAC_year_10k_future, .direction = "down") %>%
    tidyr::fill(anti_PAC_year_10k_future, .direction = "down") %>%
    tidyr::fill(pro_PAC_year_30k_future, .direction = "down") %>%
    tidyr::fill(anti_PAC_year_30k_future, .direction = "down") %>%
    
    mutate(
      time_to_proPAC_20k =  election_year_next - pro_PAC_year_20k,
      time_to_antiPAC_20k =  election_year_next - anti_PAC_year_20k,
      time_to_proPAC_10k =  election_year_next - pro_PAC_year_10k,
      time_to_antiPAC_10k =  election_year_next - anti_PAC_year_10k,
      time_to_proPAC_30k =  election_year_next - pro_PAC_year_30k,
      time_to_antiPAC_30k =  election_year_next - anti_PAC_year_30k,
      # now, this is -2, -4, -6....
      time_to_proPAC_cycle_20k = as.integer(time_to_proPAC_20k / 2),
      time_to_antiPAC_cycle_20k = as.integer(time_to_antiPAC_20k / 2),
      time_to_proPAC_cycle_10k = as.integer(time_to_proPAC_10k / 2),
      time_to_antiPAC_cycle_10k = as.integer(time_to_antiPAC_10k / 2),
      time_to_proPAC_cycle_30k = as.integer(time_to_proPAC_30k / 2),
      time_to_antiPAC_cycle_30k = as.integer(time_to_antiPAC_30k / 2),
      
      # for prospective analysis
      time_from_proPAC_20k =  election_year_next - pro_PAC_year_20k_future,
      time_from_antiPAC_20k =  election_year_next - anti_PAC_year_20k_future,
      time_from_proPAC_10k =  election_year_next - pro_PAC_year_10k_future,
      time_from_antiPAC_10k =  election_year_next - anti_PAC_year_10k_future,
      time_from_proPAC_30k =  election_year_next - pro_PAC_year_30k_future,
      time_from_antiPAC_30k =  election_year_next - anti_PAC_year_30k_future,
      # now, this is -2, -4, -6....
      time_from_proPAC_cycle_20k = as.integer(time_from_proPAC_20k / 2),
      time_from_antiPAC_cycle_20k = as.integer(time_from_antiPAC_20k / 2),
      time_from_proPAC_cycle_10k = as.integer(time_from_proPAC_10k / 2),
      time_from_antiPAC_cycle_10k = as.integer(time_from_antiPAC_10k / 2),
      time_from_proPAC_cycle_30k = as.integer(time_from_proPAC_30k / 2),
      time_from_antiPAC_cycle_30k = as.integer(time_from_antiPAC_30k / 2),
      
      # combining for prospective analysis
      time_from_proPAC_cycle_20k_combined=
        dplyr::coalesce(
          time_from_proPAC_cycle_20k,                                
          dplyr::if_else(time_to_proPAC_cycle_20k == -1L, -1L, NA_integer_)  
        ),
      # the same as below
      # case_when(
      #       !is.na(time_from_proPAC_cycle_20k) ~ as.integer(time_from_proPAC_cycle_20k),  
      #       is.na(time_from_proPAC_cycle_20k) & !is.na(time_to_proPAC_cycle_20k) &
      #         as.integer(time_to_proPAC_cycle_20k) == -1L ~ -1L,                   
      #       TRUE ~ NA_integer_
      #     )
      time_from_proPAC_cycle_30k_combined=
        dplyr::coalesce(
          time_from_proPAC_cycle_30k,                                
          dplyr::if_else(time_to_proPAC_cycle_30k == -1L, -1L, NA_integer_)  
        ),
      time_from_proPAC_cycle_10k_combined=
        dplyr::coalesce(
          time_from_proPAC_cycle_10k,                                
          dplyr::if_else(time_to_proPAC_cycle_10k == -1L, -1L, NA_integer_)  
        ),
      time_from_antiPAC_cycle_20k_combined=
        dplyr::coalesce(
          time_from_antiPAC_cycle_20k,                                
          dplyr::if_else(time_to_antiPAC_cycle_20k == -1L, -1L, NA_integer_)  
        ),
    time_from_antiPAC_cycle_30k_combined=
        dplyr::coalesce(
          time_from_antiPAC_cycle_30k,                                
          dplyr::if_else(time_to_antiPAC_cycle_30k == -1L, -1L, NA_integer_)  
        ),
      time_from_antiPAC_cycle_10k_combined=
        dplyr::coalesce(
          time_from_antiPAC_cycle_10k,                                
          dplyr::if_else(time_to_antiPAC_cycle_10k == -1L, -1L, NA_integer_)  
        )
    ) %>%
    ungroup()
  
  df <- df %>% 
    group_by(district, election_year_next)
  
  return(df)
}



calculate_PAC_share <- function(df # use grouped dataset
) {
  df <- df %>%
    group_by(election_year_next) %>%
    mutate(
      # national level
      anti_national_total = sum(anti_sum_donations, na.rm = TRUE),
      pro_national_total = sum(pro_sum_donations, na.rm = TRUE),
      both_national_total = anti_national_total + pro_national_total,
      
      # for each district
      district_total_contribution = anti_sum_donations + pro_sum_donations,
      
      # anti_share_pct = (anti_sum_donations / anti_national_total) * 100,
      # pro_share_pct = (pro_sum_donations / pro_national_total) * 100,
      # total_share_pct = (district_total_contribution / both_national_total) * 100,
      
      # just in case where total denominator is 0
      anti_share_pct = ifelse(anti_national_total > 0, 
                              (anti_sum_donations / anti_national_total) * 100, 
                              0),
      anti_share_logpct=log1p(anti_share_pct),
      pro_share_pct = ifelse(pro_national_total > 0, 
                             (pro_sum_donations / pro_national_total) * 100, 
                             0),
      pro_share_logpct = log1p(pro_share_pct),
      total_share_pct = ifelse(both_national_total > 0, 
                               (district_total_contribution / both_national_total) * 100, 
                               0),
      total_share_logpct= log1p(total_share_pct)
      
    ) %>%
    
    ungroup() %>% 
    
    mutate(
      anti_share_pct_IQR = correct_outliers_IQR(., "anti_share_pct"), 
      # if you use df, it is a dataframe in the argument, which does not reflect earlier part of this function 
      pro_share_pct_IQR = correct_outliers_IQR(., "pro_share_pct"),
      total_share_pct_IQR = correct_outliers_IQR(., "total_share_pct"),
      
      anti_share_pct_IQR1 = correct_outliers_IQR(., "anti_share_pct",1),
      pro_share_pct_IQR1 = correct_outliers_IQR(., "pro_share_pct",1),
      total_share_pct_IQR1 = correct_outliers_IQR(., "total_share_pct",1),
      
      anti_share_pct_IQR2 = correct_outliers_IQR(., "anti_share_pct",2),
      pro_share_pct_IQR2 = correct_outliers_IQR(., "pro_share_pct",2),
      total_share_pct_IQR2 = correct_outliers_IQR(., "total_share_pct",2)
    )
  
  
  return(df)
}



calculate_treatment_summary <- function(df) {
  df_summary <- df %>%
    group_by(election_year_next) %>%
    summarise(
      
      total_districts = n(),
      treatment_districts = sum(treatment == 1, na.rm = TRUE),
      treatment_district_share_pct = (treatment_districts / total_districts) * 100,
      
      comp_districts = sum(abs(pct_RDmargin_before) <= 5, na.rm = TRUE),
      comp_district_share_pct = (comp_districts / total_districts) * 100,
      
      total_anti_contributions = sum(anti_sum_donations, na.rm = TRUE), 
      # the same as anti_national_total for individual districts
      total_pro_contributions = sum(pro_sum_donations, na.rm = TRUE),
      total_all_contributions = total_anti_contributions + total_pro_contributions,
      
      
      treatment_anti_contributions = sum(anti_sum_donations[treatment == 1], na.rm = TRUE),
      treatment_pro_contributions = sum(pro_sum_donations[treatment == 1], na.rm = TRUE),
      treatment_all_contributions = treatment_anti_contributions + treatment_pro_contributions,
      
      treatment_anti_share_pct = ifelse(total_anti_contributions > 0, 
                                        (treatment_anti_contributions / total_anti_contributions) * 100, 
                                        0),
      treatment_pro_share_pct = ifelse(total_pro_contributions > 0,
                                       (treatment_pro_contributions / total_pro_contributions) * 100,
                                       0),
      treatment_all_share_pct = ifelse(total_all_contributions > 0,
                                       (treatment_all_contributions / total_all_contributions) * 100,
                                       0),
      .groups = 'drop'
    )
  
  return(df_summary)
}


election_overlay_density <- function(df, outcome="pct_rep_change",
                                     title="Distribution of GOP Vote Share Change by Treatment",
                                     subtitle, caption="", xlabel="Republican Vote Share Change (%)",
                                     xlim=c(-50, 50), type="simple", # or scale or resid or resid_direct
                                     text_x=9
){
  
  if (type=="simple"){
    df$target_outcome <- as.numeric(df[[outcome]])  #df$XXX is not compatible when XXX is var.
  } else if (type=="scale"){
    df$target_outcome <- as.numeric(scale(df[[outcome]]))  
  } else if (type == "resid"){
    # have to address NA issue
    # excluding treatment
    string <- paste0(outcome, "~ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct|id+election_year_next")
    twfe <- fixest::feols(as.formula(string), data= df, cluster = "id")
    
    # first make results by NA to ensure the same length
    df$target_outcome <- NA  
    df$target_outcome[!is.na(resid(twfe))] <- as.numeric(resid(twfe))
    
    # this does not work due to NAs
    # df$target_outcome <- as.numeric(resid(twfe) )
    
  } else if (type == "resid_direct"){
    string <- paste0(outcome, "~ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+pro_log_sum+anti_log_sum|id+election_year_next")
    twfe <- fixest::feols(as.formula(string), data= df, cluster = "id")
    
    # first make results by NA to ensure the same length
    df$target_outcome <- NA  
    df$target_outcome[!is.na(resid(twfe))] <- as.numeric(resid(twfe))
    
    # this does not work due to NAs
    # df$target_outcome <- as.numeric(resid(twfe) )
    
  }
  
  
  plot <- ggplot(df, aes(x = target_outcome, 
                         fill = factor(treatment))) +
    geom_density(alpha = 0.7, color = "black") +
    geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')
  
  if (type=="simple"| type=="resid"){
    plot <- plot +
      geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
      geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')
  }
  
  plot <- plot+
    scale_fill_manual(values = c("1" = "skyblue","0" = "lightcoral"),
                      labels = c("1" = "Treatment (Fatal School Shooting) ", "0" = "Control"),
                      breaks = c("1", "0"),
                      name = "District Category") +
    labs(title = title,
         subtitle = subtitle,
         x = xlabel,
         y = "Density",
         caption = caption) +
    coord_cartesian(xlim = xlim) +
    theme_minimal() +
    annotate("text", x = text_x, y = Inf, label = "-5%", 
             vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
    annotate("text", x = -1*(text_x), y = Inf, label = "5%", 
             vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
    theme(
      plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size=10),
      legend.position = "top",
      plot.caption = element_text(hjust = 0, size = 10,  
                                  margin = margin(t = 15), lineheight = 1.2),
    )
  
}

election_pre_event <- function(df, outcome="pct_rep_change", past=-5, contribution=FALSE,
                               ylim=c(-5, 5), ylab= "Change in Rep. Voting Share"){
  
  if (contribution==FALSE){
    str <- paste(outcome, "~i(time_to_treatment_cycle, ref = -1, keep =", past, ":-1)+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)| id + election_year_next")
  } else if (contribution==TRUE){
    str <- paste(outcome, "~i(time_to_treatment_cycle, ref = -1, keep =", past, ":-1)+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum+anti_log_sum| id + election_year_next")
  }
  # time to event is a dummy variable for each period for event study; 0 means the month of an event
  # ref means baseline
  # can remove far away points by using keep
  fixest::feols(as.formula(str),data = df,cluster = "id") %>% 
    fixest::iplot(ref.line = -1, # use vline
                  xlab = "House Election Cycle Relative to Incidents",
                  ylab = ylab,
                  # main = "Simple Event Study (School Shooting; HoR)",
                  main = "",
                  xlim     = c(past, -1),
                  ylim=ylim
    )
  
}


med_DID <- function(df, outcome="pct_rep_change_iqr",
                    cluster="id", post_treatment=FALSE, interaction=TRUE){
  timeFE <- "election_year_next"
  
  if (post_treatment==FALSE & interaction ==TRUE){
    formula_str <- paste(outcome, "~ treatment* rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +", timeFE)
  } else if (post_treatment==TRUE &  interaction ==TRUE){
    formula_str <- paste(outcome, "~ treatment* rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)+pro_log_sum +anti_log_sum| id +", timeFE)
  } else  if (post_treatment==FALSE & interaction ==FALSE){
    formula_str <- paste(outcome, "~ treatment + rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +", timeFE)
  } else if (post_treatment==TRUE & interaction ==FALSE){
    formula_str <- paste(outcome, "~ treatment + rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)+pro_log_sum +anti_log_sum| id +", timeFE)
  } 
  
  results <- feols(as.formula(formula_str), data = df, cluster=cluster)
  return(results)
}




med_DID_PAC_dollar <- function(df, outcome="pct_rep_change", gap = TRUE,
                               interaction=FALSE, treatment=FALSE){
  
  if (gap ==TRUE & interaction == FALSE&treatment==TRUE){
    formula_str <- paste(outcome, "~ gap_loggap_sum +treatment+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id + election_year_next")
  } else if (gap ==TRUE & interaction == FALSE&treatment==FALSE){
    formula_str <- paste(outcome, "~ gap_loggap_sum + rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id + election_year_next")
  } else if (gap==TRUE & interaction==TRUE&treatment==TRUE){
    formula_str <- paste(outcome, "~ gap_loggap_sum*rep_incumbent_before+treatment+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } else if (gap==TRUE & interaction==TRUE&treatment==FALSE){
    formula_str <- paste(outcome, "~ gap_loggap_sum* rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } else if (gap==FALSE & interaction==FALSE&treatment==TRUE){
    formula_str <- paste(outcome, "~ pro_log_sum+anti_log_sum+treatment+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } else if (gap==FALSE & interaction==FALSE&treatment==FALSE){
    formula_str <- paste(outcome, "~ pro_log_sum+anti_log_sum+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } 
  
  results <- feols(as.formula(formula_str), data = df, cluster="id")
  return(results)
}


med_DID_PAC_binary <- function(df, outcome="pct_rep_change",
                               interaction=FALSE, treatment=FALSE){
  
  if (interaction == FALSE&treatment==TRUE){
    formula_str <- paste(outcome, "~ pro_PAC_year_20k +anti_PAC_year_20k+treatment+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id + election_year_next")
  } else if (interaction == FALSE&treatment==FALSE){
    formula_str <- paste(outcome, "~ pro_PAC_year_20k+anti_PAC_year_20k + rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id + election_year_next")
  }
  # else if (gap==TRUE & interaction==TRUE&treatment==TRUE){
  #   formula_str <- paste(outcome, "~ pro_PAC_year_20k*rep_incumbent_before+treatment+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  # } else if (gap==TRUE & interaction==TRUE&treatment==FALSE){
  #   formula_str <- paste(outcome, "~ pro_PAC_year_20k* rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  # } else if (gap==FALSE & interaction==FALSE&treatment==TRUE){
  #   formula_str <- paste(outcome, "~ pro_PAC_year_20k+anti_PAC_year_20k+treatment+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  # } else if (gap==FALSE & interaction==FALSE&treatment==FALSE){
  #   formula_str <- paste(outcome, "~ pro_PAC_year_20k+anti_PAC_year_20k+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  # } 
  
  results <- feols(as.formula(formula_str), data = df, cluster="id")
  return(results)
}



med_DID_PAC_number <- function(df, outcome="pct_rep_change", gap = TRUE,
                               interaction=FALSE, treatment=FALSE){
  
  if (gap ==TRUE & interaction == FALSE&treatment==TRUE){
    formula_str <- paste(outcome, "~ gap_don_number +treatment+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id + election_year_next")
  } else if (gap ==TRUE & interaction == FALSE&treatment==FALSE){
    formula_str <- paste(outcome, "~ gap_don_number + rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id + election_year_next")
  } else if (gap==TRUE & interaction==TRUE&treatment==TRUE){
    formula_str <- paste(outcome, "~ gap_don_number*rep_incumbent_before+treatment+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } else if (gap==TRUE & interaction==TRUE&treatment==FALSE){
    formula_str <- paste(outcome, "~ gap_don_number* rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } else if (gap==FALSE & interaction==FALSE&treatment==TRUE){
    formula_str <- paste(outcome, "~ pro_don_number+anti_don_number+treatment+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } else if (gap==FALSE & interaction==FALSE&treatment==FALSE){
    formula_str <- paste(outcome, "~ pro_don_number+anti_don_number+ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)| id +election_year_next")
  } 
  
  results <- feols(as.formula(formula_str), data = df, cluster="id")
  return(results)
}




# filtering by covariates
filter_by_q3_q1 <- function(data, column_name # use ""
) {
  
  q3_value <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  q1_value <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  
  filtered_data_q3 <- data[data[[column_name]] >= q3_value, ]
  filtered_data_q1 <- data[data[[column_name]] <= q1_value, ]
  
  return(list(filtered_data_q3 ,filtered_data_q1))
}




incumbency_margin_density <- function(df, title= "Incumbency Change Rate by Previous Election Margin",
                                      subtitle = "Unfiltered Data; HoR Elections",
                                      caption = "Vertical lines at ±5% indicate competitive district threshold",
                                      xlim=c(-50, 50)
                                      
                                      ){
  plot <- ggplot(df,
         aes(x = pct_RDmargin_before, 
             y = ..density..)) +   # density on y-axis
    geom_density(
      fill = "skyblue", 
      color = "white", 
      alpha = 0.6, 
      adjust = 1
    ) +
    labs( title = title, subtitle = subtitle,
          x = "Republicans' Prior Voting Margin (%)",
          y = "Density",
          caption = caption
    ) +
    geom_vline(xintercept = 0, linewidth = 0.5, linetype = "solid", color = 'grey30') +
    geom_vline(xintercept = 5, linewidth = 0.5, linetype = "dashed", color = 'red') +
    geom_vline(xintercept = -5, linewidth = 0.5, linetype = "dashed", color = 'red') +
    theme_minimal() +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    coord_cartesian(xlim = xlim) +
    
    annotate("text", x = -7, y = Inf, label = "-5%", 
             vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
    annotate("text", x = 7, y = Inf, label = "5%", 
             vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = ""
    )
  plot
}


incumbency_margin_hist <- function(df, title= "Incumbency Change Rate by Previous Election Margin",
                                   subtitle = "Unfiltered Data; HoR Elections",
                                   caption = "Vertical lines at ±5% indicate competitive district threshold",
                                   xlim=c(-50, 50), ylim = c(0, 35)
                                   
){

  ggplot(df, aes(x = pct_RDmargin_before, y = abs(incumbent_change) * 100)) +
  stat_summary_bin(
    fun = mean,    bins = 40,    geom = "col",
    fill = "skyblue",  color = "white"
  ) +
  labs( title = title,
        subtitle = subtitle,
        x = "Republican's Prior Voting Margin (%)",
        y = "Incumbency Change Rate (%)",
        caption = "Vertical lines at ±5% indicate competitive district threshold"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "solid", color = 'grey30') +
  geom_vline(xintercept = 5, linewidth = 0.5, linetype = "dashed", color = 'red') +
  geom_vline(xintercept = -5, linewidth = 0.5, linetype = "dashed", color = 'red') +
  theme_minimal()+
  scale_x_continuous(breaks = seq(-50, 50, by = 10)) +  # Adjust x-axis breaks
  # ylim(0, 30)+
  coord_cartesian(ylim = ylim, xlim=xlim) +  # Use coord_cartesian instead of ylim
  
  annotate("text", x = -7, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 7, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = ""
  )
}


election_pre_PAC <- function(df, outcome="pct_rep_change", past=-5, PACtype="anti", # "pro"
                               ylim=c(-10, 10), ylab="Change in Rep. Voting Share",
                             threshold="20k"){ # or "30k" or ""10k
  
  if (PACtype=="anti" & threshold=="20k"){
    str <- paste(outcome, "~i(time_to_antiPAC_cycle_20k, ref = -1, keep =", past,
                 ":-1)+ treatment+bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum| id + election_year_next")
  } else if (PACtype=="pro"& threshold=="20k"){
    str <- paste(outcome, "~i(time_to_proPAC_cycle_20k, ref = -1, keep =", past,
                 ":-1)+ treatment+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+anti_log_sum| id + election_year_next")
  } else if (PACtype=="anti" & threshold=="30k"){
    str <- paste(outcome, "~i(time_to_antiPAC_cycle_30k, ref = -1, keep =", past,
                 ":-1)+ treatment+bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum| id + election_year_next")
  } else if (PACtype=="pro"& threshold=="30k"){
    str <- paste(outcome, "~i(time_to_proPAC_cycle_30k, ref = -1, keep =", past,
                 ":-1)+ treatment+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+anti_log_sum| id + election_year_next")
  } else if (PACtype=="anti" & threshold=="10k"){
    str <- paste(outcome, "~i(time_to_antiPAC_cycle_10k, ref = -1, keep =", past,
                 ":-1)+ treatment+bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum| id + election_year_next")
  } else if (PACtype=="pro"& threshold=="10k"){
    str <- paste(outcome, "~i(time_to_proPAC_cycle_10k, ref = -1, keep =", past,
                 ":-1)+ treatment+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+anti_log_sum| id + election_year_next")
  }
  # time to event is a dummy variable for each period for event study; 0 means the month of an event
  # ref means baseline
  # can remove far away points by using keep
  fixest::feols(as.formula(str),data = df,cluster = "id") %>% 
    fixest::iplot(ref.line = -1, # use vline
                  xlab = "House Election Cycle Relative to Large-size Gun Policy Contributions",
                  ylab = ylab,
                  # main = "Simple Event Study (School Shooting; HoR)",
                  main = "",
                  xlim= c(past, -1),
                  ylim=ylim
    )
  
}


election_post_PAC <- function(df, outcome="pct_rep_change", post=5, PACtype="anti", # "pro"
                             ylim=c(-10, 10), ylab="Change in Rep. Voting Share",
                             threshold="20k"){ # or "30k" or ""10k
  
  # time to combined =0 means the month with a large-size gun policy PAC contribution
  if (PACtype=="anti" & threshold=="20k"){
    str <- paste(outcome, "~i(time_from_antiPAC_cycle_20k_combined, ref = -1, keep =", post,
                 ":-1)+ treatment+bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum| id + election_year_next")
  } else if (PACtype=="pro"& threshold=="20k"){
    str <- paste(outcome, "~i(time_from_proPAC_cycle_20k_combined, ref = -1, keep =", post,
                 ":-1)+ treatment+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+anti_log_sum| id + election_year_next")
  } else if (PACtype=="anti" & threshold=="30k"){
    str <- paste(outcome, "~i(time_from_antiPAC_cycle_30k_combined, ref = -1, keep =", post,
                 ":-1)+ treatment+bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum| id + election_year_next")
  } else if (PACtype=="pro"& threshold=="30k"){
    str <- paste(outcome, "~i(time_from_proPAC_cycle_30k_combined, ref = -1, keep =", post,
                 ":-1)+ treatment+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+anti_log_sum| id + election_year_next")
  } else if (PACtype=="anti" & threshold=="10k"){
    str <- paste(outcome, "~i(time_from_antiPAC_cycle_10k_combined, ref = -1, keep =", post,
                 ":-1)+ treatment+bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+pro_log_sum| id + election_year_next")
  } else if (PACtype=="pro"& threshold=="10k"){
    str <- paste(outcome, "~i(time_from_proPAC_cycle_10k_combined, ref = -1, keep =", post,
                 ":-1)+ treatment+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+abs(pct_RDmargin_before)+anti_log_sum| id + election_year_next")
  }
  fixest::feols(as.formula(str),data = df,cluster = "id") %>% 
    fixest::iplot(ref.line = -1, # use vline
                  xlab = "House Election Cycle Relative to Large-size Gun Policy Contributions",
                  ylab = ylab,
                  # main = "Simple Event Study (School Shooting; HoR)",
                  main = "",
                  xlim= c(-1, post),
                  ylim=ylim
    )
  
}



# 2. Semi-parametric ------------------------------------------------------


estimate_local_fe_for_y <- function(outcome, df) {
  
  # defined again within this function
  kernel_margin <- df$pct_RDmargin_before
  kernel_h   <- 1.06 * sd(kernel_margin, na.rm = TRUE) * sum(!is.na(kernel_margin))^(-1/5) * 1.25
  Ktri <- function(u) pmax(0, 1 - abs(u))
  
  estimate_at_margin <- function(margin) { 
    weight <- Ktri((df$pct_RDmargin_before - margin) / kernel_h) # kernel_h is the basis of distance and weight
    
    # minimum threshold is 30, and producing NA
    if (sum(weight > 0, na.rm = TRUE) < 30) { 
      return(tibble::tibble(
        outcome = outcome, margin = margin, n_eff = sum(weight > 0),
        term = c(
          "pro_log_sum","anti_log_sum",
          # "gap_gaplog_sum"
        ),
        estimate = NA_real_, se = NA_real_
      ))
    } # the end of if clause
    
    df$kernel_weight <- weight
    fit <- fixest::feols(
      as.formula(paste0(
        outcome," ~ pro_log_sum + anti_log_sum + ",
        # " ~ gap_gaplog_sum + ",
        "treatment+bachelor + black + white + unemployment + log_income + ",
        "rep_incumbent_before + gun_ratio_pct +abs(pct_RDmargin_before) | id + election_year_next"
      )),
      data = df, weights = ~ kernel_weight, vcov = ~ id + election_year_next # two-way clustering
    )
    
    get_coef <- function(fit, name) if (name %in% names(coef(fit))) coef(fit)[name] else NA_real_
    get_se   <- function(fit, name) if (name %in% names(se(fit)))   se(fit)[name]   else NA_real_
    
    coef <- c(get_coef(fit, "pro_log_sum"),  get_coef(fit, "anti_log_sum"))
    se <- c(get_se(fit,   "pro_log_sum"),  get_se(fit,   "anti_log_sum"))
    
    tibble::tibble(
      outcome = outcome, margin = margin, n_eff = sum(weight > 0),
      term = c(
        "pro_log_sum","anti_log_sum"
        # "gap_gaplog_sum"
      ),
      estimate = unname(coef), se = unname(se)
    )
  }
  map_dfr(grid_margin, estimate_at_margin)
}




estimate_local_oneside <- function(outcome, df) {
  
  # defined again within this function
  kernel_margin <- df$pct_RDmargin_before
  kernel_h   <- 1.06 * sd(kernel_margin, na.rm = TRUE) * sum(!is.na(kernel_margin))^(-1/5) * 1.25
  Ktri <- function(u) pmax(0, 1 - abs(u))
  
  estimate_at_margin <- function(margin) { 
    weight <- Ktri((df$pct_RDmargin_before - margin) / kernel_h) # kernel_h is the basis of distance and weight
    
    # minimum threshold is 30, and producing NA
    if (sum(weight > 0, na.rm = TRUE) < 30) { 
      return(tibble::tibble(
        outcome = outcome, margin = margin, n_eff = sum(weight > 0),
        term = c(
          "pro_log_sum"
          # ,"anti_log_sum",
          # "gap_gaplog_sum"
        ),
        estimate = NA_real_, se = NA_real_
      ))
    } # the end of if clause
    
    df$kernel_weight <- weight
    fit <- fixest::feols(
      as.formula(paste0(
        outcome," ~ pro_log_sum + anti_log_sum + ",
        # " ~ gap_gaplog_sum + ",
        "treatment+bachelor + black + white + unemployment + log_income + ",
        "rep_incumbent_before + gun_ratio_pct +abs(pct_RDmargin_before) | id + election_year_next"
      )),
      data = df, weights = ~ kernel_weight, vcov = ~ id + election_year_next # two-way clustering
    )
    
    get_coef <- function(fit, name) if (name %in% names(coef(fit))) coef(fit)[name] else NA_real_
    get_se   <- function(fit, name) if (name %in% names(se(fit)))   se(fit)[name]   else NA_real_
    
    coef <- c(get_coef(fit, "pro_log_sum")
              # ,  get_coef(fit, "anti_log_sum")
              )
    se <- c(get_se(fit, "pro_log_sum")
            # ,  get_se(fit,   "anti_log_sum")
            )
    
    tibble::tibble(
      outcome = outcome, margin = margin, n_eff = sum(weight > 0),
      term = c(
        "pro_log_sum"
        # ,"anti_log_sum"
        # "gap_gaplog_sum"
      ),
      estimate = unname(coef), se = unname(se)
    )
  }
  map_dfr(grid_margin, estimate_at_margin)
}
