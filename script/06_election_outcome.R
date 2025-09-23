# grouping to election-cycle-basis data
# Impact on election outcome

# 0. Data prep --------------------------------------------------------

# Since something will be adjusted, new datasets should be re-defined and renamed.
# apmed refers to anti, pro, mediation

apmed_data_s_raw <- data_s

# to make lag variable, we have to include data before 2000
apmed_data_s_1990 <- data_treatment_name(data_s, 1990)

apmed_data_s_2000 <- data_s_2000
apmed_data_s_2000_filtered <- data_s_2000_filtered
apmed_data_s_baseline <- data_s_margin_baseline

# also to include non-large-size contribution
apmed_data_s_smallbaseline <- data_flexible(data_s,
                                            voting_margin=5, minimum_dol=1)
apmed_data_s_smallbaseline1000 <- data_flexible(data_s,
                                            voting_margin=5, minimum_dol=1000)
apmed_data_s_2000_smallfilter1000 <- data_flexible(data_s,
                                                voting_margin=100, minimum_dol=1000)
apmed_data_s_2000_fatal <- data_flexible(data_s, voting_margin=100, minimum_dol=1)
apmed_data_s_2000_filtered_510 <- data_voting_filter(data_s, voting_min=5, voting_max=10)
apmed_data_s_2000_filtered_1020 <- data_voting_filter(data_s, voting_min=10, voting_max=20)
apmed_data_s_2000_filtered_m10m5 <- data_voting_filter(data_s, voting_min=-10, voting_max=-5)
apmed_data_s_2000_filtered_m20m10 <- data_voting_filter(data_s, voting_min=-20, voting_max=-10)


# Use correct_outliers_IQR function for trimming
# very similar function is used for log form versus absolute form.
# but this function covers zero, while the other one only focuses on positive values.

# For data wrangling, use med_data_prep1 function
# Note that, for simplicity, make treatment var. names "treatment", consistent with all election analysis datasets

# apmed_data_s_whole <- med_data_prep1(apmed_data_s_raw) 
# there is no incident_window_re in this dataset

apmed_data_s_2000 <- med_data_prep1(apmed_data_s_2000)
apmed_data_s_2000_filtered <- med_data_prep1(apmed_data_s_2000_filtered)
apmed_data_s_2000_fatal <- med_data_prep1(apmed_data_s_2000_fatal)
apmed_data_s_baseline <- med_data_prep1(apmed_data_s_baseline)

apmed_data_s_smallbaseline <- med_data_prep1(apmed_data_s_smallbaseline)
apmed_data_s_smallbaseline1000 <- med_data_prep1(apmed_data_s_smallbaseline1000)
apmed_data_s_2000_smallfilter1000 <- med_data_prep1(apmed_data_s_2000_smallfilter1000)

apmed_data_s_2000_filtered_510<- med_data_prep1(apmed_data_s_2000_filtered_510)
apmed_data_s_2000_filtered_1020<- med_data_prep1(apmed_data_s_2000_filtered_1020)
apmed_data_s_2000_filtered_m20m10<- med_data_prep1(apmed_data_s_2000_filtered_m20m10)
apmed_data_s_2000_filtered_m10m5<- med_data_prep1(apmed_data_s_2000_filtered_m10m5)

## 0.1 Def for stratification --------------------------------------------

# filtering: 2000filtered but rep incumbent
apmed_data_s_2000_filtered_rep <-  apmed_data_s_2000_filtered %>%
  filter(rep_incumbent_before==1)

# comp and rep incumbent
apmed_data_s_baseline_rep <-  apmed_data_s_baseline %>%
  filter(rep_incumbent_before==1)

# dem incumbent
apmed_data_s_2000_filtered_dem <-  apmed_data_s_2000_filtered %>%
  filter(rep_incumbent_before==0)

apmed_data_s_baseline_dem <-  apmed_data_s_baseline %>%
  filter(rep_incumbent_before==0)

# non-competitive districts
# be noted; different from all districts

apmed_data_s_2000_filtered_noncomp <-  apmed_data_s_2000_filtered %>%
  filter(competitive_factor==0)
apmed_data_s_2000_filtered_noncomp_rep <-  apmed_data_s_2000_filtered_noncomp %>%
  filter(rep_incumbent_before==1)
apmed_data_s_2000_filtered_noncomp_dem <-  apmed_data_s_2000_filtered_noncomp %>%
  filter(rep_incumbent_before==0)


apmed_data_s_2000_noncomp <-  apmed_data_s_2000 %>%
  filter(competitive_factor==0)
apmed_data_s_2000_noncomp_rep <-  apmed_data_s_2000_noncomp %>%
  filter(rep_incumbent_before==1)
apmed_data_s_2000_noncomp_dem <-  apmed_data_s_2000_noncomp %>%
  filter(rep_incumbent_before==0)

apmed_data_s_2000_fatal_noncomp <-  apmed_data_s_2000_fatal %>%
  filter(competitive_factor==0)

## 0.2. Grouping by election cycle ----------------------------------------------------------

# Group data into two year basis might be better

# rename first
apmed_data_s_2year_2000 <- apmed_data_s_2000
apmed_data_s_2year_2000_filtered <- apmed_data_s_2000_filtered
apmed_data_s_2year_2000_fatal <- apmed_data_s_2000_fatal
apmed_data_s_2year_baseline <- apmed_data_s_baseline

apmed_data_s_2year_smallbaseline <- apmed_data_s_smallbaseline 
apmed_data_s_2year_smallbaseline1000 <- apmed_data_s_smallbaseline1000
apmed_data_s_2year_2000_smallfilter1000 <- apmed_data_s_2000_smallfilter1000
apmed_data_s_2year_2000_fatal_noncomp <- apmed_data_s_2000_fatal_noncomp


apmed_data_s_2year_2000_filtered_510 <- apmed_data_s_2000_filtered_510
apmed_data_s_2year_2000_filtered_1020<- apmed_data_s_2000_filtered_1020
apmed_data_s_2year_2000_filtered_m20m10<- apmed_data_s_2000_filtered_m20m10
apmed_data_s_2year_2000_filtered_m10m5<- apmed_data_s_2000_filtered_m10m5


# Use med_data_group function

# incident can be binary; two cases happening in the same place and election cycle are so rare.
# it's ok to change these names later when using med_data_group function

# Also, note that dollar contributions are calculated based on mean for each month
# if we need restriction or minimum threshold, have to address this before using this function

apmed_data_s_2year_2000 <- med_data_group(apmed_data_s_2year_2000)
apmed_data_s_2year_2000_filtered <- med_data_group(apmed_data_s_2year_2000_filtered)
apmed_data_s_2year_2000_fatal <- med_data_group(apmed_data_s_2year_2000_fatal)
apmed_data_s_2year_baseline <- med_data_group(apmed_data_s_2year_baseline)

apmed_data_s_2year_smallbaseline <- med_data_group(apmed_data_s_2year_smallbaseline) 
apmed_data_s_2year_smallbaseline1000 <- med_data_group(apmed_data_s_2year_smallbaseline1000)
apmed_data_s_2year_2000_smallfilter1000 <- med_data_group(apmed_data_s_2year_2000_smallfilter1000)


apmed_data_s_2year_2000_filtered_rep <- med_data_group(apmed_data_s_2000_filtered_rep)
apmed_data_s_2year_2000_filtered_dem <- med_data_group(apmed_data_s_2000_filtered_dem)

apmed_data_s_2year_baseline_rep <- med_data_group(apmed_data_s_baseline_rep)
apmed_data_s_2year_baseline_dem <- med_data_group(apmed_data_s_baseline_dem)

apmed_data_s_2year_2000_filtered_noncomp <- med_data_group(apmed_data_s_2000_filtered_noncomp)
apmed_data_s_2year_2000_filtered_noncomp_rep <- med_data_group(apmed_data_s_2000_filtered_noncomp_rep)
apmed_data_s_2year_2000_filtered_noncomp_dem <- med_data_group(apmed_data_s_2000_filtered_noncomp_dem)

apmed_data_s_2year_2000_noncomp <- med_data_group(apmed_data_s_2000_noncomp)
apmed_data_s_2year_2000_noncomp_rep <- med_data_group(apmed_data_s_2000_noncomp_rep)
apmed_data_s_2year_2000_noncomp_dem <- med_data_group(apmed_data_s_2000_noncomp_dem)

apmed_data_s_2year_2000_fatal_noncomp <- med_data_group(apmed_data_s_2year_2000_fatal_noncomp)


apmed_data_s_2year_2000_filtered_510 <- med_data_group(apmed_data_s_2year_2000_filtered_510)
apmed_data_s_2year_2000_filtered_1020 <- med_data_group(apmed_data_s_2year_2000_filtered_1020)
apmed_data_s_2year_2000_filtered_m20m10 <- med_data_group(apmed_data_s_2year_2000_filtered_m20m10)
apmed_data_s_2year_2000_filtered_m10m5 <- med_data_group(apmed_data_s_2year_2000_filtered_m10m5)




# 1 revisit SS impact -------------------------------------------------

# portion of PAC contributions out of the total in the US

# first, use calculate_PAC_share function to calculate contribution share of a district out of all districts.
# Focusing on individual districts


# next, use calculate_treatment_summary function to calculate how many competitive districts account for in terms of number and contributions
# focusing on the entirety by election cycle
# we cannot combine these two functions

# just in case, I will also make a function for the entire data set, regardless of election cycle

calculate_treatment_entire <- function(df # it is ok to use grouped dataset
) {
  df_summary <- df %>%
    ungroup() %>% 
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
                                       0)
    )
  
  return(df_summary)
}

## 1.1  Individual District -------------------------------------------


# first, individual

# dataset
apmed_data_s_2year_baseline_PAC <- calculate_PAC_share(apmed_data_s_2year_baseline)
apmed_data_s_2year_2000_filtered_PAC <- calculate_PAC_share(apmed_data_s_2year_2000_filtered)

# check data

# anti
summary(apmed_data_s_2year_baseline_PAC$anti_share_pct)
summary(apmed_data_s_2year_2000_filtered_PAC$anti_share_pct)
hist(apmed_data_s_2year_baseline_PAC$anti_share_pct)

summary(apmed_data_s_2year_baseline_PAC$anti_share_pct_IQR)

# pro
summary(apmed_data_s_2year_baseline_PAC$pro_share_pct)
summary(apmed_data_s_2year_2000_filtered_PAC$pro_share_pct)
hist(apmed_data_s_2year_baseline_PAC$pro_share_pct)
summary(apmed_data_s_2year_baseline_PAC$pro_share_pct_IQR)

# check which is maximum
max_pro_district_whole <- apmed_data_s_2year_2000_filtered_PAC %>%
  filter(pro_share_pct == max(pro_share_pct, na.rm = TRUE))
print(max_pro_district_whole)

max_pro_district_comp <- apmed_data_s_2year_baseline_PAC %>%
  filter(pro_share_pct == max(pro_share_pct, na.rm = TRUE))
print(max_pro_district_comp)


## 1.2 Entire Districts  -------------------------------------------

# next, entirety, before election-cycle-basis

apmed_data_s_2year_baseline_entire <- calculate_treatment_entire(apmed_data_s_2year_baseline)
apmed_data_s_2year_2000_filtered_entire <- calculate_treatment_entire(apmed_data_s_2year_2000_filtered)

# nrow(apmed_data_s_2year_2000_filtered_entire)

apmed_data_s_2year_2000_filtered_entire$comp_district_share_pct
apmed_data_s_2year_2000_filtered_entire$treatment_district_share_pct
apmed_data_s_2year_2000_filtered_entire$treatment_anti_share_pct
apmed_data_s_2year_2000_filtered_entire$treatment_pro_share_pct
apmed_data_s_2year_2000_filtered_entire$treatment_all_share_pct

apmed_data_s_2year_baseline_entire$comp_district_share_pct
apmed_data_s_2year_baseline_entire$treatment_district_share_pct
apmed_data_s_2year_baseline_entire$treatment_anti_share_pct
apmed_data_s_2year_baseline_entire$treatment_pro_share_pct
apmed_data_s_2year_baseline_entire$treatment_all_share_pct

## 1.3 By election cycle -------------------------------------------

# lastly, by election cycle
apmed_data_s_2year_baseline_summary <- calculate_treatment_summary(apmed_data_s_2year_baseline)
apmed_data_s_2year_2000_filtered_summary <- calculate_treatment_summary(apmed_data_s_2year_2000_filtered)

apmed_data_s_2year_2000_filtered_summary$treatment_district_share_pct
apmed_data_s_2year_2000_filtered_summary$comp_district_share_pct

apmed_data_s_2year_baseline_summary$treatment_district_share_pct
apmed_data_s_2year_baseline_summary$comp_district_share_pct


### 1.3.1 Analysis --------------------------------------------------------




# fixest::feols(anti_share_pct_IQR ~ treatment + bachelor + black + white + unemployment +
#                 log_income + rep_incumbent_before + gun_ratio_pct
#               # +abs(pct_RDmargin_before) 
#               | id+election_year_next,
#               data = apmed_data_s_2year_baseline_PAC,
#               cluster = 'id')

apmed_data_s_2year_baseline_PAC2018 <- apmed_data_s_2year_baseline_PAC %>% 
  filter(election_year_next>=2018)
# 
# fixest::feols(anti_share_pct_IQR ~ treatment + bachelor + black + white + unemployment +
#                 log_income + rep_incumbent_before + gun_ratio_pct
#               # +abs(pct_RDmargin_before) 
#               | id+election_year_next,
#               data = apmed_data_s_2year_baseline_PAC2018,
#               cluster = 'id')


fixest::feols(anti_share_logpct ~ treatment + bachelor + black + white + unemployment +
                log_income + rep_incumbent_before + gun_ratio_pct
              # +abs(pct_RDmargin_before) 
              | id+election_year_next,
              data = apmed_data_s_2year_baseline_PAC,
              cluster = 'id')


fixest::feols(pro_share_pct_IQR ~ treatment + bachelor + black + white + unemployment +
                log_income + rep_incumbent_before + gun_ratio_pct| id+election_year_next,
              data = apmed_data_s_2year_baseline_PAC,
              cluster = 'id')

fixest::feols(pro_share_logpct ~ treatment + bachelor + black + white + unemployment +
                log_income + rep_incumbent_before + gun_ratio_pct| id+election_year_next,
              data = apmed_data_s_2year_baseline_PAC,
              cluster = 'id')

# 2 Election Descriptive ---------------------------------------------------------

## 2.1 pct rep before ----------------------------------------------------

# density plot, but one color
pctrep_whole_density <- ggplot(apmed_data_s_2year_2000_filtered, aes(x = pct_rep_change)) +
  geom_density(fill = "skyblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="solid", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="solid", color='red')+
  xlim(-50,50)+
  labs(title=NULL, substitle=NULL,
    # title = "Distribution of the Change in GOP Vote Share",
    #    subtitle = "Unfiltered Data; HoR",
       x = "Republican Vote Share Change (%)",
       y = "Density") +
  annotate("text", x = -13, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 13, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
pctrep_whole_density



### 2.1.1 Hetero stratification------------------------------------------------------------



# to filter outlier covariates, use filter_by_q3_q1 function
apmed_data_s_2year_2000_filtered_whiteq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "white")[[1]] 
apmed_data_s_2year_2000_filtered_whiteq1 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "white")[[2]] 

apmed_data_s_2year_2000_filtered_incomeq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "log_income")[[1]] 
apmed_data_s_2year_2000_filtered_incomeq1 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "log_income")[[2]] 

apmed_data_s_2year_2000_filtered_prosumq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "pro_sum_donations")[[1]] 
apmed_data_s_2year_2000_filtered_prosumq1 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "pro_sum_donations")[[2]] 

apmed_data_s_2year_2000_filtered_gunq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "gun_ratio_pct")[[1]] 
apmed_data_s_2year_2000_filtered_gunq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "gun_ratio_pct")[[1]] 






### 2.1.2 Prior gun lobby contribution ---------------------------------



pctrep_overlay_histogram_prosumq3 <- ggplot(apmed_data_s_2year_2000_filtered_prosumq3, aes(x = pct_rep_change, fill = factor(treatment))) +
  geom_histogram(
    breaks = seq(-50, 50, by = 2.5),
    alpha = 0.7, color = "black", position = "identity") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "Control", "1" = "Treatment"),
                    name = "Group") +
  labs(title = "Distribution of GOP Vote Share Change by Treatment",
       subtitle = "Disticts with large gun lobby contributions",
       x = "Republican Vote Share Change (%)",
       y = "Frequency",
       
       # caption should be included
       caption = "Note: districts with dollar amount of gun lobby contributions over 75th percentile.") +
  theme_minimal() +
  annotate("text", x = -9, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 9, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8,  
                                margin = margin(t = 15), lineheight = 1.2),
  )
pctrep_overlay_histogram_prosumq3

pctrep_overlay_histogram_prosumq1 <- ggplot(apmed_data_s_2year_2000_filtered_prosumq1, aes(x = pct_rep_change, fill = factor(treatment))) +
  geom_histogram(
    breaks = seq(-50, 50, by = 2.5),
    alpha = 0.7, color = "black", position = "identity") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "Control", "1" = "Treatment"),
                    name = "Group") +
  labs(title = "Distribution of GOP Vote Share Change by Treatment",
       subtitle = "Districts with small gun lobby contributions",
       x = "Republican Vote Share Change (%)",
       y = "Frequency",
       
       # caption should be included
       caption = "Note: districts with dollar amount of gun lobby contributions below 25th percentile.") +
  theme_minimal() +
  annotate("text", x = -9, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 9, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8,  
                                margin = margin(t = 15), lineheight = 1.2),
  )
pctrep_overlay_histogram_prosumq1



### 2.1.3 Prior gun safety contribution ---------------------------------

apmed_data_s_2year_2000_filtered_antisumq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "anti_sum_donations")[[1]] 
apmed_data_s_2year_2000_filtered_antisumq1 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "anti_sum_donations")[[2]] 


pctrep_overlay_histogram_antisumq3 <- ggplot(apmed_data_s_2year_2000_filtered_antisumq3, aes(x = pct_rep_change, fill = factor(treatment))) +
  geom_histogram(
    breaks = seq(-50, 50, by = 2.5),
    alpha = 0.7, color = "black", position = "identity") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "Control", "1" = "Treatment"),
                    name = "Group") +
  labs(title = "Distribution of GOP Vote Share Change by Treatment",
       subtitle = "Disticts with large GVP contributions",
       x = "Republican Vote Share Change (%)",
       y = "Frequency",
       
       # caption should be included
       caption = "Note: districts with dollar amount of GVP contributions over 75th percentile.") +
  theme_minimal() +
  annotate("text", x = -9, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 9, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8,  
                                margin = margin(t = 15), lineheight = 1.2),
  )
pctrep_overlay_histogram_antisumq3

pctrep_overlay_histogram_antisumq1 <- ggplot(apmed_data_s_2year_2000_filtered_antisumq1, aes(x = pct_rep_change, fill = factor(treatment))) +
  geom_histogram(
    breaks = seq(-50, 50, by = 2.5),
    alpha = 0.7, color = "black", position = "identity") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "Control", "1" = "Treatment"),
                    name = "Group") +
  labs(title = "Distribution of GOP Vote Share Change by Treatment",
       subtitle = "Districts with small GVP contributions",
       x = "Republican Vote Share Change (%)",
       y = "Frequency",
       
       # caption should be included
       caption = "Note: districts with dollar amount of GVP contributions below 25th percentile.") +
  theme_minimal() +
  annotate("text", x = -9, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 9, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8,  
                                margin = margin(t = 15), lineheight = 1.2),
  )
pctrep_overlay_histogram_antisumq1

### 2.1.4 Size of Voting Margin ---------------------------------

# 75 percentile means larger margin
apmed_data_s_2year_2000_filtered_marginq3 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "pct_RDmargin_before")[[1]] 
apmed_data_s_2year_2000_filtered_marginq1 <- filter_by_q3_q1(apmed_data_s_2year_2000_filtered, "pct_RDmargin_before")[[2]] 


pctrep_overlay_histogram_marginq3 <- ggplot(apmed_data_s_2year_2000_filtered_marginq3, aes(x = pct_rep_change, fill = factor(treatment))) +
  geom_histogram(
    breaks = seq(-50, 50, by = 2.5),
    alpha = 0.7, color = "black", position = "identity") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "Control", "1" = "Treatment"),
                    name = "Group") +
  labs(title = "Distribution of GOP Vote Share Change by Treatment",
       subtitle = "Disticts with larger voting margin",
       x = "Republican Vote Share Change (%)",
       y = "Frequency",
       
       # caption should be included
       caption = "Note: districts with voting margin in the House previous election over 75th percentile.") +
  theme_minimal() +
  annotate("text", x = -9, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 9, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8,  
                                margin = margin(t = 15), lineheight = 1.2),
  )
pctrep_overlay_histogram_marginq3

pctrep_overlay_histogram_marginq1 <- ggplot(apmed_data_s_2year_2000_filtered_marginq1, aes(x = pct_rep_change, fill = factor(treatment))) +
  geom_histogram(
    breaks = seq(-50, 50, by = 2.5),
    alpha = 0.7, color = "black", position = "identity") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="solid", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="dashed", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="dashed", color='red')+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "Control", "1" = "Treatment"),
                    name = "Group") +
  labs(title = "Distribution of GOP Vote Share Change by Treatment",
       subtitle = "Districts with small margin",
       x = "Republican Vote Share Change (%)",
       y = "Frequency",
       
       # caption should be included
       caption = "Note: districts with voting margin in the previous House election below 25th percentile.") +
  theme_minimal() +
  annotate("text", x = -9, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 9, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size= 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8,  
                                margin = margin(t = 15), lineheight = 1.2),
  )
pctrep_overlay_histogram_marginq1


### 2.2.1. Stratified density plot ----------------------------------------------------

# Use election_overlay_density function
# Three types; "simple", "scale" using scaled outcome, and "resid" for control by covariates 

# whole districts
pctrep_overlay_density_whole <- election_overlay_density(apmed_data_s_2year_2000_filtered,
                                                         title = NULL, subtitle = NULL,
                                                         # subtitle = "Unfiltered Data (Whole Districts)",
                                                         caption = "",
                                                         xlim=c(-50,50)) %>% print()


pctrep_overlay_density_whole_resid <- election_overlay_density(apmed_data_s_2year_2000_filtered,
                                                         title = NULL, subtitle = NULL,
                                                         # subtitle = "Unfiltered Data (Whole Districts)",
                                                         caption = "Controlled for Baseline Covariates and Log Dollar Amount of Gun Policy PAC Contributions.",
                                                         type = "resid_direct",
                                                         xlim=c(-50,50)) %>% print()


# non-competitive districts
# separate from whole districts
pctrep_overlay_density_noncomp <- election_overlay_density(apmed_data_s_2year_2000_filtered_noncomp,
                                                           title = NULL, subtitle = NULL,
                                                         # subtitle = "Unfiltered Data (Whole Districts)",
                                                         caption = "Non-competitive Districts with Prior Voting Margin > 5%", type="simple",
                                                         xlim=c(-50,50)) %>% print()

pctrep_overlay_density_noncomp_resid <- election_overlay_density(apmed_data_s_2year_2000_filtered_noncomp,
                                                           title = NULL, subtitle = NULL,
                                                           # subtitle = "Unfiltered Data (Whole Districts)",
                                                           caption = "", type="resid_direct",
                                                           xlim=c(-50,50)) %>% print()


# competitive districts

pctrep_overlay_density_comp <- election_overlay_density(apmed_data_s_2year_baseline, title = NULL, subtitle = NULL,
                                                        # subtitle = "Competitive Districts",
                                                        caption = "Competitive Districts with Prior Voting Margin <= 5%",
                                                        xlim=c(-50,50)) %>% print()



pctrep_overlay_density_comp_resid <- election_overlay_density(apmed_data_s_2year_baseline,title = NULL, subtitle = NULL,
                                                              # subtitle = "Competitive Districts; Controled for Covariates",
                                                              caption = "Competitive Districts with prior voting margin <= 5%; Basic covariates are used.",
                                                              xlim=c(-15,15), type="resid_direct", text_x = 6) %>% print()


pctrep_overlay_density_whole_residindirect <- election_overlay_density(apmed_data_s_2year_2000_filtered,
                                                               subtitle = "Unfiltered Data (Whole Districts); Controled for Covariates",
                                                               caption = "Basic Covariates are used.",
                                                               xlim=c(-50,50), type="resid") %>% print()




# whole rep incumbency
pctrep_overlay_density_2000_filtered_rep <- election_overlay_density(apmed_data_s_2year_2000_filtered_rep,title = NULL, subtitle = NULL,
                                                            # subtitle = "Districts with GOP incumbents",
                                                           caption = "Note: Districts with Rep. Incumbents.",
                                                            xlim=c(-50,50)) %>% print()

pctrep_overlay_density_2000_filtered_rep_resid <- election_overlay_density(apmed_data_s_2year_2000_filtered_rep,title = NULL, subtitle = NULL,
                                                                     # subtitle = "Districts with GOP incumbents",
                                                                     caption = "Note: Districts with Rep. Incumbents. Controlled for Baseline Covariates and Log Dollar Amount of Gun Policy PAC Contributions.",
                                                                     xlim=c(-50,50), type = "resid_direct") %>% print()

# DEM incumbent
pctrep_overlay_density_2000_filtered_dem <- election_overlay_density(apmed_data_s_2year_2000_filtered_dem,title = NULL, subtitle = NULL,
                                                                     # subtitle = "Districts with DEM incumbents",
                                                                     caption = "Note: Districts with Dem. Incumbents.",
                                                                     xlim=c(-50,50)) %>% print()

pctrep_overlay_density_2000_filtered_dem_resid <- election_overlay_density(apmed_data_s_2year_2000_filtered_dem,title = NULL, subtitle = NULL,
                                                                     # subtitle = "Districts with DEM incumbents",
                                                                     caption = "Note: Districts with Dem. Incumbents. Controlled for Baseline Covariates and Log Dollar Amount of Gun Policy PAC Contributions.",
                                                                     xlim=c(-50,50), type = "resid_direct") %>% print()


####  2.2.2 Further hetero Analysis -----------------------------------------------

# first, race

pctrep_overlay_density_diverse <- election_overlay_density(apmed_data_s_2year_2000_filtered_whiteq1,
                                                           title = NULL, subtitle = NULL,
                                                           # subtitle = "GVP; Districts with More Diverse Residents",
                                                           caption = "Districts with non-white resident proportion over 75th percentile",
                                                           xlim=c(-50,50)) %>% print()



# next, gun lobby contribution

pctrep_overlay_density_moregunlobby <- election_overlay_density(apmed_data_s_2year_2000_filtered_prosumq3,
                                                                title = NULL, subtitle = NULL,
                                                                # subtitle = "GVP; Districts with More Prior Gun Lobby Contributions",
                                                                caption = "Districts with prior large-size pro-gun PAC contributions over 75th percentile",
                                                                xlim=c(-50,50)) %>% print()


# then, less gunlobby
pctrep_overlay_density_lessgunlobby <- election_overlay_density(apmed_data_s_2year_2000_filtered_prosumq1,
                                                                subtitle = "GVP; Districts with Smaller Prior Gun Lobby Contributions",
                                                                caption = "Districts with prior gun lobby contributions below 25th percentile",
                                                                xlim=c(-50,50)) %>% print()

pctrep_overlay_density_lessgunlobby_resid <- election_overlay_density(apmed_data_s_2year_2000_filtered_prosumq1,
                                                                      subtitle = "GVP; Districts with Smaller Prior Gun Lobby Contributions; Controlled",
                                                                      caption = "Districts with prior gun lobby contributions below 25th percentile; Basic covariates are used.",
                                                                      xlim=c(-50,50), type = "resid") %>% print()

# districts with slim margin
# pctrep_overlay_density_marginq1 <- election_overlay_density(apmed_data_s_2year_2000_filtered_marginq1,
#                                                        subtitle = "Districts with small margin",
#        caption = "Note: districts with voting margin in the previous House election below 25th percentile.",
#        xlim=c(-50,50))
# pctrep_overlay_density_marginq1

# Next, Income

pctrep_overlay_density_poor <- election_overlay_density(apmed_data_s_2year_2000_filtered_incomeq1,
                                                        title = NULL, subtitle = NULL,
                                                                # subtitle = "GVP; Districts with Smaller Prior Gun Lobby Contributions",
                                                                caption = "Districts with Median Household Income below 25th percentile",
                                                                xlim=c(-50,50)) %>% print()


# Then, gun possession

pctrep_overlay_density_gun <- election_overlay_density(apmed_data_s_2year_2000_filtered_gunq3,
                                                        title = NULL, subtitle = NULL,
                                                        # subtitle = "GVP; Districts with Smaller Prior Gun Lobby Contributions",
                                                        caption = "Districts with Household Firearm Possession Rate over 75th percentile",
                                                        xlim=c(-50,50)) %>% print()



## 2.2A Margin change -----------------------------------------------------


# First, whole one-color plot

margin_whole_density <- ggplot(apmed_data_s_2year_2000_filtered, aes(x =pct_RDmargin_change)) +
  geom_density(fill = "skyblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color='grey30')+
  geom_vline(xintercept = 5, linewidth=0.5, linetype="solid", color='red')+
  geom_vline(xintercept = -5, linewidth=0.5, linetype="solid", color='red')+
  xlim(-70,70)+
  labs(title=NULL, substitle=NULL,
       # title = "Distribution of the Change in GOP Vote Share",
       #    subtitle = "Unfiltered Data; HoR",
       x = "Republicans' Vote Margin Change (%)",
       y = "Density") +
  annotate("text", x = -13, y = Inf, label = "-5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  annotate("text", x = 13, y = Inf, label = "5%", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
margin_whole_density



# whole districts
margin_overlay_density_whole <- election_overlay_density(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change",
                                                         # title="Distribution of GOP Margin Change by Treatment",
                                                         # subtitle = "Unfiltered Data (Whole Districts)",
                                                         title = NULL, subtitle = NULL,
                                                         xlabel="Republican Margin Change (Percent Point)",
                                                         caption = "",
                                                         xlim=c(-70,70)) %>% print()


margin_overlay_density_whole_resid <- election_overlay_density(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change",
                                                               title="Distribution of GOP Margin Change by Treatment",
                                                               subtitle = "Unfiltered Data; Controlled for Covariates",
                                                               xlabel="Republican Margin Change (percent point)",
                                                               caption = "Basic covariates are used.",
                                                               xlim=c(-50,50), type="resid") %>% print()

# competitive districts

margin_overlay_density_comp <- election_overlay_density(apmed_data_s_2year_baseline, outcome = "pct_RDmargin_change",
                                                        # title="Distribution of GOP Margin Change by Treatment",
                                                        # subtitle = "Baseline Filtering",
                                                        title = NULL, subtitle = NULL,
                                                        xlabel="Republican Margin Change (Percent Point)",
                                                        caption = "",
                                                        xlim=c(-70,70)) %>% print()



margin_overlay_density_comp_resid <- election_overlay_density(apmed_data_s_2year_baseline, outcome = "pct_RDmargin_change",
                                                              title="Distribution of GOP Margin Change by Treatment",
                                                              subtitle = "GVP; Basic Filtering; Controlled for Covariates",
                                                              xlabel="Republican Margin Change (percent point)",
                                                              caption = "Basic covariates are used.",
                                                              xlim=c(-15,15), type="resid")


margin_overlay_density_noncomp <- election_overlay_density(apmed_data_s_2year_2000_filtered_noncomp, outcome = "pct_RDmargin_change",
                                                        # title="Distribution of GOP Margin Change by Treatment",
                                                        # subtitle = "Baseline Filtering",
                                                        title = NULL, subtitle = NULL,
                                                        xlabel="Republican Margin Change (Percent Point)",
                                                        caption = "",
                                                        xlim=c(-70,70)) %>% print()


## 2.3 Incumbent change --------------------------------------------------


# Simple calculation of incumbency change

table(apmed_data_s_2year_2000_filtered$incumbent_change)
(221+222)/(221+222+5128)
# about 8%
# n(apmed_data_s_2year_2000_filtered$incumbent_change[incumbent_change==1])
# n(apmed_data_s_2year_2000_filtered$incumbent_change[incumbent_change==-1])
# n(apmed_data_s_2year_2000_filtered$incumbent_change)

# competitive districts
table(apmed_data_s_2year_baseline$incumbent_change)
(41+62)/(41+62+247)
# about 29%

# non-competitive
# defined before
# apmed_data_s_2year_2000_filtered_noncomp <- apmed_data_s_2year_2000_filtered %>% 
#   filter(abs(pct_RDmargin_before)>5)

table(apmed_data_s_2year_2000_filtered_noncomp$incumbent_change)
(180+160)/(180+160+4911)
# about 6%


### 2.3.1 plot of margin and incumbency change -----------------------------

# Use incumbency_margin_density function for density plot

incumbency_margin_density(apmed_data_s_2year_2000_filtered, title = NULL,subtitle = NULL)

# but in this context, histogram might be more intuitive
# Use incumbency_margin_hist function

incumbency_margin_hist(apmed_data_s_2year_2000_filtered, title = NULL,subtitle = NULL)



# 3 Baseline DID for SS --------------------------------------------------------

# For simplicity, integrate treatment var.s names



## 3.0 Pre-treatment trend -------------------------------------------------

# Use election_pre_event function


election_pre_event(apmed_data_s_2year_2000_filtered)
election_pre_event(apmed_data_s_2year_2000_filtered, contribution = TRUE)
election_pre_event(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change", ylim = c(-8,8), 
                   ylab = "Change in Rep. Voting Margin")
election_pre_event(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change", contribution = TRUE, ylim = c(-8,8), 
                   ylab = "Change in Rep. Voting Margin")
election_pre_event(apmed_data_s_2year_2000_filtered, outcome = "incumbent_change", ylim = c(-0.1,0.1), 
ylab = "Change in Rep. Seat")
election_pre_event(apmed_data_s_2year_2000_filtered, contribution = TRUE, outcome = "incumbent_change", ylim = c(-0.1,0.1), 
                   ylab = "Change in Rep. Seat")



## 3.1  Analaysis and Visualization ------------------------------------------------------

# use med_DID function

# first, be noted that incumbent_change is a binary, inconsistent with outlier.

# without interaction or post
med_2year_whole_pct_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change", post_treatment=FALSE, interaction=FALSE)
med_2year_whole_pctiq_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_iqr", post_treatment=FALSE, interaction=FALSE)
med_2year_whole_pctw_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_w", post_treatment=FALSE, interaction=FALSE)
med_2year_whole_margin_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change", post_treatment=FALSE, interaction=FALSE)
med_2year_whole_marginiq_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_iqr", post_treatment=FALSE, interaction=FALSE)
med_2year_whole_marginw_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_w", post_treatment=FALSE, interaction=FALSE)
med_2year_whole_inc_noint <- med_DID(apmed_data_s_2year_2000_fatal, "incumbent_change", post_treatment=FALSE, interaction=FALSE)

med_DID_2year_nopost_noint_whole_list <- list(
  "Vote Share Percentage" = med_2year_whole_pct_noint,
  "Vote Share Percentage (IQR)" = med_2year_whole_pctiq_noint,
  "Vote Share Percentage (WZ)" = med_2year_whole_pctw_noint,
  "GOP Margin" = med_2year_whole_margin_noint,
  "GOP Margin (IQR)" = med_2year_whole_marginiq_noint,
  "GOP Margin (WZ)" = med_2year_whole_marginw_noint,
  "Seat" = med_2year_whole_inc_noint)

med_2year_baseline_pct_noint <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change", post_treatment=FALSE, interaction=FALSE)
med_2year_baseline_pctiq_noint <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_iqr", post_treatment=FALSE, interaction=FALSE)
med_2year_baseline_pctw_noint <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w", post_treatment=FALSE, interaction=FALSE)
med_2year_baseline_margin_noint <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change", post_treatment=FALSE, interaction=FALSE)
med_2year_baseline_marginiq_noint <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_iqr", post_treatment=FALSE, interaction=FALSE)
med_2year_baseline_marginw_noint <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w", post_treatment=FALSE, interaction=FALSE)
med_2year_baseline_inc_noint <- med_DID(apmed_data_s_2year_baseline, "incumbent_change", post_treatment=FALSE, interaction=FALSE)

med_DID_2year_nopost_noint_baseline_list <- list(
  "Vote Share Percentage" = med_2year_baseline_pct_noint,
  "Vote Share Percentage (IQR)" = med_2year_baseline_pctiq_noint,
  "Vote Share Percentage (WZ)" = med_2year_baseline_pctw_noint,
  "GOP Margin" = med_2year_baseline_margin_noint,
  "GOP Margin (IQR)" = med_2year_baseline_marginiq_noint,
  "GOP Margin (WZ)" = med_2year_baseline_marginw_noint,
  "Seat" = med_2year_baseline_inc_noint)

# with post-treatment
med_2year_whole_pct_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change", post_treatment=TRUE, interaction=FALSE)
med_2year_whole_pctiq_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_iqr", post_treatment=TRUE, interaction=FALSE)
med_2year_whole_pctw_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_w", post_treatment=TRUE, interaction=FALSE)
med_2year_whole_margin_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change", post_treatment=TRUE, interaction=FALSE)
med_2year_whole_marginiq_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_iqr", post_treatment=TRUE, interaction=FALSE)
med_2year_whole_marginw_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_w", post_treatment=TRUE, interaction=FALSE)
med_2year_whole_inc_post_noint <- med_DID(apmed_data_s_2year_2000_fatal, "incumbent_change", post_treatment=TRUE, interaction=FALSE)

med_DID_2year_post_noint_whole_list <- list(
  "Vote Share Percentage" = med_2year_whole_pct_post_noint,
  "Vote Share Percentage (IQR)" = med_2year_whole_pctiq_post_noint,
  "Vote Share Percentage (WZ)" = med_2year_whole_pctw_post_noint,
  "GOP Margin" = med_2year_whole_margin_post_noint,
  "GOP Margin (IQR)" = med_2year_whole_marginiq_post_noint,
  "GOP Margin (WZ)" = med_2year_whole_marginw_post_noint,
  "Seat" = med_2year_whole_inc_post_noint)

med_2year_baseline_pct_post_noint <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change", post_treatment=TRUE, interaction=FALSE)
med_2year_baseline_pctiq_post_noint <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_iqr", post_treatment=TRUE, interaction=FALSE)
med_2year_baseline_pctw_post_noint <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w", post_treatment=TRUE, interaction=FALSE)
med_2year_baseline_margin_post_noint <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change", post_treatment=TRUE, interaction=FALSE)
med_2year_baseline_marginiq_post_noint <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_iqr", post_treatment=TRUE, interaction=FALSE)
med_2year_baseline_marginw_post_noint <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w", post_treatment=TRUE, interaction=FALSE)
med_2year_baseline_inc_post_noint <- med_DID(apmed_data_s_2year_baseline, "incumbent_change", post_treatment=TRUE, interaction=FALSE)

med_DID_2year_post_noint_baseline_list <- list(
  "Vote Share Percentage" = med_2year_baseline_pct_post_noint,
  "Vote Share Percentage (IQR)" = med_2year_baseline_pctiq_post_noint,
  "Vote Share Percentage (WZ)" = med_2year_baseline_pctw_post_noint,
  "GOP Margin" = med_2year_baseline_margin_post_noint,
  "GOP Margin (IQR)" = med_2year_baseline_marginiq_post_noint,
  "GOP Margin (WZ)" = med_2year_baseline_marginw_post_noint,
  "Seat" = med_2year_baseline_inc_post_noint)

 

# with interaction

med_2year_whole_pct <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change")
med_2year_whole_pctiq <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_iqr")
med_2year_whole_pctw <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_w")
med_2year_whole_margin <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change")
med_2year_whole_marginiq <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_iqr")
med_2year_whole_marginw <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_w")
med_2year_whole_inc <- med_DID(apmed_data_s_2year_2000_fatal, "incumbent_change")

med_DID_2year_nopost_int_whole_list <- list(
  "Vote Share Percentage" = med_2year_whole_pct,
  "Vote Share Percentage (IQR)" = med_2year_whole_pctiq,
  "Vote Share Percentage (WZ)" = med_2year_whole_pctw,
  "GOP Margin" = med_2year_whole_margin,
  "GOP Margin (IQR)" = med_2year_whole_marginiq,
  "GOP Margin (WZ)" = med_2year_whole_marginw,
  "Seat" = med_2year_whole_inc)

med_2year_baseline_pct <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change")
med_2year_baseline_pctiq <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_iqr")
med_2year_baseline_pctw <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w")
med_2year_baseline_margin <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change")
med_2year_baseline_marginiq <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_iqr")
med_2year_baseline_marginw <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w")
med_2year_baseline_inc <- med_DID(apmed_data_s_2year_baseline, "incumbent_change")

med_DID_2year_nopost_int_baseline_list <- list(
  "Vote Share Percentage" = med_2year_baseline_pct,
  "Vote Share Percentage (IQR)" = med_2year_baseline_pctiq,
  "Vote Share Percentage (WZ)" = med_2year_baseline_pctw,
  "GOP Margin" = med_2year_baseline_margin,
  "GOP Margin (IQR)" = med_2year_baseline_marginiq,
  "GOP Margin (WZ)" = med_2year_baseline_marginw,
  "Seat" = med_2year_baseline_inc)

# with interaction and contribution cov
med_2year_whole_pct_post <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change", post_treatment=TRUE)
med_2year_whole_pctiq_post <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_iqr", post_treatment=TRUE)
med_2year_whole_pctw_post <- med_DID(apmed_data_s_2year_2000_fatal, "pct_rep_change_w", post_treatment=TRUE)
med_2year_whole_margin_post <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change", post_treatment=TRUE)
med_2year_whole_marginiq_post <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_iqr", post_treatment=TRUE)
med_2year_whole_marginw_post <- med_DID(apmed_data_s_2year_2000_fatal, "pct_RDmargin_change_w", post_treatment=TRUE)
med_2year_whole_inc_post <- med_DID(apmed_data_s_2year_2000_fatal, "incumbent_change", post_treatment=TRUE)

med_DID_2year_post_int_whole_list <- list(
  "Vote Share Percentage" = med_2year_whole_pct_post,
  "Vote Share Percentage (IQR)" = med_2year_whole_pctiq_post,
  "Vote Share Percentage (WZ)" = med_2year_whole_pctw_post,
  "GOP Margin" = med_2year_whole_margin_post,
  "GOP Margin (IQR)" = med_2year_whole_marginiq_post,
  "GOP Margin (WZ)" = med_2year_whole_marginw_post,
  "Seat" = med_2year_whole_inc_post)

med_2year_baseline_pct_post <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change", post_treatment=TRUE)
med_2year_baseline_pctiq_post <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_iqr", post_treatment=TRUE)
med_2year_baseline_pctw_post <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w", post_treatment=TRUE)
med_2year_baseline_margin_post <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change", post_treatment=TRUE)
med_2year_baseline_marginiq_post <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_iqr", post_treatment=TRUE)
med_2year_baseline_marginw_post <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w", post_treatment=TRUE)
med_2year_baseline_inc_post <- med_DID(apmed_data_s_2year_baseline, "incumbent_change", post_treatment=TRUE)

med_DID_2year_post_int_baseline_list <- list(
  "Vote Share Percentage" = med_2year_baseline_pct_post,
  "Vote Share Percentage (IQR)" = med_2year_baseline_pctiq_post,
  "Vote Share Percentage (WZ)" = med_2year_baseline_pctw_post,
  "GOP Margin" = med_2year_baseline_margin_post,
  "GOP Margin (IQR)" = med_2year_baseline_marginiq_post,
  "GOP Margin (WZ)" = med_2year_baseline_marginw_post,
  "Seat" = med_2year_baseline_inc_post)

# 
# med_DID_2year_nopost_list <- list(
#   "Vote Share Percentage: Whole Districts" = med_2year_whole_pct,
#   "Vote Share Percentage: Competitive" = med_2year_baseline_pct ,
#   "GOP Margin: Whole Districts" = med_2year_whole_margin,
#   "GOP Margin: Competitive" = med_2year_baseline_margin,
#   "Seat: Whole Districts" = med_2year_whole_inc,
#   "Seat: Competitive" = med_2year_baseline_inc)
# 
# med_DID_2year_nopost_list <- list(
#   "Vote Share Percentage: Whole Districts" = med_2year_whole_pct,
#   "Vote Share Percentage: Competitive" = med_2year_baseline_pct ,
#   "GOP Margin: Whole Districts" = med_2year_whole_margin,
#   "GOP Margin: Competitive" = med_2year_baseline_margin,
#   "Seat: Whole Districts" = med_2year_whole_inc,
#   "Seat: Competitive" = med_2year_baseline_inc)
# 
# med_DID_2year_post_list <- list(
#   "GOP Vote Share Percentage: Whole Districts" = med_2year_whole_pct_post,
#   "GOP Vote Share Percentage: Competitive" = med_2year_baseline_pct_post ,
#   "GOP Margin: Whole Districts" = med_2year_whole_margin_post,
#   "GOP Margin: Competitive" = med_2year_baseline_margin_post,
#   "Seat: Whole Districts" = med_2year_whole_inc_post,
#   "Seat: Competitive" = med_2year_baseline_inc_post
# ) 
# 
# med_DID_2year_post_noint_list <- list(
#   "Vote Share Percentage: Whole Districts" = med_2year_whole_pct_post_noint,
#   "Vote Share Percentage: Competitive" = med_2year_baseline_pct_post_noint ,
#   "GOP Margin: Whole Districts" = med_2year_whole_margin_post_noint,
#   "GOP Margin: Competitive" = med_2year_baseline_margin_post_noint,
#   "Seat: Whole Districts" = med_2year_whole_inc_post_noint,
#   "Seat: Competitive" = med_2year_baseline_inc_post_noint
# ) 

# med_DID_modellist <- list(
#   "Vote Percentage: Month" = med_month_pct,
#   "Margin: Month" = med_month_margin,
#   "Seat: Month" = med_month_inc,
#   "Vote Percentage: Year" = med_2year_pct,
#   "Margin: Year" = med_2year_margin,
#   "Seat: Year" = med_2year_inc) 

# Variable
# Incumbency matters, so include its interaction
coef_map_med <- c(
  "treatment" = "Treatment (Fatal School Shooting)"
  # "rep_incumbent_before" = "GOP Incumbency in the Previous Election"
  # "rep_incumbent_before" = "Incumbency",
  # "treatment:rep_incumbent_before" = "Interaction"
)

coef_map_med_post <- c(
  "treatment" = "Treatment (Fatal School Shooting)",
  # "rep_incumbent_before" = "GOP Incumbency in the Previous Election",
  "pro_log_sum" = "Gun Lobby Contribution (log $)",
  "anti_log_sum" = "GVP Contribution (log $)",
  "rep_incumbent_before" = "Incumbency",
  "treatment:rep_incumbent_before" = "Interaction"
)

# add rows
gof_add_med_2year <- data.frame(
  raw = c("Outcome (Change in ...)", "Outlier Correction"
          # , "Opposite Contribution Covariate"
  ),
  mod1 = c("Vote Share of Rep. candidates", "No"),
  mod2 = c("Vote Share of Rep. candidates", "IQR"),
  mod3 = c("Vote Share of Rep. candidates", "Winsorizing"),
  mod4 = c("Rep. Margin against Dem.", "No"),
  mod5 = c("Rep. Margin against Dem.", "IQR"),
  mod6 = c("Rep. Margin against Dem.", "Winsorizing"),
  mod7 = c("Rep. Seat (1 = Win, 0 = Lose)", "No"),
  # mod6 = c("Seat (1 = Win, 0 = Lose)", "Competitive"),
  # mod5 = c("% of Rep. candidates", "All", "Yes"),
  # mod6 = c("% of Rep. candidates", "Competitive", "Yes"),
  # mod7 = c("Margin with Dem.", "All", "Yes"),
  # mod8 = c("Margin with Dem.", "Competitive", "Yes"),
  stringsAsFactors = FALSE
)

med_sourcenote_SS <- 
  "Baseline covariates plus absolute value of prior voting margin. "
# or winsorized at the 5% level


# med_2year_pct <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w", "election_year_next")
# med_2year_margin <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w", "election_year_next")
# med_2year_inc <- med_DID(apmed_data_s_2year_baseline, "incumbent_change_w", "election_year_next")


med_DID_2year_nopost_noint_whole_plot <- gt_table(med_DID_2year_nopost_noint_whole_list,
                                                  coef_map_med_post,  c("nobs"),
                                      gof_add_med_2year,
                                      "Impact of Fatal School Shootings on the Next Election Results; All Districts",
                                      "No interaction between treatment and  Rep. Incumbency; No PAC Contribution as Covariates", 
                                      source_note =med_sourcenote_SS)
# med_DID_2year_nopost_noint_whole_plot


med_DID_2year_nopost_noint_baseline_plot <- gt_table(med_DID_2year_nopost_noint_baseline_list,
                                                  coef_map_med,  c("nobs"),
                                                  gof_add_med_2year,
                                                  "Impact of Fatal School Shootings on the Next Election Results; Competitive Districts",
                                                  "No interaction between treatment and Rep. Incumbency; No PAC Contribution as Covariates", 
                                                  source_note =med_sourcenote_SS)
# med_DID_2year_nopost_noint_baseline_plot


med_DID_2year_post_noint_whole_plot <- gt_table(med_DID_2year_post_noint_whole_list,
                                                  coef_map_med,  c("nobs"),
                                                  gof_add_med_2year,
                                                  "Impact of Fatal School Shootings on the Next Election Results; All Districts",
                                                  "Including PAC Contribution as Covariates; No interaction between treatment and Rep. Incumbency", 
                                                  source_note =med_sourcenote_SS)
med_DID_2year_post_noint_whole_plot


med_DID_2year_post_noint_baseline_plot <- gt_table(med_DID_2year_post_noint_baseline_list,
                                                coef_map_med,  c("nobs"),
                                                gof_add_med_2year,
                                                "Impact of Fatal School Shootings on the Next Election Results; Competitive Districts",
                                                "Including PAC Contribution as Covariates; No interaction between treatment and Rep. Incumbency", 
                                                source_note =med_sourcenote_SS)
med_DID_2year_post_noint_baseline_plot

med_DID_2year_nopost_int_whole_plot <- gt_table(med_DID_2year_nopost_int_whole_list,
                                                coef_map_med,  c("nobs"),
                                                gof_add_med_2year,
                                                "Impact of Fatal School Shootings on the Next Election Results; All Districts",
                                                "Including interaction between treatment and Rep. Incumbency; No PAC Contribution as Covariates", 
                                                source_note =med_sourcenote_SS)
med_DID_2year_nopost_int_whole_plot

med_DID_2year_nopost_int_baseline_plot <- gt_table(med_DID_2year_nopost_int_baseline_list,
                                                coef_map_med,  c("nobs"),
                                                gof_add_med_2year,
                                                "Impact of Fatal School Shootings on the Next Election Results; Competitive Districts",
                                                "Including interaction between treatment and Rep. Incumbency; No PAC Contribution as Covariates", 
                                                source_note =med_sourcenote_SS)
med_DID_2year_nopost_int_baseline_plot


med_DID_2year_post_int_whole_plot <- gt_table(med_DID_2year_post_int_whole_list,
                                                coef_map_med,  c("nobs"),
                                                gof_add_med_2year,
                                                "Impact of Fatal School Shootings on the Next Election Results; All Districts",
                                                "Including interaction between treatment and Rep. Incumbency; PAC Contribution as Covariates", 
                                                source_note =med_sourcenote_SS)
med_DID_2year_post_int_whole_plot

med_DID_2year_post_int_baseline_plot <- gt_table(med_DID_2year_post_int_baseline_list,
                                              coef_map_med,  c("nobs"),
                                              gof_add_med_2year,
                                              "Impact of Fatal School Shootings on the Next Election Results; Competitive Districts",
                                              "Including interaction between treatment and Rep. Incumbency; PAC Contribution as Covariates", 
                                              source_note =med_sourcenote_SS)
med_DID_2year_post_int_baseline_plot

# med_DID_2year_post_plot <- gt_table(med_DID_2year_post_list,
#                                     # coef_map_med_post, # if you want to show estimates for PAC contributions
#                                     coef_map_med, # simple ver.
#                                     c("nobs"),
#                                     gof_add_med_2year,
#                                     "Impact of Fatal School Shootings on the Next Election Results",
#                                     "PAC Contribution as Cov. (with interactions); Data Aggregared by Election Cycle, IQR Cap for Percentage Outcomes", 
#                                     source_note =med_sourcenote_SS)
# 
# med_DID_2year_post_noint_plot <- gt_table(med_DID_2year_post_noint_list,coef_map_med,  c("nobs"),
#                                           gof_add_med_2year,
#                                           "Impact of Fatal School Shootings on the Next Election Results",
#                                           "PAC Contributions as Cov. (No interaction); Data Aggregared by Election Cycle, IQR Cap for Percentage Outcomes", 
#                                           source_note =med_sourcenote_SS)




## 3.2 Winsorzing ------------------------------------------------------

med_2year_whole_pct_win <- med_DID(apmed_data_s_2year_2000_filtered, "pct_rep_change_w")
med_2year_baseline_pct_win <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w")
med_2year_whole_margin_win <- med_DID(apmed_data_s_2year_2000_filtered, "pct_RDmargin_change_w")
med_2year_baseline_margin_win <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w")
med_2year_whole_pct_post_win <- med_DID(apmed_data_s_2year_2000_filtered, "pct_rep_change_w", post_treatment=TRUE)
med_2year_baseline_pct_post_win <- med_DID(apmed_data_s_2year_baseline, "pct_rep_change_w", post_treatment=TRUE)
med_2year_whole_margin_post_win <- med_DID(apmed_data_s_2year_2000_filtered, "pct_RDmargin_change_w", post_treatment=TRUE)
med_2year_baseline_margin_post_win <- med_DID(apmed_data_s_2year_baseline, "pct_RDmargin_change_w", post_treatment=TRUE)


med_DID_2year_win_list <- list(
  "Vote Share Percentage: Unfiltered" = med_2year_whole_pct_win,
  "Vote Share Percentage: Competitive" = med_2year_baseline_pct_win ,
  "Margin: Unfiltered" = med_2year_whole_margin_win,
  "Margin: Competitive" = med_2year_baseline_margin_win,
  "Vote Share Percentage: Unfiltered; PAC Contribution" = med_2year_whole_pct_post_win,
  "Vote Share Percentage: Competitive; PAC Contribution" = med_2year_baseline_pct_post_win ,
  "Margin: Unfiltered; PAC Contribution" = med_2year_whole_margin_post_win,
  "Margin: Competitive; PAC Contribution" = med_2year_baseline_margin_post_win
) 
# Incumbency matters, so include its interaction
coef_map_med <- c(
  "treatment" = "Treatment"
  # "rep_incumbent_before" = "Incumbency in the previous election",
  # # "rep_incumbent_before" = "Incumbency",
  # "treatment:rep_incumbent_before" = "Interaction"
)

# add rows
gof_add_med_win <- data.frame(
  raw = c("Outcome (Change in ...)", "Scope of Districts"
          , "Opposite Contribution Covariates"
  ),
  mod1 = c("% of Rep. candidates", "All", "No"),
  mod2 = c("% of Rep. candidates", "Competitive", "No"),
  mod3 = c("Margin with Dem.", "All", "No"),
  mod4 = c("Margin with Dem.", "Competitive", "No"),
  # mod5 = c("Seat (1 = Win, 0 = Lose)", "All"),
  # mod6 = c("Seat (1 = Win, 0 = Lose)", "Competitive"),
  mod5 = c("% of Rep. candidates", "All", "Yes"),
  mod6 = c("% of Rep. candidates", "Competitive", "Yes"),
  mod7 = c("Margin with Dem.", "All", "Yes"),
  mod8 = c("Margin with Dem.", "Competitive", "Yes"),
  stringsAsFactors = FALSE
)

med_sourcenote_win <- 
  "Baseline covariates; In each model, percentage outcomes were winsorized at the 5% level."


med_DID_2year_win_plot <- gt_table(med_DID_2year_win_list,coef_map_med,  c("nobs"),
                                   gof_add_med_win,
                                   "Impact of Fatal School Shootings on the Next Election Results",
                                   "Data Aggregared by Election Cycle, Winsorzing", 
                                   source_note =med_sourcenote_win)

# med_DID_2year_win_plot



# 4 PAC DID -----------------------------------------------------------


## 4.0 Pre-treatment trend -------------------------------------------------

# Use election_pre_PAC function


election_pre_PAC(apmed_data_s_2year_2000_filtered, ylim = c(-10,10))
election_pre_PAC(apmed_data_s_2year_2000_filtered, PACtype="pro", ylim = c(-10,10))
election_pre_PAC(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change", ylim = c(-18,18),
                 ylab = "Change in Rep. Voting Margin")
election_pre_PAC(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change", PACtype = "pro", ylim = c(-18,18),
                 ylab = "Change in Rep. Voting Margin")
election_pre_PAC(apmed_data_s_2year_2000_filtered, outcome = "incumbent_change", ylim = c(-0.25,0.25),
                 ylab = "Change in Rep. Seat")
election_pre_PAC(apmed_data_s_2year_2000_filtered, PACtype = "pro", outcome = "incumbent_change", ylim = c(-0.25,0.25),
                 ylab = "Change in Rep. Seat")

election_pre_PAC(apmed_data_s_2year_2000_filtered, ylim = c(-10,10), threshold = "30k")
election_pre_PAC(apmed_data_s_2year_2000_filtered, PACtype="pro", 
                 ylim = c(-10,10), threshold = "30k")
election_pre_PAC(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change", ylim = c(-18,18),
                 ylab = "Change in Rep. Voting Margin", threshold = "30k")
election_pre_PAC(apmed_data_s_2year_2000_filtered, outcome = "pct_RDmargin_change", PACtype = "pro", ylim = c(-18,18),
                 ylab = "Change in Rep. Voting Margin", threshold = "30k")
election_pre_PAC(apmed_data_s_2year_2000_filtered, outcome = "incumbent_change", ylim = c(-0.35,0.35),
                 ylab = "Change in Rep. Seat", threshold = "30k")
election_pre_PAC(apmed_data_s_2year_2000_filtered, PACtype = "pro", outcome = "incumbent_change", ylim = c(-0.35,0.35),
                 ylab = "Change in Rep. Seat", threshold = "30k")


## 4.1 Vote Share -------------------------------------------------

# Use med_DID_PAC_dollar function for DID

# competitive districts
medPAC_DID_comp <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome="pct_rep_change")
medPAC_DID_comp_treat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                          treatment = TRUE, outcome="pct_rep_change")
medPAC_DID_comp_int <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                            interaction = TRUE, outcome="pct_rep_change")
medPAC_DID_comp_nogap <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, gap=FALSE,
                                            outcome="pct_rep_change")

medPAC_DID_comp_iq <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome="pct_rep_change_iqr")
medPAC_DID_comp_treat_iq <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                          treatment = TRUE, outcome="pct_rep_change_iqr")
medPAC_DID_comp_int_iq <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                               interaction = TRUE, outcome="pct_rep_change_iqr")
medPAC_DID_comp_nogap_iq <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, gap=FALSE,
                                            outcome="pct_rep_change_iqr")

# compact
PACDID_comp_list1 <- list(
  "Baseline"= medPAC_DID_comp,
  "IQR" = medPAC_DID_comp_iq,
  "Covariate of School Shooting"= medPAC_DID_comp_treat,
  "Covariate of School Shooting; IQR"=medPAC_DID_comp_treat_iq
)

# for appendix; more detailed
PACDID_comp_list2 <- list(
  "Baseline"= medPAC_DID_comp,
  "IQR" = medPAC_DID_comp_iq,
  "Covariate of School Shooting"= medPAC_DID_comp_treat,
  "Covariate of School Shooting; IQR"=medPAC_DID_comp_treat_iq,
  "Interaction with Incumbency"= medPAC_DID_comp_int,
  "Interaction with Incumbency; IQR"=medPAC_DID_comp_int_iq,
  "Each PAC" = medPAC_DID_comp_nogap,
  "Each PAC; IQR"=medPAC_DID_comp_nogap_iq
)

coef_map_medPAC <- c(
  "gap_loggap_sum"="Contribution Gap (Pro-Gun minus Gun Control; log)",
  "pro_log_sum"="Pro-Gun Contribution (log)",
  "anti_log_sum" = "Gun Control Contribution (log)"
  # "treatment"="School Shooting"
  # "rep_incumbent_before" = "Incumbency in the previous election",
  # # "rep_incumbent_before" = "Incumbency",
  # "treatment:rep_incumbent_before" = "Interaction"
)

gof_add_medPAC1 <- data.frame(
  raw = c("Treatment Variable",  "Covariate of Fatal School Shootings", "Outlier Correction" ),
  mod1 = c("Gap Between Pro-gun and Gun Control contributions", "No", "No"),
  mod2 = c("Gap Between Pro-gun and Gun Control contributions", "No", "IQR"),
  mod3 = c("Gap Between Pro-gun and Gun Control contributions", "Yes", "No"),
  mod4 = c("Gap Between Pro-gun and Gun Control contributions", "Yes", "IQR"),
   
  stringsAsFactors = FALSE
)

gof_add_medPAC2 <- data.frame(
  raw = c("Treatment Variable",  "Covariate of Fatal School Shootings", "Interaction with Rep. Incumbency", "Outlier Correction" ),
  mod1 = c("Gap Between Pro-gun and Gun Control contributions", "No", "No","No"),
  mod2 = c("Gap Between Pro-gun and Gun Control contributions", "No", "No","IQR"),
  mod3 = c("Gap Between Pro-gun and Gun Control contributions", "Yes", "No","No"),
  mod4 = c("Gap Between Pro-gun and Gun Control contributions", "Yes", "No","IQR"),
  mod5 = c("Gap Between Pro-gun and Gun Control contributions","No", "Yes", "No"),
  mod6 = c("Gap Between Pro-gun and Gun Control contributions","No", "Yes", "IQR"),
  mod7 = c("Each of Pro-gun and Gun Control contributions", "No", "No","No"),
  mod8 = c("Each of Pro-gun and Gun Control contributions", "No", "No","IQR"),
  
  stringsAsFactors = FALSE
)


med_DID_2year_PAC_comp_plot1 <- gt_table(PACDID_comp_list1, coef_map_medPAC , c("nobs"),
                                  gof_add_medPAC1,
                                  "Impact of Gun Policy PAC Contributions on the Next Election Results; Competitive Districts",
                                  "Rep. Vote Share Change as Outcome", 
                                  source_note =med_sourcenote_SS) %>% print()

med_DID_2year_PAC_comp_plot2 <- gt_table(PACDID_comp_list2, coef_map_medPAC , c("nobs"),
                                         gof_add_medPAC2,
                                         "Impact of Gun Policy PAC Contributions on the Next Election Results; Competitive Districts",
                                         "Rep. Vote Share Change as Outcome", 
                                         source_note =med_sourcenote_SS) %>% print()





## 4.2 Vote margin ------------------------------------------------------

# competitive districts
medPAC_DID_comp_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome="pct_RDmargin_change")
medPAC_DID_comp_treat_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                          treatment = TRUE, outcome="pct_RDmargin_change")
medPAC_DID_comp_int_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                                   interaction = TRUE, outcome="pct_RDmargin_change")
medPAC_DID_comp_nogap_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, gap=FALSE,
                                            outcome="pct_RDmargin_change")

medPAC_DID_comp_iq_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome="pct_RDmargin_change_iqr")
medPAC_DID_comp_treat_iq_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                             treatment = TRUE, outcome="pct_RDmargin_change_iqr")
medPAC_DID_comp_int_iq_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, 
                                                      interaction = TRUE, outcome="pct_RDmargin_change_iqr")
medPAC_DID_comp_nogap_iq_margin <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, gap=FALSE,
                                               outcome="pct_RDmargin_change_iqr")

# for main table
PACDID_comp_margin_list1 <- list(
  "Baseline"= medPAC_DID_comp_margin,
  "IQR" = medPAC_DID_comp_iq_margin,
  "School Shooting Covariate"= medPAC_DID_comp_treat_margin,
  "School Shooting Covariate; IQR"=medPAC_DID_comp_treat_iq_margin
  )

# for appendix
PACDID_comp_margin_list2 <- list(
  "Baseline"= medPAC_DID_comp_margin,
  "IQR" = medPAC_DID_comp_iq_margin,
  "School Shooting Covariate"= medPAC_DID_comp_treat_margin,
  "School Shooting Covariate; IQR"=medPAC_DID_comp_treat_iq_margin,
  "Interaction with Rep. Incumbency"= medPAC_DID_comp_int_margin,
  "Interaction with Rep. Incumbency; IQR"=medPAC_DID_comp_int_iq_margin,
  "Each PAC" = medPAC_DID_comp_nogap_margin,
  "Each PAC; IQR"=medPAC_DID_comp_nogap_iq_margin
)


med_DID_2year_PAC_comp_margin_plot1 <- gt_table(PACDID_comp_margin_list1, coef_map_medPAC , c("nobs"),
                                        gof_add_medPAC1,
                                        "Impact of Gun Policy PAC Contributions on the Next Election Results; Competitive Districts",
                                        "Rep. Voting Margin Change as Outcome", 
                                        source_note =med_sourcenote_SS) %>% print()


med_DID_2year_PAC_comp_margin_plot2 <- gt_table(PACDID_comp_margin_list2, coef_map_medPAC , c("nobs"),
                                                gof_add_medPAC2,
                                                "Impact of Gun Policy PAC Contributions on the Next Election Results; Competitive Districts",
                                                "Rep. Voting Margin Change as Outcome", 
                                                source_note =med_sourcenote_SS) %>% print()


## 4.3 Seat change ---------------------------------------------------------

# competitive districts
medPAC_DID_comp_seat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome="incumbent_change")
medPAC_DID_comp_treat_seat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline,
                                                 treatment = TRUE, outcome="incumbent_change")
medPAC_DID_comp_int_seat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline,
                                                 interaction = TRUE, outcome="incumbent_change")

medPAC_DID_comp_nogap_seat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, gap=FALSE,
                                                   outcome="incumbent_change")


PACDID_comp_seat_list <- list(
  "Baseline"= medPAC_DID_comp_seat,
  "School Shooting Covariate"= medPAC_DID_comp_treat_seat,
  "Interaction with Rep. Incumbency"= medPAC_DID_comp_int_seat,
  "Each PAC" = medPAC_DID_comp_nogap_seat
)

gof_add_medPAC_seat <- data.frame(
  raw = c("Treatment Variable", "Covariate of Fatal School Shootings", "Interaction with Rep. Incumbency"),
  mod1 = c("Gap between Pro-gun and Gun Control contributions", "No","No"),
  mod2 = c("Gap between Pro-gun and Gun Control contributions", "Yes","No"),
  mod3 = c("Gap between Pro-gun and Gun Control contributions", "No", "Yes"),
  mod4 = c("Each of Pro-gun and Gun Control contributions", "No","No"),
  
  stringsAsFactors = FALSE
)

med_DID_2year_PAC_comp_seat_plot <- gt_table(PACDID_comp_seat_list, coef_map_medPAC , c("nobs"),
                                               gof_add_medPAC_seat,
                                               "Impact of Gun Policy PAC Contributions on the Next Election Results; Competitive Districts",
                                               "Change in seat as Outcome", 
                                               source_note =med_sourcenote_SS) %>% print()



### 4.3.1 Unfiltered Non-competitive districts ------------------------------------------------

# medPAC_DID_noncomp_seat <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome="incumbent_change")
# medPAC_DID_noncomp_int_seat <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, 
#                                                     interaction = TRUE, outcome="incumbent_change")
# medPAC_DID_noncomp_nogap_seat <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, gap=FALSE,
#                                                       outcome="incumbent_change")
# 
# 
# PACDID_noncomp_seat_list <- list(
#   "Baseline"= medPAC_DID_noncomp_seat,
#   "Interaction"= medPAC_DID_noncomp_int_seat,
#   "Each PAC" = medPAC_DID_noncomp_nogap_seat)
#  
# 
# med_DID_2year_PAC_noncomp_seat_plot <- gt_table(PACDID_noncomp_seat_list, coef_map_medPAC , c("nobs"),
#                                                   gof_add_medPAC_seat,
#                                                   "Impact of Gun Policy PAC Contributions on the Next Election Results; Non-competitive Districts",
#                                                   "Change in seat as outcome", 
#                                                   source_note =med_sourcenote_SS) %>% print()




## 4.4. Incumbency ----------------------------------------------------

medPAC_DID_comp_inc <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome = "incumbent_change")
medPAC_DID_comp_notreat_inc <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome = "incumbent_change", treatment=FALSE)
# medPAC_DID_comp_nogap_inc <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome = "incumbent_change", gap=FALSE,
#                                                 interaction=TRUE,treatment=TRUE)
medPAC_DID_comp_nogap_notreat_inc <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome = "incumbent_change", gap=FALSE,
                                                        interaction=TRUE,treatment=FALSE)
medPAC_DID_comp_nogap_noint_inc <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome = "incumbent_change", gap=FALSE,
                                                      interaction=FALSE,treatment=TRUE)
medPAC_DID_comp_nogap_notreat_noint_inc <- med_DID_PAC_dollar(apmed_data_s_2year_baseline, outcome = "incumbent_change", gap=FALSE,
                                                              interaction=FALSE,treatment=FALSE)

PACDID_comp_inc_list <- list(
  "Basic"= medPAC_DID_comp_inc,
  "No shooting var." = medPAC_DID_comp_notreat_inc,
  "Each PAC"= medPAC_DID_comp_nogap_inc,
  "Each PAC; no shooting var."=medPAC_DID_comp_nogap_notreat_inc,
  "Each PAC; no interaction" = medPAC_DID_comp_nogap_noint_inc,
  "Each PAC; no shooting var.; no interaction"=medPAC_DID_comp_nogap_notreat_noint_inc
)

# add rows
gof_add_medPAC_inc <- data.frame(
  raw = c("Treatment", "School Shooting Var.", "Interaction", "Outcome (Change in ...)", "Scope of Districts" ),
  mod1 = c("Gap between gun lobby and GVP contributions", "Yes", "No", "Seat of Rep. candidates", "Competitive"),
  mod2 = c("Gap between gun lobby and GVP contributions", "No", "No", "Seat of Rep. candidates", "Competitive"),
  mod3 = c("Gun lobby and GVP contributions", "Yes", "Yes", "Seat of Rep. candidates", "Competitive"),
  mod4 = c("Gun lobby and GVP contributions", "No", "Yes", "Seat of Rep. candidates", "Competitive"),
  mod5 = c("Gun lobby and GVP contributions", "Yes", "No", "Seat of Rep. candidates", "Competitive"),
  mod6 = c("Gun lobby and GVP contributions", "No", "No", "Seat of Rep. candidates", "Competitive"),
  
  stringsAsFactors = FALSE
)


med_DID_2year_PACplot_inc <- gt_table(PACDID_comp_inc_list,coef_map_medPAC,  c("nobs"),
                                      gof_add_medPAC_inc,
                                      "Impact of Gun-related PAC Contributions on the Next Election Results; GOP Seat Outcome",
                                      "Data Aggregared by Election Cycle, Competitive Districts", 
                                      source_note ="Basic Cov. are used.")

med_DID_2year_PACplot_inc



### 4.4.1 whole districts ----------------------------------------------

medPAC_DID_whole_inc <- med_DID_PAC_dollar(apmed_data_s_2year_2000_filtered_noncomp, outcome = "incumbent_change")
medPAC_DID_whole_notreat_inc <- med_DID_PAC_dollar(apmed_data_s_2year_2000_filtered_noncomp, outcome = "incumbent_change", treatment=FALSE)
medPAC_DID_whole_nogap_inc <- med_DID_PAC_dollar(apmed_data_s_2year_2000_filtered_noncomp, outcome = "incumbent_change", gap=TRUE,
                                                 interaction=TRUE,treatment=TRUE)
medPAC_DID_whole_nogap_notreat_inc <- med_DID_PAC_dollar(apmed_data_s_2year_2000_filtered_noncomp, outcome = "incumbent_change", gap=FALSE,
                                                         interaction=TRUE,treatment=FALSE)
medPAC_DID_whole_nogap_noint_inc <- med_DID_PAC_dollar(apmed_data_s_2year_2000_filtered_noncomp, outcome = "incumbent_change", gap=FALSE,
                                                       interaction=FALSE,treatment=TRUE)
medPAC_DID_whole_nogap_notreat_noint_inc <- med_DID_PAC_dollar(apmed_data_s_2year_2000_filtered_noncomp, outcome = "incumbent_change", gap=TRUE,
                                                               interaction=FALSE,treatment=FALSE)

PACDID_whole_inc_list <- list(
  "Basic Pattern"= medPAC_DID_whole_inc,
  "No shooting var." = medPAC_DID_whole_notreat_inc,
  "Each PAC"= medPAC_DID_whole_nogap_inc,
  "Each PAC; no shooting var."=medPAC_DID_whole_nogap_notreat_inc,
  "Each PAC; no interaction" = medPAC_DID_whole_nogap_noint_inc,
  "Each PAC; no shooting var.; no interaction"=medPAC_DID_whole_nogap_notreat_noint_inc
)

# add rows
gof_add_medPAC_whole_inc <- data.frame(
  raw = c("Treatment", "School Shooting Var.", "Interaction", "Outcome (Change in ...)", "Scope of Districts" ),
  mod1 = c("Gap between gun lobby and GVP contributions", "Yes", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod2 = c("Gap between gun lobby and GVP contributions", "No", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod3 = c("Gun lobby and GVP contributions", "Yes", "Yes", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod4 = c("Gun lobby and GVP contributions", "No", "Yes", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod5 = c("Gun lobby and GVP contributions", "Yes", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod6 = c("Gun lobby and GVP contributions", "No", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  
  stringsAsFactors = FALSE
)


med_DID_2year_PACplot_whole_inc <- gt_table(PACDID_whole_inc_list,coef_map_medPAC,  c("nobs"),
                                            gof_add_medPAC_whole_inc,
                                            "Impact of Gun-related PAC Contributions on the Next Election Results; Whole Districts",
                                            "Data Aggregared by Election Cycle, GOP Seat Outcome", 
                                            source_note ="Basic Cov. are used.")

med_DID_2year_PACplot_whole_inc



### 4.4.2 unfiltered ---------------------------------------------------

medPAC_DID_whole_inc_nf <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome = "incumbent_change")
medPAC_DID_whole_notreat_inc_nf <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome = "incumbent_change", treatment=FALSE)
medPAC_DID_whole_nogap_inc_nf <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome = "incumbent_change", gap=FALSE,
                                                 interaction=TRUE,treatment=TRUE)
medPAC_DID_whole_nogap_notreat_inc_nf <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome = "incumbent_change", gap=FALSE,
                                                         interaction=TRUE,treatment=FALSE)
medPAC_DID_whole_nogap_noint_inc_nf <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome = "incumbent_change", gap=FALSE,
                                                       interaction=FALSE,treatment=TRUE)
medPAC_DID_whole_nogap_notreat_noint_inc_nf <- med_DID_PAC_dollar(apmed_data_s_2year_2000_noncomp, outcome = "incumbent_change", gap=FALSE,
                                                               interaction=FALSE,treatment=FALSE)

PACDID_whole_inc_list_nf <- list(
  "Basic Pattern"= medPAC_DID_whole_inc_nf,
  "No shooting var." = medPAC_DID_whole_notreat_inc_nf,
  "Each PAC"= medPAC_DID_whole_nogap_inc_nf,
  "Each PAC; no shooting var."=medPAC_DID_whole_nogap_notreat_inc_nf,
  "Each PAC; no interaction" = medPAC_DID_whole_nogap_noint_inc_nf,
  "Each PAC; no shooting var.; no interaction"=medPAC_DID_whole_nogap_notreat_noint_inc_nf
)

# add rows
gof_add_medPAC_whole_inc_nf <- data.frame(
  raw = c("Treatment", "School Shooting Var.", "Interaction", "Outcome (Change in ...)", "Scope of Districts" ),
  mod1 = c("Gap between gun lobby and GVP contributions", "Yes", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod2 = c("Gap between gun lobby and GVP contributions", "No", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod3 = c("Gun lobby and GVP contributions", "Yes", "Yes", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod4 = c("Gun lobby and GVP contributions", "No", "Yes", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod5 = c("Gun lobby and GVP contributions", "Yes", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  mod6 = c("Gun lobby and GVP contributions", "No", "No", "Seat of Rep. candidates", "Whole (unfiltered)"),
  
  stringsAsFactors = FALSE
)


med_DID_2year_PACplot_whole_inc_nf <- gt_table(PACDID_whole_inc_list_nf,coef_map_medPAC,  c("nobs"),
                                            gof_add_medPAC_whole_inc_nf,
                                            "Impact of Gun-related PAC Contributions on the Next Election Results; Whole Districts",
                                            "Data Aggregared by Election Cycle, GOP Seat Outcome", 
                                            source_note ="Basic Cov. are used.")

med_DID_2year_PACplot_whole_inc_nf

### 4.4.3. Focusing on each side incumbency ------------------------------------------

#apmed_data_s_2year_wholerep, apmed_data_s_2year_wholedem
# apmed_data_s_2year_baseline_rep, apmed_data_s_2year_baseline_dem


medPAC_DID_comp_inc_rep <- med_DID_PAC_dollar(apmed_data_s_2year_baseline_rep, outcome = "incumbent_change")
medPAC_DID_comp_inc_rep_notreat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline_rep, outcome = "incumbent_change", treatment = FALSE)
# medPAC_DID_whole_inc_rep <- med_DID_PAC_dollar(apmed_data_s_2year_wholerep, outcome = "incumbent_change", treatment=FALSE)
medPAC_DID_comp_inc_dem <- med_DID_PAC_dollar(apmed_data_s_2year_baseline_dem, outcome = "incumbent_change")
medPAC_DID_comp_inc_dem_notreat <- med_DID_PAC_dollar(apmed_data_s_2year_baseline_dem, outcome = "incumbent_change", treatment = FALSE)
# medPAC_DID_whole_inc_dem <- med_DID_PAC_dollar(apmed_data_s_2year_wholedem, outcome = "incumbent_change", treatment=FALSE)

PACDID_comp_incfocus_list <- list(
  "GOP Incumbent; Competitive"= medPAC_DID_comp_inc_rep,
  "GOP Incumbent; No School Shooting Cov."= medPAC_DID_comp_inc_rep_notreat,
  # "GOP Incumbent; Whole"= medPAC_DID_whole_inc_rep,
  "DEM Incumbent; Competitive"= medPAC_DID_comp_inc_dem,
  "DEM Incumbent; No School Shooting Cov."= medPAC_DID_comp_inc_dem_notreat
  # "DEM Incumbent; Whole"= medPAC_DID_whole_inc_dem
)

# add rows
gof_add_medPAC_incfocus <- data.frame(
  raw = c("Incumbency", "School Shooting Cov." ),
  mod1 = c( "GOP", "Yes"),
  mod2 = c( "GOP", "No"),
  mod3 = c( "DEM", "Yes"),
  mod4 = c( "DEM", "No"),
  stringsAsFactors = FALSE
)


med_DID_2year_PACplot_incfocus <- gt_table(PACDID_comp_incfocus_list,coef_map_medPAC,  c("nobs"),
                                           gof_add_medPAC_incfocus,
                                           "Impact of Gap in Gun-related PAC Contributions on the Next Election Seat",
                                           "Competitive Districts; Data Aggregared by Election Cycle", 
                                           source_note ="Basic Cov. are used")

med_DID_2year_PACplot_incfocus


## 4.5 whole districts ----------------------------------------------


medPAC_DID_whole_inc_rep <- med_DID_PAC_dollar(apmed_data_s_2year_wholerep, outcome = "incumbent_change")
medPAC_DID_whole_inc_rep_notreat <- med_DID_PAC_dollar(apmed_data_s_2year_wholerep, outcome = "incumbent_change", treatment = FALSE)
# medPAC_DID_whole_inc_rep <- med_DID_PAC_dollar(apmed_data_s_2year_wholerep, outcome = "incumbent_change", treatment=FALSE)
medPAC_DID_whole_inc_dem <- med_DID_PAC_dollar(apmed_data_s_2year_wholedem, outcome = "incumbent_change")
medPAC_DID_whole_inc_dem_notreat <- med_DID_PAC_dollar(apmed_data_s_2year_wholedem, outcome = "incumbent_change", treatment = FALSE)
# medPAC_DID_whole_inc_dem <- med_DID_PAC_dollar(apmed_data_s_2year_wholedem, outcome = "incumbent_change", treatment=FALSE)

PACDID_whole_incfocus_list <- list(
  "GOP Incumbent"= medPAC_DID_whole_inc_rep,
  "GOP Incumbent; No School Shooting Cov."= medPAC_DID_whole_inc_rep_notreat,
  # "GOP Incumbent; Whole"= medPAC_DID_whole_inc_rep,
  "DEM Incumbent"= medPAC_DID_whole_inc_dem,
  "DEM Incumbent; No School Shooting Cov."= medPAC_DID_whole_inc_dem_notreat
  # "DEM Incumbent; Whole"= medPAC_DID_whole_inc_dem
)

med_DID_2year_PACplot_whole_incfocus <- gt_table(PACDID_whole_incfocus_list,coef_map_medPAC,  c("nobs"),
                                                 gof_add_medPAC_incfocus,
                                                 "Impact of Gap in Gun-related PAC Contributions on the Next Election Seat",
                                                 "Whole Districts (Unfiltered); Data Aggregared by Election Cycle", 
                                                 source_note ="Basic Cov. are used")

med_DID_2year_PACplot_whole_incfocus



## 4.6 IV ------------------------------------------------------------------

# strong instrument at competitive districts, but weak at non-competitive districts
# one endogenous var., which means gap, would be better??

# no cov ver.
# iv_nocov_whole_margin <- fixest::feols(pct_RDmargin_change ~ 1 | id + election_year_next |
#                                      gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_2000_filtered)
# iv_nocov_whole_share <- fixest::feols(pct_rep_change ~ 1 | id + election_year_next |
#                                          gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_2000_filtered)
# iv_nocov_whole_incum <- fixest::feols(incumbent_change ~ 1 | id + election_year_next |
#                                         gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_2000_filtered)
# 
# iv_nocov_baseline_margin <- fixest::feols(pct_RDmargin_change ~ 1 | id + election_year_next |
#                                         gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_baseline)
# iv_nocov_baseline_share <- fixest::feols(pct_rep_change ~ 1 | id + election_year_next |
#                                             gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_baseline)
# iv_nocov_baseline_incum <- fixest::feols(incumbent_change ~ 1 | id + election_year_next |
#                                            gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_baseline)

iv_whole_margin <- fixest::feols(pct_RDmargin_change ~ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)
                                 | id + election_year_next |
                                   gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_2000_filtered)
iv_baseline_margin <- fixest::feols(pct_RDmargin_change ~ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before) | id + election_year_next |
                                      gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_baseline)

iv_whole_share <- fixest::feols(pct_rep_change ~ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)
                                 | id + election_year_next |
                                   gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_2000_filtered)
iv_baseline_share <- fixest::feols(pct_rep_change ~ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before) | id + election_year_next |
                                      gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_baseline)
iv_whole_incum <- fixest::feols(incumbent_change ~ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before)
                                 | id + election_year_next |
                                   gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_2000_filtered)
iv_baseline_incum <- fixest::feols(incumbent_change ~ rep_incumbent_before+bachelor+ black+white+unemployment+log_income+gun_ratio_pct +abs(pct_RDmargin_before) | id + election_year_next |
                                      gap_gaplog_sum ~ treatment, data = apmed_data_s_2year_baseline)


# how strong??
f_iv_fe_cov_baseline <- fitstat(iv_baseline_margin, type = "ivf")
# how to extract info
# class(f_iv_fe_cov_baseline)
# f_iv_fe_cov_baseline[[1]]$stat

# how endogenous
wu_hausman_iv_fe_cov_baseline <- fitstat(iv_fe_cov_baseline, type = "ivwald") 
wu_hausman_iv_fe_cov_baseline[[1]] #$stat


### 4.6.1 visualization -----------------------------------------------------


IV_election_list <- list(
  # "No FE; Whole Districts"= iv_simple_whole,
  # "No FE; Competitive Districts" = iv_simple_baseline,
  "Vote Share Outcome; All Districts"= iv_whole_share,
  "Vote Share Outcome; Competitive Districts"= iv_baseline_share,
  "Vote Margin Outcome; All Districts" = iv_whole_margin,
  "Vote Margin Outcome; Competitive Districts"=iv_baseline_margin,
  "Seat Outcome; All Districts" = iv_whole_incum,
  "Seat Outcome; Competitive Districts" = iv_baseline_incum
)


# check var. name
names(coef(iv_baseline_incum))

# have to cover both, since ivreg and fixest use diff. names
coef_map_IV <- c(
  "fit_gap_gaplog_sum"="Contribution Gap",
  "gap_gaplog_sum" = "Contribution Gap"
  # "fit_incumbent_change"="Seat Change"
)

# add rows
gof_add_IV <- data.frame(
  raw = c("F-value"
          , "Wu-Hausman's p-value"
          ,"Scope of Districts", 
          "Outcome (Change in ...)"),
  # mod1 = c(sprintf("%.3f",summary(iv_simple_whole, diagnostics = TRUE)$diagnostics["Weak instruments", "statistic"]),
  #          # fitstat(iv_simple_whole, type = "ivf")[[1]]$stat, # for fixest package
  #          sprintf("%.3f",summary(iv_simple_whole, diagnostics = TRUE)$diagnostics["Wu-Hausman", "p-value"]), 
  #          # fitstat(iv_simple_whole, type = "ivwald") [[2]]$stat,
  #          "Whole Districts (unfiltered)", "Vote Margin for Rep. candidates", "No", "Yes" ),
  # mod2 = c(sprintf("%.3f",summary(iv_simple_baseline, diagnostics = TRUE)$diagnostics["Weak instruments", "statistic"]),
  #          sprintf("%.3f",summary(iv_simple_baseline, diagnostics = TRUE)$diagnostics["Wu-Hausman", "p-value"]), 
  #          "Competitive Districts", "Vote Margin for Rep. candidates", "No", "Yes"),
  mod1 = c(sprintf("%.3f",fitstat(iv_whole_share, type = "ivf")[[1]]$stat),
           sprintf("%.3f",fitstat(iv_whole_share, type = "ivwald") [[1]]$p),
           # 0.439993,
           "All Districts",
           # "Vote Margin for Rep. candidates",
           # "Yes", 
           "Vote Share"),
  mod2 = c(sprintf("%.3f",fitstat(iv_baseline_share, type = "ivf")[[1]]$stat),
           sprintf("%.3f",fitstat(iv_baseline_share, type = "ivwald") [[1]]$p),
           "Competitive Districts", 
           # "Vote Margin for Rep. candidates",
           # "Yes", 
           "Vote Share"),
  mod3 = c(sprintf("%.3f",fitstat(iv_whole_margin, type = "ivf")[[1]]$stat),
           sprintf("%.3f",fitstat(iv_whole_margin, type = "ivwald") [[1]]$p),
           "All Districts", 
           # "Vote Margin for Rep. candidates",
           # "Yes",
           "Vote Margin"),
  mod4 = c(sprintf("%.3f",fitstat(iv_baseline_margin, type = "ivf")[[1]]$stat),
           sprintf("%.3f",fitstat(iv_baseline_margin, type = "ivwald") [[1]]$p),
           "Competitive Districts", 
           # "Vote Margin for Rep. candidates",
           # "Yes",
           "Vote Margin"),
  mod5 = c(sprintf("%.3f",fitstat(iv_whole_incum, type = "ivf")[[1]]$stat),
           sprintf("%.3f",fitstat(iv_whole_incum, type = "ivwald") [[1]]$p),
           "All Districts", "Seat of Rep. candidates"),
  mod6 = c(sprintf("%.3f",fitstat(iv_baseline_incum, type = "ivf")[[1]]$stat),
           sprintf("%.3f",fitstat(iv_baseline_incum, type = "ivwald") [[1]]$p),
           "Competitive Districts", "Seat of Rep. candidates"),
  
  
  
  stringsAsFactors = FALSE
)


med_IV_result <- gt_table(IV_election_list,coef_map_IV,  c("nobs"),
                          gof_add_IV,
                          "Impact of Gun Policy PAC Contributions on the Next Election Results; IV Method",
                          "Gap of Contributions as Treatment; Three Different Outcomes", 
                          source_note ="Baseline covariates, absolute value of prior voting margin, and two-way FEs") %>% print()




# 5 Semi-parametric ------------------------------------------------------

kernel_df <- apmed_data_s_2year_2000_filtered  %>% # all districts with the same threshold
  filter(abs(pct_RDmargin_before)<=40)
  # filter(election_year_next<2020)

kernel_margin <- kernel_df$pct_RDmargin_before

# Silverman’s Rule of Thumb for band-width
# multiplying 1.25 will lead to more smoothness
kernel_h   <- 1.06 * sd(kernel_margin, na.rm = TRUE) * sum(!is.na(kernel_margin))^(-1/5) * 1.25

# use Ktri function to triangular kernel for linear weighting
# when u is close to zero, larger weight

# margin grid
# from 0.05 to 0.95, devided by 60
grid_margin <- seq(
  quantile(kernel_margin, 0.05, na.rm = TRUE),
  quantile(kernel_margin, 0.95, na.rm = TRUE),
  length.out = 60
)

kernel_yvar1 <- "incumbent_change"
kernel_yvar2 <- "pct_rep_change"
kernel_yvar3 <- "pct_RDmargin_change"
kernel_yvar_all <- c("pct_rep_change", "pct_RDmargin_change", "incumbent_change")

# use estimate_local_fe_for_y function

# estimates using y list
kernel_res_1 <- map_dfr(kernel_yvar1, estimate_local_fe_for_y, kernel_df) %>%
  mutate(
    kernel_term = factor(term, # treatment pattern
                         levels = c("pro_log_sum","anti_log_sum"),
                         labels = c("Pro-gun PAC","Gun-control PAC")),
    kernel_low = estimate - 1.96*se,
    kernel_high = estimate + 1.96*se
  )

kernel_res_2 <- map_dfr(kernel_yvar2, estimate_local_fe_for_y, kernel_df) %>%
  mutate(
    kernel_term = factor(term, 
                         levels = c("pro_log_sum","anti_log_sum"),
                         labels = c("Pro-gun PAC","Gun-control PAC")),
    kernel_low = estimate - 1.96*se,
    kernel_high = estimate + 1.96*se
  )

kernel_res_3 <- map_dfr(kernel_yvar3, estimate_local_fe_for_y, kernel_df) %>%
  mutate(
    kernel_term = factor(term, 
                         levels = c("pro_log_sum","anti_log_sum"),
                         labels = c("Pro-gun PAC","Gun-control PAC")),
    kernel_low = estimate - 1.96*se,
    kernel_high = estimate + 1.96*se
  )

# plot of change seats
ggplot(kernel_res_1, aes(x = margin, y = estimate, color = kernel_term, fill = kernel_term)) +
  geom_ribbon(aes(ymin = kernel_low, ymax = kernel_high), alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed", color = "gray20") +
  geom_hline(yintercept = 0) +
  
  # for multiple outcome
  # facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  
  labs(x = "Voting margin in previous election (%)",
       y = "Local FE effect",
       color = "PAC Type", fill = "PAC Type",
       # title = "Effect of PAC contributions on election outcomes by voting margin"
       title=""
       ) +
  scale_color_manual(
    values = c("Pro-gun PAC" = "salmon", "Gun-control PAC" = "turquoise"),
    limits = c("Pro-gun PAC", "Gun-control PAC"),
    name   = "PAC Type"
  ) +
  scale_fill_manual(
    values = c("Pro-gun PAC" = "salmon", "Gun-control PAC" = "turquoise"),
    limits = c("Pro-gun PAC", "Gun-control PAC"),
    name   = "PAC Type"
  ) +
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# plot of other outcomes
ggplot(kernel_res_2, aes(x = margin, y = estimate, color = kernel_term, fill = kernel_term)) +
  geom_ribbon(aes(ymin = kernel_low, ymax = kernel_high), alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed", color = "gray20") +
  geom_hline(yintercept = 0) +
  
  # facet_wrap(~ outcome, ncol = 1, scales = "free_y",
  #            labeller = labeller(
  #              outcome = c(
  #                pct_rep_change      = "Change in Vote Share",
  #                pct_RDmargin_change = "Change in Vote Margin"
  #              )
  #            )) +
  
  labs(x = "Voting margin in previous election (%)",
       y = "Local FE effect",
       color = "PAC Type", fill = "PAC Type",
       # title = "Effect of PAC contributions on election outcomes by voting margin"
       title=""
  ) +
  scale_color_manual(
    values = c("Pro-gun PAC" = "salmon", "Gun-control PAC" = "turquoise"),
    limits = c("Pro-gun PAC", "Gun-control PAC"),
    name   = "PAC Type"
  ) +
  scale_fill_manual(
    values = c("Pro-gun PAC" = "salmon", "Gun-control PAC" = "turquoise"),
    limits = c("Pro-gun PAC", "Gun-control PAC"),
    name   = "PAC Type"
  ) +
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


ggplot(kernel_res_3, aes(x = margin, y = estimate, color = kernel_term, fill = kernel_term)) +
  geom_ribbon(aes(ymin = kernel_low, ymax = kernel_high), alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed", color = "gray20") +
  geom_hline(yintercept = 0) +
  
  # facet_wrap(~ outcome, ncol = 1, scales = "free_y",
  #            labeller = labeller(
  #              outcome = c(
  #                pct_rep_change      = "Change in Vote Share",
  #                pct_RDmargin_change = "Change in Vote Margin"
  #              )
  #            )) +
  
  labs(x = "Voting margin in previous election (%)",
       y = "Local FE effect",
       color = "PAC Type", fill = "PAC Type",
       # title = "Effect of PAC contributions on election outcomes by voting margin"
       title=""
  ) +
  scale_color_manual(
    values = c("Pro-gun PAC" = "salmon", "Gun-control PAC" = "turquoise"),
    limits = c("Pro-gun PAC", "Gun-control PAC"),
    name   = "PAC Type"
  ) +
  scale_fill_manual(
    values = c("Pro-gun PAC" = "salmon", "Gun-control PAC" = "turquoise"),
    limits = c("Pro-gun PAC", "Gun-control PAC"),
    name   = "PAC Type"
  ) +
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

