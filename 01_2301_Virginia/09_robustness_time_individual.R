######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Robustness to time for individual analyses.
# 81_robustness_time.R
# 
# Check if approval and processing time
# estimates are robust to time changes.
#---------------------------------------------------------------------

rm(list=ls())
source("00_2301_misc/analysis_helpers.R")
library(tidyverse)

df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/indiv_robustness_time_apps.rds")
run_df_clean <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/run_df_clean.rds")
zctas_to_drop <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/zctas_to_drop.rds")

# Weekly increments between 12/22/2020 and 6/10/2021
mdy("6/10/2021") - mdy("12/22/2020")
week_limits <- mdy("6/10/2021") - (1:24 * 7)

# make past one 12/22/2020, not 12/24/2020
week_limits[24] <- "2020-12-22"

week_res <- lapply(week_limits, function(week_limit) {

  samp_df <- df
  
  indiv <- samp_df %>%
    select(application_id, Status, Approved,
           `Household Size`, `Submission Date`, `Payment Amount`,
           zip = `Property Zipcode`,
           most_recent_status_date,
           post_FSP,
           `Applicant Race`,
           `Applicant Ethnicity`,
           `Is Applicant Veteran`,
           `Does Applicant Have Disability`,
           `AMI Bucket`) %>% 
    mutate(zip = as.character(zip)) %>% 
    left_join(run_df_clean, by = "zip") %>% 
    mutate(running_variable = med_inc - 66950,
           running_variable = -1 * running_variable,
           fsp = ifelse(med_inc <= 66950, 1, 0)) %>% 
    # still drop applications from other programs
    filter(!(zip %in% zctas_to_drop)) %>%
    # Have to re-create same date variables
    mutate(submission_date_clean = 
             `Submission Date` %>% 
             mdy(),
           processing_date_clean = 
             most_recent_status_date %>% 
             substr(1, 10) %>% 
             mdy()) %>% 
    mutate(processing_date_clean = 
             # Get date of most recent status
             most_recent_status_date %>% 
             # Get first 10 characters (YYYY/MM/DD)
             substr(1, 10) %>% 
             # Convert to date format
             mdy(), 
           submission_date_clean = 
             # Get date of submission
             `Submission Date` %>% 
             # Convert to date format
             mdy(), 
           proc_time = processing_date_clean - submission_date_clean) %>% 
    filter(`Household Size` <= 3) %>%
    filter(submission_date_clean >= week_limit) %>% 
    filter(submission_date_clean < mdy("6/10/2021")) %>% 
    filter(processing_date_clean >= mdy("6/10/2021")) %>% 
    mutate(proc_time_win = Winsorize(proc_time, na.rm = TRUE))
  
  indiv$approved <- ifelse(indiv$Status == "APPROVED-PAYMENT REQUEST",
                           1, 0)
  
  afc_proc <- lm_robust(proc_time_win ~ fsp + med_inc,
                        clusters = zip,
                        data = indiv) %>% 
    tidy() %>% 
    filter(term == "fsp") %>% 
    mutate(model = "Proc Time")
  
  afc_approv <- lm_robust(approved ~ fsp + med_inc,
                          clusters = zip,
                          data = indiv) %>% 
    tidy() %>% 
    filter(term == "fsp") %>% 
    mutate(model = "Approval")
  
  #-------------------------------------------
  bind_rows(afc_proc,
            afc_approv) %>% 
    mutate(week_limit = week_limit)
})

bind_rows(week_res) %>% 
  mutate(model = case_when(model == "Proc Time" ~ "Processing Time",
                           model == "Approval" ~ "Approval Probability",
                           TRUE ~ NA_character_)) %>% 
  ggplot(aes(x = week_limit, y = estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  facet_wrap(~model, scales = "free_y") + 
  geom_hline(yintercept = 0, col = "red", lty = "dashed") + 
  theme_bw() + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10)) +
  labs(y = "Estimated \n FSP Coefficient",
       x = "Rolling Week Cutoff")
