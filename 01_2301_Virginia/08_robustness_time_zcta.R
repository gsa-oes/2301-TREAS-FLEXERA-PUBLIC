######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Robustness to time.
# 81_robustness_time.R
# 
# Check if application count and total paid 
# estimates are robust to time changes.
#---------------------------------------------------------------------

library(tidyverse)
source("00_2301_misc/analysis_helpers.R")

zctas_to_drop <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/zctas_to_drop.rds")
df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds")
all_apps <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/cleaned_applications.rds")

# Define weekly increasing "post" time limit
days(mdy("12/1/2021") - mdy("6/10/2021"))


week_limits <- mdy("6/10/2021") + (1:25 * 7)

# make past one 12/1/2021, not 12/2/2021
week_limits[25] <- "2021-12-01"

week_res <- lapply(week_limits, function(week_limit) {
    
  samp_apps <- all_apps
  
  #--------------------------------------------
  # Start with Application Count
  app_vars <- samp_apps %>% 
    # Filter to reported HHSize <= 3 and in our time window
    filter(!is.na(`Household Size`), `Household Size` <= 3) %>% 
    filter(mdy(`Submission Date`) >= mdy("12/22/2020")) %>% 
    filter(mdy(`Submission Date`) <= week_limit) %>% 
    rename(zip = `Property Zipcode`) %>% 
    mutate(zip = as.character(zip)) %>%
    filter(!(zip %in% zctas_to_drop)) %>% 
    # count in each ZIP, pre-post combination
    group_by(zip, post_FSP) %>% 
    summarise(n_app = n(),
              total_paid = sum(`Payment Amount`, na.rm = TRUE),
              .groups = "drop"
    ) %>% 
    pivot_wider(id_cols = zip,  
                names_from = post_FSP,
                values_from = n_app:total_paid) %>% 
    # Summarise value of 0 indicates missing, so replace with 0
    mutate(zip = as.character(zip))
  
  # Merge back onto ZIP sample for med_inc
  this_run_df <- df %>% 
    select(zip, med_inc, fsp) %>% 
    left_join(app_vars, by = "zip") %>% 
    # impute 0s for missing zips and log outcomes
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)),
           log_app_post = log1p(n_app_TRUE),
           log_app_pre  = log1p(n_app_FALSE),
           log_paid_post = log1p(total_paid_TRUE),
           log_paid_pre = log1p(total_paid_FALSE)
    )
  
  
  #-------------------------------------------
  # AFC
  afc_napp <- lm_robust(formula = log_app_post ~ fsp + log_app_pre + med_inc,
                        data = this_run_df) %>% 
    tidy() %>% 
    select(-outcome) %>% 
    subset(term == "fsp") %>% 
    mutate(model = "N-apps")
  
  afc_paid <- lm_robust(formula = log_paid_post ~ fsp + log_paid_pre + med_inc,
                        data = this_run_df) %>% 
    tidy() %>% 
    select(-outcome) %>% 
    subset(term == "fsp") %>% 
    mutate(model = "Total Paid")
  
  bind_rows(afc_napp,
            afc_paid) %>% 
    mutate(week_limit = week_limit)
  
})

bind_rows(week_res) %>% 
  mutate(model = case_when(model == "N-apps" ~ "Log(# Apps + 1)",
                           model == "Total Paid" ~ "Log(Total Paid + 1)",
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
