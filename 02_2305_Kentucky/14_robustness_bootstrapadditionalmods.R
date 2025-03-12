######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Bootstrapping SE estimates for additional models 
# 
#---------------------------------------------------------------------
rm(list = ls())

library(dplyr)       # for data manipulation
library(estimatr)
library(zoo)
library(rdrobust)
source("00_2301_misc/analysis_helpers.R")

BASE_DIR <- "G:/Shared drives/OES data 2305 ERA Grantee Flexibilities KY/"
df <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))

# Bootstrap SEs -----------------------------------------------------------

ids <- df %>% select(row_id, zip)
fsp_zips <- df %>% filter(fsp == 1) %>% pull(zip) %>% unique()
non_fsp_zips <- df %>% filter(fsp == 0) %>% pull(zip) %>% unique()

resample_df <- function(resample_within = TRUE){
  
  # Resample zips
  zips_resample <- c(
    sample(x = fsp_zips, replace = TRUE),
    sample(x = non_fsp_zips, replace = TRUE))
  
  # Build resulting dataset
  new_zip_id <- 1
  resampled_zips <- 
    # Loop through the resampled ZIPs, building out the IDs
    lapply(
      X = zips_resample, 
      FUN = function(zip_to_resample){
        resample <- ids %>% filter(zip == zip_to_resample) %>% 
          # The resample will duplicate ZIPs, but we want to treat
          # them like different clusters.
          mutate(zip_id_resample = new_zip_id)
        new_zip_id <<- new_zip_id + 1
        return(resample)
      }
    ) %>% 
    # Put list of dataframes into a single dataframe
    do.call(what = rbind.data.frame) %>% 
    data.frame()
  
  if(resample_within){
    # Now go through the resampled ZIPs and resample applications
    # need to use loop because group_by + mutate breaks with zips
    # of size 1
    for(i in unique(resampled_zips$zip_id_resample)){
      
      rows_to_resample <- 
        resampled_zips %>% 
        filter(zip_id_resample == i) %>% 
        pull(row_id) 
      
      if(length(rows_to_resample) == 1) resampled_rows <- rows_to_resample
      else resampled_rows <- sample(x = rows_to_resample, replace = TRUE)
      
      resampled_zips$row_id[resampled_zips$zip_id_resample == i] <- resampled_rows
    }
  }
  
  resampled_zips <- left_join(df, resampled_zips, by = c("row_id", "zip"))
  
  return(resampled_zips)
}


app_approval_marketing <- with(df, rdrobust(y = approval,
                                            x = running_variable,
                                            covs = final_rent_inc + renter_pop + marketing,
                                            c = 0,
                                            bwselect = "mserd"))
app_ipw  <- with(df, lm_robust(formula = 
                                 approval ~  fsp + final_rent_inc + 
                                 renter_pop + suppressed,
                               weights = ipw))

rerun_bootstraps <- FALSE
if(rerun_bootstraps){
  set.seed(1234)
  sim_counter <- 0
  sims <- replicate(
    n = 200, 
    expr = {
      sim_counter <<- sim_counter + 1
      print(sim_counter)
      resampled_data <- resample_df(resample_within = TRUE)
      marketing_bootstrap <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable,
                      covs = final_rent_inc + renter_pop + marketing,
                      c = 0,
                      bwselect = "mserd")) %>% tidy() %>% select(-cutoff) 
      ipw_bootstrap <- 
        with(resampled_data, 
             lm_robust(formula = 
                         approval ~  fsp + final_rent_inc + 
                         renter_pop + suppressed,
                       weights = ipw)) %>% tidy() %>% select(-df, -outcome)
      resampled_data_nowithin <- resample_df(resample_within = FALSE)
      ipw_bootstrap_nowithin <- 
        with(resampled_data_nowithin, 
             lm_robust(formula = 
                         approval ~  fsp + final_rent_inc + 
                         renter_pop + suppressed,
                       weights = ipw)) %>% tidy() %>% select(-df, -outcome)
      
      
      marketing_bootstrap$model <- "marketing robust"
      ipw_bootstrap$model <- "ipw robust"
      ipw_bootstrap_nowithin$model <- "ipw_robust"
      marketing_bootstrap$bootstrap_method <- "within"
      ipw_bootstrap$bootstrap_method <- "within"
      ipw_bootstrap_nowithin$bootstrap_method <- "no within"
      
      rbind(marketing_bootstrap,
            ipw_bootstrap,
            ipw_bootstrap_nowithin
      )
      
      
    }, 
    simplify = FALSE)
  sim_mat <- sims %>% do.call(what = rbind.data.frame)
  write.csv(x = sim_mat, file = paste0(BASE_DIR, "2305_intermediate_data/sim_mat_bootstrapSEs_IPWmarketing.csv"))
}

sim_mat <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/sim_mat_bootstrapSEs_IPWmarketing.csv"))


observed_coefs <- data.frame(
  observed_est = c(
    tidy(app_approval_marketing) %>% filter(term == "Robust") %>% pull(estimate), 
    tidy(app_ipw) %>% filter(term == "fspTRUE") %>% pull(estimate)
  ), 
  model = c("marketing robust", "ipw robust"))

boot_ses <- 
  sim_mat %>% 
  filter(term %in% c("Robust", "fspTRUE")) %>% 
  mutate(model = ifelse(model %in% c("ipw_robust", "ipw robust"), "ipw robust", model)) %>%
  group_by(model, bootstrap_method) %>% 
  summarize(se = sd(estimate, na.rm = TRUE)) %>% 
  left_join(observed_coefs, by = c("model")) %>% 
  mutate(lower_ci = observed_est - 1.96 * se, 
         upper_ci = observed_est + 1.96 * se
  )


write.csv(x = boot_ses, file = paste0(BASE_DIR, "2305_intermediate_data/bootstrapSEs_IPWmarketing.csv"))





