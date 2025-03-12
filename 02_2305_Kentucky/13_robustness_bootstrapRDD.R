######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Bootstrapping SE estimates for RDD with alternative ML classifiers 
# 
#---------------------------------------------------------------------
rm(list = ls())

library(dplyr)      
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


app_svm <- with(df, rdrobust(y = approval,
                             x = running_variable_svm,
                             covs = final_rent_inc_svm + renter_pop,
                             c = 0,
                             bwselect = "mserd"))
app_xgboost <- with(df, rdrobust(y = approval,
                                 x = running_variable_xgboost,
                                 covs = final_rent_inc_xgboost + renter_pop,
                                 c = 0,
                                 bwselect = "mserd"))
app_knn <- with(df, rdrobust(y = approval,
                             x = running_variable_knn,
                             covs = final_rent_inc_knn + renter_pop,
                             c = 0,
                             bwselect = "mserd"))

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
      rdd_covs_boot_resample_within_svm <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable_svm,
                      covs = final_rent_inc_svm + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      rdd_covs_boot_resample_within_xgboost <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable_xgboost,
                      covs = final_rent_inc_xgboost + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      rdd_covs_boot_resample_within_knn <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable_knn,
                      covs = final_rent_inc_knn + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      
      rdd_covs_boot_resample_within_svm$model <- "svm"
      rdd_covs_boot_resample_within_xgboost$model <- "xgboost"
      rdd_covs_boot_resample_within_knn$model <- "knn"
      
      
      rbind(rdd_covs_boot_resample_within_svm,
            rdd_covs_boot_resample_within_xgboost,
            rdd_covs_boot_resample_within_knn
      )
      
      
    }, 
    simplify = FALSE)
  
  sim_mat <- sims %>% do.call(what = rbind.data.frame)
  write.csv(x = sim_mat, file = paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_sim_mat_robustml.csv"))
  
}

sim_mat <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_sim_mat_robustml.csv"))



observed_coefs <- data.frame(
  observed_est = c(
    tidy(app_svm) %>% filter(term == "Robust") %>% pull(estimate), 
    tidy(app_xgboost) %>% filter(term == "Robust") %>% pull(estimate), 
    tidy(app_knn) %>% filter(term == "Robust") %>% pull(estimate)
  ), 
  model = c("svm", "xgboost", "knn"))

boot_ses <- 
  sim_mat %>% group_by(model) %>% 
  filter(term == "Robust") %>% 
  summarize(se = sd(estimate, na.rm = TRUE)) %>% 
  left_join(observed_coefs, by = "model") %>% 
  mutate(lower_ci = observed_est - 1.96 * se, 
         upper_ci = observed_est + 1.96 * se
  )


write.csv(x = boot_ses, file = paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_SEs_robustml.csv"))


## write results
boot_ses <- read.csv(file = paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_SEs_robustml.csv"))
boot_results <- as.data.frame(boot_ses %>% select(model, observed_est, se, lower_ci, upper_ci) %>% arrange(desc(model)))
write.csv(boot_results, paste0(BASE_DIR, "2305_model_output/rddresults_robustml.csv"),
          row.names = FALSE)
