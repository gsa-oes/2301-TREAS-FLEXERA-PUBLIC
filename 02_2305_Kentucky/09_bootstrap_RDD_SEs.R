######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Exploratory analyses that were not pre-registered
#---------------------------------------------------------------------
rm(list = ls())

library(tidyverse)       
library(estimatr)
library(rdrobust)
library(zoo)
source("00_2301_misc/analysis_helpers.R")

BASE_DIR <- "G:/Shared drives/OES data 2305 ERA Grantee Flexibilities KY/"
df <- read_csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))

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

app_1 <- with(df, rdrobust(y = approval,
                           x = running_variable,
                           # covs = final_rent_inc + renter_pop,
                           c = 0,
                           bwselect = "mserd"))
app_2 <- with(df, rdrobust(y = approval,
                           x = running_variable,
                           covs = final_rent_inc + renter_pop,
                           c = 0,
                           bwselect = "mserd"))

# Status: ‘ Applications Denied - Suspected Fraud’
df$app_fraud <- df$status == "Applications Denied - Suspected Fraud" 


fraud_rdd_cov <- with(df, rdrobust(y = app_fraud,
                                   x = running_variable,
                                   # cluster = zip,
                                   covs = suppressed + renter_pop, 
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
      rdd_no_cov_boot_resample_within <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable,
                      # covs = final_rent_inc + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      rdd_covs_boot_resample_within <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable,
                      covs = final_rent_inc + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      rdd_covs_boot_resample_within_fraud <- 
        with(resampled_data, rdrobust(y = app_fraud,
                                      x = running_variable,
                                      # cluster = zip,
                                      covs = suppressed + renter_pop, 
                                      c = 0,
                                      bwselect = "mserd")) %>% tidy()
      resampled_data <- resample_df(resample_within = FALSE)
      rdd_no_cov_boot_no_resample_within <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable,
                      # covs = final_rent_inc + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      rdd_covs_boot_no_resample_within <- 
        with(resampled_data, 
             rdrobust(y = approval,
                      x = running_variable,
                      covs = final_rent_inc + renter_pop,
                      c = 0,
                      bwselect = "mserd")) %>% tidy()
      rdd_covs_boot_no_resample_within_fraud <- 
        with(resampled_data, rdrobust(y = app_fraud,
                                      x = running_variable,
                                      # cluster = zip,
                                      covs = suppressed + renter_pop, 
                                      c = 0,
                                      bwselect = "mserd")) %>% tidy()
      
      rdd_no_cov_boot_resample_within$model <- "approval - no cov"
      rdd_covs_boot_resample_within$model <- "approval - covs"
      rdd_covs_boot_resample_within_fraud$model <- "fraud - covs"
      rdd_no_cov_boot_no_resample_within$model <- "approval - no cov"
      rdd_covs_boot_no_resample_within$model <- "approval - covs"
      rdd_covs_boot_no_resample_within_fraud$model <- "fraud - covs"
      rdd_no_cov_boot_resample_within$bootstrap_method <- "within-cluster resample"
      rdd_covs_boot_resample_within$bootstrap_method <- "within-cluster resample"
      rdd_covs_boot_resample_within_fraud$bootstrap_method <- "within-cluster resample"
      rdd_no_cov_boot_no_resample_within$bootstrap_method <- "no within-cluster resample"
      rdd_covs_boot_no_resample_within$bootstrap_method <- "no within-cluster resample"
      rdd_covs_boot_no_resample_within_fraud$bootstrap_method <- "no within-cluster resample"
      
      
      rbind(rdd_no_cov_boot_resample_within,
            rdd_covs_boot_resample_within,
            rdd_covs_boot_resample_within_fraud,
            rdd_no_cov_boot_no_resample_within,
            rdd_covs_boot_no_resample_within,
            rdd_covs_boot_no_resample_within_fraud
      )
      
      
    }, 
    simplify = FALSE)
  
  sim_mat <- sims %>% do.call(what = rbind.data.frame)
  write.csv(x = sim_mat, file = paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_sim_mat.csv"))
  
}

sim_mat <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_sim_mat.csv"))


observed_coefs <- data.frame(
  observed_est = c(
    tidy(app_1) %>% filter(term == "Robust") %>% pull(estimate), 
    tidy(app_2) %>% filter(term == "Robust") %>% pull(estimate), 
    tidy(fraud_rdd_cov) %>% filter(term == "Robust") %>% pull(estimate)
  ), 
  model = c("approval - no cov", "approval - covs", "fraud - covs"))

boot_ses <- 
  sim_mat %>% group_by(model, bootstrap_method) %>% 
  filter(term == "Robust") %>% 
  summarize(se = sd(estimate, na.rm = TRUE)) %>% 
  left_join(observed_coefs, by = "model") %>% 
  mutate(lower_ci = observed_est - 1.96 * se, 
         upper_ci = observed_est + 1.96 * se
  )


write.csv(x = boot_ses, file = paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_SEs.csv"))



# Parametric comparisons for robustness analyses --------------------------

parametric_no_cluster <- with(df, rdrobust(y = approval,
                                           x = running_variable,
                                           covs = final_rent_inc + renter_pop,
                                           c = 0,
                                           bwselect = "mserd"))

parametric_cluster <- with(df, rdrobust(y = approval,
                                        x = running_variable,
                                        covs = final_rent_inc + renter_pop,
                                        cluster = zip,
                                        c = 0,
                                        bwselect = "mserd"))

tidy(parametric_no_cluster)
tidy(parametric_cluster)

parametric_ses <- 
  data.frame(
    approach = c("Robust", "Cluster robust"), 
    se = c(parametric_no_cluster %>% tidy() %>% filter(term == "Robust") %>% pull(std.error),
           parametric_cluster %>% tidy() %>% filter(term == "Robust") %>% pull(std.error))
  )

write.csv(x = parametric_ses, file = paste0(BASE_DIR, "2305_intermediate_data/RDD_parametric_ses.csv"))










