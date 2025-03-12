######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Various robustness checks.
#---------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(logr)
library(estimatr)
library(here)
library(readxl)
library(rdrobust)

source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))
options(scipen = 999)

## read in data 
app_analytic_i <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))
app_analytic_i_wfsp <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel_wfsprobust.csv"))

# Effect of FSP among yes
rural_effect <- lm_robust(approval ~ fsp + final_rent_inc + renter_pop + suppressed,
                          data = app_analytic_i %>% filter(rural),
                          clusters = zip,
                          se_type = "CR2",
                          weights = app_analytic_i %>% filter(rural) %>% pull(ipw))

xli_effect <- lm_robust(approval ~ fsp + final_rent_inc + renter_pop + suppressed,
                          data = app_analytic_i %>% filter(x_low_income),
                          clusters = zip,
                          se_type = "CR2",
                          weights = app_analytic_i %>% filter(x_low_income) %>% pull(ipw))

poc_effect <- lm_robust(approval ~ fsp + final_rent_inc + renter_pop + suppressed,
                        data = app_analytic_i %>% filter(poc),
                        clusters = zip,
                        se_type = "CR2",
                        weights = app_analytic_i %>% filter(poc) %>% pull(ipw))


# Composition of those impacted by FSP
app_analytic_i_approved <- app_analytic_i %>% filter(approval)
app_analytic_i_notapproved <- app_analytic_i %>% filter(!approval)
lowinc_fsp_wt <- lm_robust(x_low_income ~ fsp, 
                  data = app_analytic_i_approved, 
                  clusters = zip,
                  se_type = "CR2",
                  weights = ipw)
lowinc_fsp_wt_covars <- lm_robust(x_low_income ~ fsp + final_rent_inc + renter_pop + suppressed, 
                           data = app_analytic_i_approved, 
                           clusters = zip,
                           se_type = "CR2",
                           weights = ipw)
lowinc_fsp_nowt <- lm_robust(x_low_income ~ fsp, 
                           data = app_analytic_i_approved, 
                           clusters = zip,
                           se_type = "CR2")
rural_wt <- lm_robust(rural ~ fsp, 
                           data = app_analytic_i_approved, 
                           clusters = zip,
                           se_type = "CR2",
                           weights = ipw)
rural_wt_covars <- lm_robust(rural ~ fsp + final_rent_inc + renter_pop + suppressed, 
                      data = app_analytic_i_approved, 
                      clusters = zip,
                      se_type = "CR2",
                      weights = ipw)
rural_wt_covars_notapproved <- lm_robust(rural ~ fsp + final_rent_inc + renter_pop + suppressed, 
                             data = app_analytic_i_notapproved, 
                             clusters = zip,
                             se_type = "CR2",
                             weights = ipw)
rural_nowt <- lm_robust(rural ~ fsp, 
                      data = app_analytic_i_approved, 
                      clusters = zip,
                      se_type = "CR2")
poc_wt <- lm_robust(poc ~ fsp, 
                      data = app_analytic_i_approved, 
                      clusters = zip,
                      se_type = "CR2",
                      weights = ipw)
poc_wt_covars <- lm_robust(poc ~ fsp + final_rent_inc + renter_pop + suppressed, 
                    data = app_analytic_i_approved, 
                    clusters = zip,
                    se_type = "CR2",
                    weights = ipw)
poc_nowt <- lm_robust(poc ~ fsp, 
                        data = app_analytic_i_approved, 
                        clusters = zip,
                        se_type = "CR2")

lapply(list(lowinc_fsp_wt, lowinc_fsp_wt_covars, lowinc_fsp_nowt, 
            rural_wt, rural_wt_covars, rural_nowt,
            poc_wt, poc_wt_covars, poc_nowt), tidy)

# TA Robustness

# Use alternate indicator of FSP status
approval_IPW_altfsp <- lm_robust(formula = 
                            approval ~  fsp_popmerge + final_rent_inc + 
                            renter_pop + suppressed,
                          clusters = zip, 
                          se_type = "CR2", 
                          data = app_analytic_i_wfsp,
                          weights = ipw)
write.csv(tidy(approval_IPW_altfsp),
          paste0(BASE_DIR, "2305_model_output/ipwresults_robustFSP.csv"),
          row.names = FALSE)


# Control for marketing
approval_IPW <- lm_robust(formula = 
                            approval ~  fsp + final_rent_inc + 
                            renter_pop + suppressed + marketing,
                          clusters = zip, 
                          se_type = "CR2", 
                          data = app_analytic_i,
                          weights = ipw)

write.csv(tidy(approval_IPW),
          paste0(BASE_DIR, "2305_model_output/ipwresults_robustmarketing.csv"),
          row.names = FALSE)

# TA Robustness
# bin median renter income values, add category for suppressed
approval_IPW <- lm_robust(formula = 
                            approval ~  fsp + med_inc_cat + 
                            renter_pop,
                          clusters = zip, 
                          se_type = "CR2", 
                          data = app_analytic_i,
                          weights = ipw)

write.csv(tidy(approval_IPW),
          paste0(BASE_DIR, "2305_model_output/ipwresults_robustincomedecile.csv"),
          row.names = FALSE)

# TA robustness
# different predictions for median renter income 
# vector of suffices
rent_inc_vars <- c("svm",
                   "xgboost",
                   "knn")

# estimate with that model's renter income and ipw 
compare_rentinc <- lapply(rent_inc_vars,
                      function(x) lm_robust(formula = formula(sprintf("approval ~ fsp + final_rent_inc_%s + renter_pop + suppressed", x)),
                                            data = app_analytic_i, 
                                            clusters = zip,
                                            weights = app_analytic_i[[sprintf("ipw_%s", x)]]))

all_tidy_comparer <- lapply(compare_rentinc, function(x) tidy(x) %>% select(term, estimate, std.error, statistic, p.value))
write.csv(all_tidy_comparer[[1]], 
          paste0(BASE_DIR, "2305_model_output/ipwresults_robustlm_svm.csv"),
          row.names = FALSE)
write.csv(all_tidy_comparer[[2]], 
          paste0(BASE_DIR, "2305_model_output/ipwresults_robustlm_xgboost.csv"),
          row.names = FALSE)
write.csv(all_tidy_comparer[[2]], 
          paste0(BASE_DIR, "2305_model_output/ipwresults_robustlm_knn.csv"),
          row.names = FALSE)

