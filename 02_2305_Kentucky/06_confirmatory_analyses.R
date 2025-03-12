######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Confirmatory analysis code 
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

log_open(here(LOG_DIR, "06_confirmatory_analyses.log"))


######################################################################
# Step 1: read in data from cleaning
#-------------------------------------------------------------------

## read in data 
app_analytic_i <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))
app_analytic_z_wide <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_ziplevel_wide.csv"))

######################################################################
# Outcome: approval rates 
#-------------------------------------------------------------------

# for TA:
# original DiD: applicant level 
approval_DiD <- lm_robust(formula = 
                         approval ~ pre_post + fsp + pre_post*fsp + final_rent_inc + 
                         renter_pop + suppressed,
                       clusters = zip, 
                       se_type = "CR2", 
                       data = app_analytic_i)

summary(approval_DiD)
write.csv(tidy(approval_DiD),
          paste0(BASE_DIR, "2305_model_output/robustdid_results.csv"),
          row.names = FALSE)

tidy(approval_DiD) %>% kableExtra::kable()

log_print(summary(approval_DiD))

#-------------------------------------------------------
# IPW: applicant level 
approval_IPW <- lm_robust(formula = 
                       approval ~  fsp + final_rent_inc + 
                       renter_pop + suppressed,
                     clusters = zip, 
                     se_type = "CR2", 
                     data = app_analytic_i,
                     weights = ipw)

log_print(summary(approval_IPW))

# predicting number of approvals caused by FSP
# 1. Reclassify treatment group data as control FSP
# 2. Predict outcomes under treatment model
# 3. Difference between # observed approvals - # predicted approvals

set.seed(02138)
treat <- app_analytic_i %>% filter(fsp == TRUE)
preds <- predict(approval_IPW, newdata = treat, weights = ipw)
quantile(preds)

n_treat <- sum(treat$approval)

treat$fsp <- FALSE # reclassify
preds <- predict(approval_IPW, newdata = treat, weights = ipw)
quantile(preds)

n_pred_treat <- rbinom(nrow(treat), size = 1, prob = preds) %>% sum()

n_treat - n_pred_treat

# Run 10,000 times to get a sense of uncertainty
test <- tibble(x = 1:10000) %>% 
  mutate(run = map_dbl(x, ~ sum(rbinom(nrow(treat),
                                       size = 1, prob = preds))))

quantile(test$run)

quantile(n_treat - test$run, probs = c(0.025, 0.975))

#-------------------------------------------------------

# RDD at applicant level : see bootstrapping script


# RDD at ZCTA level 
df_zcta_someapp <- app_analytic_z_wide %>% mutate(total_app = n_app_pre + n_app_post) %>%
  filter(total_app > 0)
approval_rdd_zcta <- with(df_zcta_someapp, rdrobust(y = prop_approved,
                                           x = running_variable,
                                           cluster = zip,
                                           c = 0,
                                           bwselect = "mserd",
                                           covs = suppressed + renter_pop,
                                           weight = total_app)) # weight by total app 

tidy_a_rd_z <- tidy.rdrobust(approval_rdd_zcta)
log_print(tidy_a_rd_z)

######################################################################
# Outcome: difference in approval rates by group 
#-------------------------------------------------------------------

## descriptive gaps in control
app_analytic_i %>% filter(!is.na(poc)) %>% group_by(poc, fsp) %>% summarise(mean = mean(approval),
                   weighted.mean = weighted.mean(approval, w = ipw)) %>%
                  arrange(fsp)

app_analytic_i %>% filter(!is.na(x_low_income)) %>% group_by(x_low_income, fsp) %>% summarise(mean = mean(approval),
                                                                            weighted.mean = weighted.mean(approval, w = ipw)) %>%
  arrange(fsp)

app_analytic_i %>% filter(!is.na(rural)) %>% group_by(rural, fsp) %>% summarise(mean = mean(approval),
                                     weighted.mean = weighted.mean(approval, w = ipw)) %>%
  arrange(fsp)



## differential effects
ipw_poc <- lm_robust(formula = 
                       approval ~  fsp*poc + final_rent_inc + 
                       renter_pop + suppressed,
                     clusters = zip, 
                     se_type = "CR2", 
                     data = app_analytic_i %>% filter(!is.na(poc)),
                     weights = ipw)
log_print(summary(ipw_poc))

ipw_xli <- lm_robust(formula = 
                       approval ~  fsp*x_low_income + final_rent_inc + 
                       renter_pop + suppressed,
                     clusters = zip, 
                     se_type = "CR2", 
                     data = app_analytic_i %>% filter(!is.na(x_low_income)),
                     weights = ipw)
log_print(summary(ipw_xli))

ipw_rural <- lm_robust(formula = 
                         approval ~  fsp*rural + final_rent_inc + 
                         renter_pop + suppressed,
                       clusters = zip, 
                       se_type = "CR2", 
                       data = app_analytic_i %>% filter(!is.na(rural)),
                       weights = ipw)
log_print(summary(ipw_rural))



######################################################################
# Outcome: total applications 
#-------------------------------------------------------------------

app_analytic_z_wide <- app_analytic_z_wide %>%
              mutate(total_app = n_app_pre + n_app_post,
                     total_paid = total_paid_pre + total_paid_post)

# for TA:
# ipw
totalapp_ipw <-  lm_robust(formula = 
                  log1p(total_app) ~  fsp + final_rent_inc + 
                      renter_pop + suppressed,
                      data = app_analytic_z_wide,
                      weights = ipw)


write.csv(tidy(totalapp_ipw),
          paste0(BASE_DIR, "2305_model_output/ipwresults_robusttotalapps.csv"),
          row.names = FALSE)

log_print(summary(totalapp_ipw))

# rd 
totalapps_rdd_zcta <- with(app_analytic_z_wide, rdrobust(y = log1p(total_app),
                                           x = running_variable,
                                           cluster = zip,
                                           covs = suppressed + renter_pop,
                                           c = 0,
                                           bwselect = "mserd")) # no warning message

tidy_totalapps_rd_z <- tidy.rdrobust(totalapps_rdd_zcta)
log_print(tidy_totalapps_rd_z)

######################################################################
# Outcome: total paid  
#-------------------------------------------------------------------

# ipw
totalpaid_ipw <-  lm_robust(formula = 
                             log1p(total_paid) ~  fsp + final_rent_inc + 
                             renter_pop + suppressed,
                           data = app_analytic_z_wide,
                           weights = ipw)

log_print(summary(totalpaid_ipw))

# rd 
totalpaid_rdd_zcta <- with(app_analytic_z_wide, rdrobust(y = log1p(total_paid),
                                                         x = running_variable,
                                                         cluster = zip,
                                                         covs = suppressed + renter_pop,
                                                         c = 0,
                                                         bwselect = "mserd")) # no warning message

tidy_totalp_rd_z <- tidy.rdrobust(totalpaid_rdd_zcta)
log_print(tidy_totalp_rd_z)


######################################################################
# Outcome: fraud detection
#-------------------------------------------------------------------

# Payment integrity -------------------------------------------------------

# Look at likelihood that FSP increased the likelihood that an application was
# denied due to suspected fraud
app_analytic_i$app_fraud <- app_analytic_i$status == "Applications Denied - Suspected Fraud" 


fraud_ipw <- lm_robust(formula = 
                       app_fraud ~  fsp + final_rent_inc + renter_pop + suppressed,
                     clusters = zip, 
                     se_type = "CR2", 
                     data = app_analytic_i,
                     weights = ipw)





######################################################################
# Write the results for plotting 
#-------------------------------------------------------------------

# approval 
ipw_a <- data.frame(model = "IPTW-weighted regression",
                          outcome = "Approval",
                          unit = "Applicant",
                          est = approval_IPW$coefficients["fspTRUE"],
                          ci_lower = approval_IPW$conf.low["fspTRUE"],
                          ci_upper = approval_IPW$conf.high["fspTRUE"])
rdd_a_z <- data.frame(model = "RD: zip-level",
                outcome = "Approval",
                unit = "Zip",
                est = tidy_a_rd_z[tidy_a_rd_z$term == "Robust", "estimate"],
                ci_lower = tidy_a_rd_z[tidy_a_rd_z$term == "Robust", "conf.low"],
                ci_upper = tidy_a_rd_z[tidy_a_rd_z$term == "Robust", "conf.high"])
did_a <- data.frame(model = "Difference-in-differences",
                   outcome= "Approval",
                   unit = "Applicant",
                   est = approval_DiD$coefficients["pre_postTRUE:fspTRUE"],
                   ci_lower = approval_DiD$conf.low["pre_postTRUE:fspTRUE"],
                   ci_upper =  approval_DiD$conf.high["pre_postTRUE:fspTRUE"])
  
# het effects 
ipw_rural_summary <- data.frame(model = "IPTW-weighted regression: rural",
                    outcome = "Approval: heterogeneous effects",
                    unit = "Applicant",
                    est = ipw_rural$coefficients["fspTRUE:ruralTRUE"],
                    ci_lower = ipw_rural$conf.low["fspTRUE:ruralTRUE"],
                    ci_upper = ipw_rural$conf.high["fspTRUE:ruralTRUE"])
ipw_poc_summary <- data.frame(model = "IPTW-weighted regression: poc",
                        outcome = "Approval: heterogeneous effects",
                        unit = "Applicant",
                        est = ipw_poc$coefficients["fspTRUE:pocTRUE"],
                        ci_lower = ipw_poc$conf.low["fspTRUE:pocTRUE"],
                        ci_upper = ipw_poc$conf.high["fspTRUE:pocTRUE"])
ipw_xli_summary <- data.frame(model = "IPTW-weighted regression: poc",
                      outcome = "Approval: heterogeneous effects",
                      unit = "Applicant",
                      est = ipw_xli$coefficients["fspTRUE:x_low_incomeTRUE"],
                      ci_lower = ipw_xli$conf.low["fspTRUE:x_low_incomeTRUE"],
                      ci_upper = ipw_xli$conf.high["fspTRUE:x_low_incomeTRUE"])

# total apps 
ipw_totalapp <- data.frame(model = "IPTW-weighted regression",
                    outcome = "Total applications",
                    unit = "Zip",
                    est = totalapp_ipw$coefficients["fspTRUE"],
                    ci_lower = totalapp_ipw$conf.low["fspTRUE"],
                    ci_upper = totalapp_ipw$conf.high["fspTRUE"])
rdd_totalapp_z <- data.frame(model = "RD: zip-level",
                      outcome = "Total applications",
                      unit = "Zip",
                      est = tidy_totalapps_rd_z[tidy_totalapps_rd_z$term == "Robust", "estimate"],
                      ci_lower = tidy_totalapps_rd_z[tidy_totalapps_rd_z$term == "Robust", "conf.low"],
                      ci_upper = tidy_totalapps_rd_z[tidy_totalapps_rd_z$term == "Robust", "conf.high"])

# total paid 
ipw_totalpaid <- data.frame(model = "IPTW-weighted regression",
                           outcome = "Total paid",
                           unit = "Zip",
                           est = totalpaid_ipw$coefficients["fspTRUE"],
                           ci_lower = totalpaid_ipw$conf.low["fspTRUE"],
                           ci_upper = totalpaid_ipw$conf.high["fspTRUE"])
rdd_totalpaid_z <- data.frame(model = "RD: zip-level",
                             outcome = "Total paid",
                             unit = "Zip",
                             est = tidy_totalp_rd_z[tidy_totalp_rd_z$term == "Robust", "estimate"],
                             ci_lower = tidy_totalp_rd_z[tidy_totalp_rd_z$term == "Robust", "conf.low"],
                             ci_upper = tidy_totalp_rd_z[tidy_totalp_rd_z$term == "Robust", "conf.high"])

# fraud detection

fraud_ipw_table <- 
  tidy(fraud_ipw) %>% 
  filter(term == "fspTRUE") %>% 
  mutate(model = "IPTW-weighted regression: application-level", 
         outcome = "Fraud detection",
         unit = "Applicant") %>% 
  select(model, outcome, unit, est = estimate, 
         ci_lower = conf.low, ci_upper = conf.high)


# rowbind
all_results <- do.call(rbind.data.frame,
                       list(ipw_a, ipw_totalapp, ipw_totalpaid, did_a, 
                            ipw_rural_summary, ipw_poc_summary, ipw_xli_summary,
                            rdd_a_z, rdd_totalapp_z, rdd_totalpaid_z, 
                            fraud_ipw_table))

write.csv(all_results,
          paste0(BASE_DIR, "2305_intermediate_data/summaries_forgraph_ipw_zlevelRDD.csv"),
          row.names = FALSE)
