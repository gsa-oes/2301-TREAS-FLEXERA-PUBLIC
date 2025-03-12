######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Final Confirmatory Analysis Code
# 2_confirmatory.R
# 
# Runs confirmatory analyses: (1) RD and (2) AFC
# Runs same code for exploratory analyses on demographic subgroups.
# 
# All steps correspond to 2301 Analysis Plan,
# starting on pg. 16
#---------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(rdrobust)
library(estimatr)
library(broom)
library(kableExtra)

# Source in helper functions
source("00_2301_misc/analysis_helpers.R")

df <- readRDS("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds")

df$log_app_post <- log1p(df$n_app_post)
df$log_app_pre  <- log1p(df$n_app_pre)

rd_fit <- with(df, rdrobust(y = log_app_post,
                            x = running_variable,
                            c = 0,
                            covs = log_app_pre, bwselect = "mserd"))

tidy(rd_fit)

rdplot(y = df$log_app_post, x = df$running_variable, c = 0, 
                covs = df$n_app_pre)

lm_fit <- lm_robust(formula = log_app_post ~ fsp + log_app_pre + med_inc, data = df)
summary(lm_fit)

# save models for coef plot
t1 <- tidy(rd_fit) %>% mutate(model = "RD")
t2 <- tidy(lm_fit) %>% mutate(model = "AFC")

mod_napps <- bind_rows(t1, t2) %>% 
  mutate(outcome = "log_app_post")
writeRDS(mod_napps, "D:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/log_napps.rds")

#----------------------------------------------
# Exploratory Analysis:
# Applications from Underserved Groups 
# 
# Same two confirmatory analyses for:
# 1. AMI between 0 and 30%, 31-50%, and 51-80%.
# 2. Identify as: Black, Asian, Hawaiian/Pacific Islander, Native American, or multi-racial. 
# 3. Identify as Hispanic or Latino.
# 4. Identify as women, transgender, or non-binary. 
# 5. veterans, disabled.


# get unique group labels
group_names <- colnames(df)[6:33]

(group_names <- gsub("n_app_", "", group_names) %>% 
  gsub("_pre|_post", "", .) %>% unique())

group_names <- c("", group_names)

# read zctas for dropping below
zctas_to_drop <- readRDS("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/zctas_to_drop.rds")
all_apps <- readRDS("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/cleaned_applications.rds")

all_apps <- all_apps %>% 
  mutate(`Applicant Race` = ifelse(is.na(`Applicant Race`), "Race Not Reported", `Applicant Race`),
         `Is Applicant Veteran` = ifelse(is.na(`Is Applicant Veteran`), "Veteran Not Reported", `Is Applicant Veteran`),
         `Does Applicant Have Disability` = ifelse(is.na(`Does Applicant Have Disability`), "Disability Not Reported", `Does Applicant Have Disability`),
         `Applicant Ethnicity` = ifelse(is.na(`Applicant Ethnicity`), "Ethnicity Not Reported", `Applicant Ethnicity`))

#-----------------------------------------------------
# loop through these IDs and run group effects for
# total paid and application counts



subgroup_afc <- function(column, group) {
  
  cli::cli_alert_info(glue::glue("Running group: ", group, "\n"))
  
  # Just to avoid accidental filters
  samp_apps <- all_apps
  
  #--------------------------------------------
  # Start with Application Count
  app_vars <- samp_apps %>% 
    # Filter to reported HHSize <= 3 and in our time window
    filter(!is.na(`Household Size`), `Household Size` <= 3) %>% 
    filter(mdy(`Submission Date`) >= mdy("12/22/2020")) %>% 
    filter(mdy(`Submission Date`) <= mdy("12/1/2021")) %>% 
    rename(zip = `Property Zipcode`) %>% 
    mutate(zip = as.character(zip)) %>%
    filter(!(zip %in% zctas_to_drop)) %>% 
    # filter to group of interest
    filter(get(column) == group) %>% 
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
  
  #-------------------------------------------
  bind_rows(afc_napp,
            afc_paid) %>% 
    mutate(column = column,
           group = group)
}

run_subgroup_effects <- function(colname) {
  
  this_df <- tibble(column = colname, 
                    group = all_apps %>% pull(colname) %>% unique()) %>% 
    filter(group != "Race Not Reported",
           group != "Disability Not Reported",
           group != "Veteran Not Reported",
           group != "Ethnicity Not Reported",
           !is.na(group))
  
  cli::cli_alert_info(glue::glue("Starting ", colname, " subgroup analyses."))
  res <- pmap(this_df, subgroup_afc)
  cli::cli_alert_success("Done!")
  res
}

hetero_zip_level <- map(c("Applicant Race",
                          "Applicant Ethnicity",
      "AMI Bucket",
      "Does Applicant Have Disability",
      "Is Applicant Veteran"),
    .f = run_subgroup_effects)

writeRDS(hetero_zip_level, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/hetero_zip_level.rds")
#------------------------------------------------------------------

df$log_approved_post <- log1p(df$n_approved_post)
df$log_approved_pre  <- log1p(df$n_approved_pre)

rd_fit <- with(df, rdrobust(y = log_approved_post,
                            x = running_variable,
                            c = 0,
                            covs = log_approved_pre, bwselect = "mserd"))

tidy(rd_fit)

#----------------------------------------------

rd_fit_approval <- rd_fit <- with(df, rdrobust(y = prop_approv,
                                               x = running_variable,
                                               c = 0,
                                               bwselect = "mserd"))

lm_fit_approval <- lm_robust(prop_approv ~ fsp  + med_inc,
                    data = df)

t1 <- tidy(rd_fit_approval) %>% mutate(model = "RD")
t2 <- tidy(lm_fit_approval) %>% mutate(model = "AFC")

mod_prop <- bind_rows(t1, t2) %>% 
  mutate(outcome = "approval_prop")
write_rds(mod_prop, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/prop_approval.rds")



# Marginal effects --------------------------------------------------------


# Pull out polynomial predictions from the model used for the rdplot
poly_preds <- rdplot(y = df$log_app_post, 
                     x = df$running_variable, 
                     c = 0, 
                     covs = df$log_app_pre)$vars_poly
# Pull the polynomial predictions at the threshold (first is control, second treatment)
poly_preds <- poly_preds$rdplot_y[poly_preds$rdplot_x == 0]
# Exponentiate and subtract 1
poly_preds <- exp(poly_preds) - 1
# Compute predicted difference
poly_preds[2] - poly_preds[1]

# Compare predicted means to raw means
df %>% 
  filter(fsp == 0) %>%
  pull(n_app_post) %>% 
  mean()

df$running_variable

# For AFC, use predict function
preds <- predict(lm_fit, 
                 newdata = data.frame(fsp = c(1,0), 
                                      med_inc = rep(66950, 2), 
                                      log_app_pre = rep(mean(df$log_app_pre),2)))
# Exponentiate
preds <- exp(preds) - 1
# Compute predicted difference
preds[1] - preds[2]


