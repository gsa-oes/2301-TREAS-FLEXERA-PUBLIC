######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Run individual-level models.
# 6_individual.R

#---------------------------------------------------------------------

library(estimatr)
library(sandwich)
library(lmtest)
library(rdrobust)

indiv <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/indiv.rds")


m1 <- lm_robust(proc_time ~ fsp + med_inc,
                clusters = zip,
                data = indiv)

summary(m1)

m2 <- lm_robust(proc_time_win ~ fsp + med_inc,
                clusters = zip,
                data = indiv)

rd_proc_time_ind <- with(indiv, rdrobust(y = proc_time_win,
                                         x = running_variable,
                                         c = 0, bwselect = "mserd",
                                         cluster = zip))

source("00_2301_misc/analysis_helpers.R")

t1 <- tidy(m2) %>% mutate(model = "AFC")
t2 <- tidy(rd_proc_time_ind) %>% mutate(model = "RD")

bind_rows(t1, t2) %>% 
  mutate(outcome = "proc_time_individual") %>% 
  write_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/proc_time_individ.rds")

#------------------------------------------------
table(indiv$Status)

# Simple, binary approval outcome
indiv$approved <- ifelse(indiv$Status == "APPROVED-PAYMENT REQUEST",
                         1, 0)

indiv$paid0 <- ifelse(is.na(indiv$`Payment Amount`), 0, 1)

m3 <- lm_robust(approved ~ fsp + med_inc,
                clusters = zip,
                data = indiv)

summary(m3)

lm_approve <- m3

oesr::oes_prep(m3, treatment_vars = "fsp")

m3p <- lm_robust(paid0 ~ fsp + med_inc,
          clusters = zip,
          data = indiv)

summary(m3p)

#---------------------------------------------

m4 <- glm(approved ~ fsp + med_inc,
          data = indiv,
          family = binomial)

summary(m4)

coeftest(m4,
         vcov. = vcovCL(m4,
                        cluster = indiv$zip,
                        type = "HC0"))

m4 <- glm(paid0 ~ fsp + med_inc,
          data = indiv,
          family = binomial)

summary(m4)

coeftest(m4,
         vcov. = vcovCL(m4,
                        cluster = indiv$zip,
                        type = "HC0"))

#------------------------------------

rd_fit <- with(indiv, rdrobust(y = approved,
                               x = running_variable,
                               c = 0, bwselect = "mserd"))

tidy(rd_fit)

# save models for coef plot
t1 <- tidy(rd_fit) %>% mutate(model = "RD")
t2 <- tidy(lm_approve) %>% mutate(model = "AFC")

mod_approval <- bind_rows(t1, t2) %>% 
  mutate(outcome = "approval")

write_rds(mod_approval, "~/Google Drive/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/approval.rds")


# try clustering
rd_fit <- with(indiv, rdrobust(y = paid0,
                               x = running_variable,
                               c = 0, bwselect = "mserd",
                               cluster = zip))

tidy(rd_fit)

#-----------------------------------------------------
# Heterogeneity at individual level

subgroup_afc <- function(column, group) {
  
  cli::cli_alert_info(glue::glue("Running group: ", group, "\n"))
  
  # Just to avoid accidental filters
  samp_apps <- indiv
  
  #--------------------------------------------
  # Start with Application Count
  app_vars <- samp_apps %>% 
    # filter to group of interest
    filter(get(column) == group)
  
  #-------------------------------------------
  # AFC
  afc_proc <- lm_robust(proc_time_win ~ fsp + med_inc,
                  clusters = zip,
                  data = app_vars) %>% 
    tidy() %>% 
    filter(term == "fsp") %>% 
    mutate(model = "Proc Time")
  
  afc_approv <- lm_robust(approved ~ fsp + med_inc,
                  clusters = zip,
                  data = app_vars) %>% 
    tidy() %>% 
    filter(term == "fsp") %>% 
    mutate(model = "Approval")
  
  #-------------------------------------------
  bind_rows(afc_proc,
            afc_approv) %>% 
    mutate(column = column,
           group = as.character(group))
}

run_subgroup_effects <- function(colname) {
  
  this_df <- tibble(column = colname, 
                    group = indiv %>% pull(colname) %>% unique()) %>% 
    filter(group != "Race Not Reported",
           group != "Disability Not Reported",
           group != "Veteran Not Reported",
           !is.na(group))
  
  cli::cli_alert_info(glue::glue("Starting ", colname, " subgroup analyses."))
  res <- pmap(this_df, subgroup_afc)
  cli::cli_alert_success("Done!")
  res
}

hetero_indiv_level <- map(c("Applicant Race",
                          "AMI Bucket",
                          "Does Applicant Have Disability",
                          "Is Applicant Veteran"),
                        .f = run_subgroup_effects)

write_rds(hetero_indiv_level, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/hetero_indiv_level.rds")
