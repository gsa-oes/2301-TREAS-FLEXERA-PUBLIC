
rm(list = ls())

library(tidyverse)
library(estimatr)
library(rdrobust)
library(broom)
library(knitr)

df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds")

# Source in helper functions
source("00_2301_misc/analysis_helpers.R")


# All households ----------------------------------------------------------

n_app_hh_all_rd <- rdrobust(y = df$n_app_post_hh_all, 
                          x = df$running_variable, 
                          c = 0, 
                         covs = df$n_app_pre_hh_all,
                          bwselect = "mserd")

n_app_hh_all_lm <- lm_robust(
  formula = n_app_post_hh_all ~ fsp + med_inc + n_app_pre_hh_all, 
  data = df) 

n_app_hh_all_rd %>% tidy %>% kable(x = ., format = "simple", digits = 3)
n_app_hh_all_lm %>% tidy %>% kable(x = ., format = "simple", digits = 3)
