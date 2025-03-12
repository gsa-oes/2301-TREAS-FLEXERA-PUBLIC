######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Script to calculate standalone statistics 
#---------------------------------------------------------------------

rm(list = ls())

library(dplyr)

source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))
options(scipen = 999)

app_i <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))
app_wide <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_ziplevel_wide.csv"))

# pg. 1: FSP ZIP code % of total population
sum(app_wide$renter_pop[app_wide$fsp == TRUE]) / sum(app_wide$renter_pop)

# pg. 1: proportion of apps from FSP areas
nrow(app_i[app_i$fsp == TRUE,]) / nrow(app_i)

# pg. 3: application dates received
min(app_i$received_dt)
max(app_i$received_dt)

# pg. 3: % of application from each group
sum(app_i$poc, na.rm = TRUE) / sum(!is.na(app_i$poc))
sum(app_i$rural, na.rm = TRUE) / sum(!is.na(app_i$rural))
sum(app_i$x_low_income, na.rm = TRUE) / sum(!is.na(app_i$x_low_income))
sum(app_i$race == "White", na.rm = TRUE) / sum(!is.na(app_i$race))
sum(app_i$rural, na.rm = TRUE) / sum(!is.na(app_i$rural))
