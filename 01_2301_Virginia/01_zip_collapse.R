######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Final Cleaning Code
# 1_zip_collapse.R
# 
# Collapses application data into pre-post 6/10/2021 for RD.
# 
# All steps correspond to 2301 Analysis Plan,
# starting on pg. 12
#---------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(lubridate)
library(tigris)
library(readxl)
library(DescTools)

df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/merged_precollapse.rds")

#----------------------------------------------------
# Step 1: Create N_app_pre and N_app_post

df$post_FSP <- mdy(df$`Submission Date`) >= mdy("06/10/2021")

# One fails to parse, drop NA date
df$`Submission Date`[which(is.na(df$post_FSP))]

df <- df %>% filter(!is.na(post_FSP))

# for each unique Zip code
length(unique(df$`Property Zipcode`))


# -------------------------------------------------------------------------
# Inspecting missingness in key variables

# Missingness in HH size
# There are ~14K apps where we're missing HH size
df$`Household Size` %>% table(useNA = "always")

# Missingness in Zips
# There are 5K apps where we're missing zip
df$`Property Zipcode` %>% is.na() %>% table()
# But we're missing all geographic info for these, 
# so there's not much we can do about it
df %>% filter(`Property Zipcode` %>% is.na) %>% 
  select(starts_with("Property")) %>% 
  is.na() %>% colMeans()

# Missingness in demos
# 30K missing for AMI Bucket
df %>% pull(`AMI Bucket`) %>% table(useNA = "always")
# 15K missing for race
df %>% pull(`Applicant Race`) %>% table(useNA = "always")
# 15K missing for ethnicity
df %>% pull(`Applicant Ethnicity`) %>% table(useNA = "always")
# 15K missing for veteran status
df %>% pull(`Is Applicant Veteran`) %>% table(useNA = "always")
# 15K missing for disability status
df %>% pull(`Does Applicant Have Disability`) %>% table(useNA = "always")

# -----------------------------
# Sample criteria
#
# 1. Household size <= 3
table(!is.na(df$`Household Size` & df$`Household Size` <= 3))

# 2. Submission Date 12/22/2020-12/1/2021
table(mdy(df$`Submission Date`) >= mdy("12/22/2020"))
table(mdy(df$`Submission Date`) <= mdy("12/1/2021"))
table(is.na(df$`Submission Date`))

# write_rds(df, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/cleaned_applications.rds")

# Count applications by period
app_df <- df %>% 
  # Filter to reported HHSize <= 3 and in our time window
  filter(!is.na(`Household Size`), `Household Size` <= 3) %>% 
  filter(mdy(`Submission Date`) >= mdy("12/22/2020")) %>% 
  filter(mdy(`Submission Date`) <= mdy("12/1/2021")) %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  # count in each ZIP, pre-post combination
  group_by(zip, post_FSP) %>% 
  summarise(n_app = n(),
            n_app_ami_0_30 = sum(`AMI Bucket` == "AT OR BELOW 30% AMI", 
                             na.rm = TRUE),
            n_app_ami_31_50 = sum(`AMI Bucket` == "31-50% AMI", 
                             na.rm = TRUE),
            n_app_ami_51_80 = sum(`AMI Bucket` == "51-80% AMI", 
                              na.rm = TRUE),
            n_app_ami_over = sum(`AMI Bucket` == "OVER INCOME", 
                             na.rm = TRUE),
            n_app_white = sum(`Applicant Race` == "WHITE", 
                              na.rm = TRUE),
            n_app_black_or_aa = sum(`Applicant Race` == "BLACK OR AFRICAN-AMERICAN", 
                              na.rm = TRUE),
            n_app_asian = sum(`Applicant Race` == "ASIAN", 
                                    na.rm = TRUE),
            n_app_nhpi = sum(`Applicant Race` == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", 
                                    na.rm = TRUE),
            n_app_aian = sum(`Applicant Race` == "AMERICAN-INDIAN OR ALASKA NATIVE", 
                             na.rm = TRUE),
            n_app_multi = sum(`Applicant Race` == "MULTI-RACIAL", 
                             na.rm = TRUE),
            n_app_latinx = sum(`Applicant Ethnicity` == "HISPANIC OR LATINO", 
                              na.rm = TRUE),
            n_app_not_latinx = sum(`Applicant Ethnicity` == "NON-HISPANIC OR LATINO", 
                               na.rm = TRUE),
            n_app_veteran = sum(`Is Applicant Veteran` == TRUE, 
                                   na.rm = TRUE),
            n_app_disability = sum(`Does Applicant Have Disability` == TRUE, 
                                na.rm = TRUE),
            n_approved = sum(Status == "APPROVED-PAYMENT REQUEST"),
            .groups = "drop") %>% 
  # pivot to calculate change score
  pivot_wider(id_cols = zip,  
              names_from = post_FSP,
              values_from = n_app:n_approved) %>% 
  # Summarise value of 0 indicates missing, so replace with 0
  mutate(zip = as.character(zip)) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
        
app_df <- app_df %>% 
  rename_with(~ ifelse(str_detect(string = .x, pattern = "TRUE"),
                       gsub("TRUE", "post", .x),
                       gsub("FALSE", "pre", .x)),
                       .cols = n_app_TRUE:n_approved_FALSE)
 
#----------------------------------------------------
# Step 2: Merge income (running variable)

# Raw ACS data for running variable
run_df <- read_xls("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_00_raw_data/VA_program_documents/Fact specific proxy data_ACSST5Y2019.S1901_data_with_overlays_2021-05-21T154015 (2).xls")

# Drop first row (descriptions)
run_df[1,]
run_df <- run_df[-1,]

# Count by cutoff, should be 500 treated
table(as.numeric(run_df$S1901_C01_012E) < 66950, useNA = "always")

# Look at NAs, should be 81 suppressed and two max values
run_df$S1901_C01_012E[which(is.na(as.numeric(run_df$S1901_C01_012E)))]

# Clean and prep ACS data above for merge
run_df_clean <- run_df %>% 
  # Better column names
  rename(med_inc = S1901_C01_012E,
         med_inc_moe = S1901_C01_012M,
         zip = NAME...2) %>% 
  select(zip, med_inc, med_inc_moe) %>% 
  # Two values of "250,000+" -- convert to 250,000
  mutate(med_inc = gsub("+", "", med_inc, fixed = TRUE),
         med_inc = gsub(",", "", med_inc, fixed = TRUE),
         med_inc_moe = gsub("+", "", med_inc_moe, fixed = TRUE),
         med_inc_moe = gsub(",", "", med_inc_moe, fixed = TRUE)) %>% 
  # Turn supppressed values ("-") into NAs
  mutate(med_inc = as.numeric(med_inc), 
         med_inc_moe = as.numeric(med_inc_moe))

table(as.numeric(run_df_clean$med_inc) < 66950, useNA = "always")

#----------------------------------------------------
# Some applicants put an invalid zip code
table(app_df$zip %in% run_df_clean$zip, useNA = "always")
setdiff(app_df$zip, run_df_clean$zip) %>% unique()

# But this is a small proportion of overall applications
table(df$`Property Zipcode` %in% run_df_clean$zip)

# Some zip codes did not receive an application
table(run_df_clean$zip %in% app_df$zip)
#----------------------------------------------------

# This code results in any zip code that never received
# an application being dropped, but they should have 
# zeros imputed.
app_df_original <- 
  app_df %>%
  # 1. Merge running variable (run)
  left_join(run_df_clean, by = "zip") %>%
  mutate(running_variable = med_inc - 66950,
         running_variable = -1 * running_variable,
         fsp = ifelse(med_inc <= 66950, 1, 0)) %>%
  # 2. Remove NA ZCTAs
  filter(!is.na(med_inc))

# This merges to run_df_clean, which has the full universe
# of ZCTAs included in the study, and then imputes zeros
app_df <- 
  run_df_clean %>% 
  # Drop those zip codes with no median income estimate
  filter(!is.na(med_inc)) %>% 
  # 1. Merge running variable (run)
  left_join(app_df, by = "zip") %>% 
  # Code NAs of outcome variables to 0
  mutate(across(starts_with("n_app"), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(running_variable = med_inc - 66950,
         running_variable = -1 * running_variable,
         fsp = ifelse(med_inc <= 66950, 1, 0))

table(app_df_original$fsp, useNA = "always")
table(app_df$fsp, useNA = "always")
table(app_df$n_approved_post)
#-----------------------------------------------------
# Drop Chesterfield and Fairfax

# First, identify Chesterfield / Fairfax zips

# Read Census ZCTA-county 2020 crosswalk
# https://www.census.gov/geographies/reference-files/2020/geo/relationship-files.html#zcta
# Raw: https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt

cwalk <- read.delim("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_00_raw_data/Misc/tab20_zcta520_county20_natl.txt", sep = "|", header = TRUE)

# Each record is a county-ZCTA pair
# So take Fairfax and Chesterfield IDs
  # Fairfax 
va_counties <- counties(state = "VA", cb = TRUE)

(counties_to_drop <- 
    va_counties %>% 
  filter(NAMELSAD %in% c("Chesterfield County", "Fairfax County")) %>% 
  pull(GEOID))

rm(va_counties)

zctas_to_drop <- cwalk %>% 
  filter(GEOID_COUNTY_20 %in% counties_to_drop) %>% 
  pull(GEOID_ZCTA5_20)

write_rds(zctas_to_drop, 
          "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/zctas_to_drop.rds")

length(zctas_to_drop)
sum(app_df$zip %in% zctas_to_drop)

# Do reported ZIP codes align with reported counties?
df %>% 
  filter(`Property Zipcode` %in% zctas_to_drop) %>% 
  group_by(`Property City County`) %>% 
  count() %>% 
  arrange(desc(n))

app_df %>% 
  filter(zip %in% zctas_to_drop) %>% 
  pull(n_app_post) %>% sum()

app_df <- app_df %>% 
  filter(!(zip %in% zctas_to_drop))

# -------------------------------------------------------------------------
# Construct processing times variable

# Step 1: construct raw processing time by subtracting submission date from
# most recent status date
proc_times <- 
  df %>% 
  mutate(processing_date_clean = 
           # Get date of most recent status
           most_recent_status_date %>% 
           # Get first 10 characters (YYYY/MM/DD)
           substr(1, 10) %>% 
           # Convert to date format
           mdy(), 
         submission_date_clean = 
           # Get date of submission
           `Submission Date` %>% 
           # Convert to date format
           mdy(), 
         proc_time = processing_date_clean - submission_date_clean)
  
# Step 2: subset to specified criteria:
# - HHs of size 3 or smaller 
# - who applied before deadline of June 10, 2021 and
# - after the implementation of the two-door model on December 22, 2020
# - had outstanding statuses by June 10, 2021
# - do not have negative values for proc_time

#---------------------------------------------------
# First: Household size
# This applies the subsetting to HHs of size 3 or fewer
proc_times_hh_3 <- 
  proc_times %>% 
  filter(`Household Size` <= 3) %>%
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>% 
  filter(proc_time >= 0)

# This does not subset on HH size
proc_times_hh_all <- 
  proc_times %>% 
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>% 
  filter(proc_time >= 0)
#---------------------------------------------------
# Second: Status, all still subset on household size

# This does not subset on status
proc_times_status_all <- 
  proc_times %>% 
  filter(`Household Size` <= 3) %>%
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>% 
  filter(proc_time >= 0)

# This subsets to non-inactive statuses
proc_times_status_non_inactive <- 
  proc_times %>% 
  filter(`Household Size` <= 3) %>%
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>% 
  filter(proc_time >= 0) %>% 
  filter(Status != "INACTIVITY")

# This subsets to non-inactive statuses
proc_times_status_inactive <- 
  proc_times %>% 
  filter(`Household Size` <= 3) %>%
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>% 
  filter(proc_time >= 0) %>% 
  filter(Status == "INACTIVITY")

# This subsets to a pre-approved list of statuses
proc_times_status_list <- 
  proc_times %>% 
  filter(`Household Size` <= 3) %>%
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>% 
  filter(proc_time >= 0) %>% 
  filter(Status %in% toupper(c("APPROVED-PAYMENT REQUEST",
                       "APPROVED PAYMENT- EDD",
                       "DENIED- EDD",
                       "DENIED - ABOVE 150% FMR",
                       "DENIED - AMI OVER INCOME",
                       "DENIED ERA MAX",
                       "NOT ELIGIBLE- DENIED")))

#---------------------------------------------------
  
# Step 3: winsorize 
proc_times_hh_3 <- proc_times_hh_3 %>% 
  mutate(proc_time_win = Winsorize(proc_time))
proc_times_hh_all <- proc_times_hh_all %>% 
  mutate(proc_time_win = Winsorize(proc_time))

proc_times_status_all <- proc_times_status_all %>% 
  mutate(proc_time_win = Winsorize(proc_time))
proc_times_status_non_inactive <- proc_times_status_non_inactive %>% 
  mutate(proc_time_win = Winsorize(proc_time))
proc_times_status_list <- proc_times_status_list %>% 
  mutate(proc_time_win = Winsorize(proc_time))
proc_times_status_inactive <- proc_times_status_inactive %>% 
  mutate(proc_time_win = Winsorize(proc_time))

# Step 4: aggregate
proc_times_hh_3 <- 
  proc_times_hh_3 %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarize(proc_time_hh_3 = proc_time %>% mean(na.rm = TRUE), 
            proc_time_hh_3_win = proc_time_win %>% mean(na.rm = TRUE))

proc_times_hh_all <- 
  proc_times_hh_all %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarize(proc_time_hh_all = proc_time %>% mean(na.rm = TRUE), 
            proc_time_hh_all_win = proc_time_win %>% mean(na.rm = TRUE))

proc_times_status_all <- 
  proc_times_status_all %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarize(proc_time_status_all = proc_time %>% mean(na.rm = TRUE), 
            proc_time_status_all_win = proc_time_win %>% mean(na.rm = TRUE))

proc_times_status_non_inactive <- 
  proc_times_status_non_inactive %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarize(proc_time_status_noninactive = proc_time %>% mean(na.rm = TRUE), 
            proc_time_status_noninactive_win = proc_time_win %>% mean(na.rm = TRUE))

proc_times_status_inactive <- 
  proc_times_status_inactive %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarize(proc_time_status_inactive = proc_time %>% mean(na.rm = TRUE), 
            proc_time_status_inactive_win = proc_time_win %>% mean(na.rm = TRUE))

proc_times_status_list <- 
  proc_times_status_list %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarize(proc_time_status_list = proc_time %>% mean(na.rm = TRUE), 
            proc_time_status_list_win = proc_time_win %>% mean(na.rm = TRUE))

#-----------------------------------------------
# Aggregate Approvals

approval_rates <- 
  df %>% 
  mutate(processing_date_clean = 
           # Get date of most recent status
           most_recent_status_date %>% 
           # Get first 10 characters (YYYY/MM/DD)
           substr(1, 10) %>% 
           # Convert to date format
           mdy(), 
         submission_date_clean = 
           # Get date of submission
           `Submission Date` %>% 
           # Convert to date format
           mdy(), 
         proc_time = processing_date_clean - submission_date_clean) %>% 
    filter(`Household Size` <= 3) %>%
    filter(submission_date_clean >= mdy("12/22/2020")) %>% 
    filter(submission_date_clean < mdy("6/10/2021")) %>% 
    filter(processing_date_clean >= mdy("6/10/2021")) %>% 
    rename(zip = `Property Zipcode`) %>% 
    mutate(zip = as.character(zip)) %>% 
    group_by(zip) %>% 
    summarize(prop_approv = mean(Status == "APPROVED-PAYMENT REQUEST"))

#-----------------------------------------------

# Step 5: merge back with ZCTA data

app_df <- 
  app_df %>% 
  left_join(proc_times_hh_3, by = "zip") %>% 
  left_join(proc_times_hh_all, by = "zip") %>%
  left_join(proc_times_status_all, by = "zip") %>%
  left_join(proc_times_status_non_inactive, by = "zip") %>%
  left_join(proc_times_status_inactive, by = "zip") %>% 
  left_join(proc_times_status_list, by = "zip") %>% 
  left_join(approval_rates, by = "zip")

# Check how many don't merge due to sparseness


# Total payment amounts ---------------------------------------------------

# Total Payment Amount (total_paid_pre, total_paid_post):
# For our exploratory analysis, we sum the total dollar amount paid to all
# applicants with households of size three or fewer in the pre- and post-FSP
# periods, summing all payments across payees.

total_paid_hh_3 <- 
  df %>% 
  # Subset to the appropriate group
  filter(!is.na(`Household Size`), `Household Size` <= 3) %>% 
  filter(mdy(`Submission Date`) >= mdy("12/22/2020")) %>% 
  filter(mdy(`Submission Date`) <= mdy("12/1/2021")) %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarise(
    total_paid_pre = sum(`Payment Amount`[post_FSP == 0], na.rm = TRUE),
    total_paid_post = sum(`Payment Amount`[post_FSP == 1], na.rm = TRUE),
  )

total_paid_hh_all <- 
  df %>% 
  # Subset to the appropriate group
  filter(mdy(`Submission Date`) >= mdy("12/22/2020")) %>% 
  filter(mdy(`Submission Date`) <= mdy("12/1/2021")) %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarise(
    total_paid_pre_hh_all = sum(`Payment Amount`[post_FSP == 0], na.rm = TRUE),
    total_paid_post_hh_all = sum(`Payment Amount`[post_FSP == 1], na.rm = TRUE),
  )


app_df <- 
  app_df %>% 
  left_join(total_paid_hh_3, by = "zip") %>% 
  left_join(total_paid_hh_all, by = "zip")


app_df$total_paid_post[is.na(app_df$total_paid_post)] <- 0
app_df$total_paid_pre[is.na(app_df$total_paid_pre)] <- 0
app_df$total_paid_post_hh_all[is.na(app_df$total_paid_post_hh_all)] <- 0
app_df$total_paid_pre_hh_all[is.na(app_df$total_paid_pre_hh_all)] <- 0

# All applications ignoring hh size ---------------------------------------

n_app_hh_all <- 
  df %>% 
  # Subset to the appropriate group
  filter(mdy(`Submission Date`) >= mdy("12/22/2020")) %>% 
  filter(mdy(`Submission Date`) <= mdy("12/1/2021")) %>% 
  rename(zip = `Property Zipcode`) %>% 
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>% 
  summarise(
    n_app_pre_hh_all = sum(post_FSP == 0, na.rm = TRUE),
    n_app_post_hh_all = sum(post_FSP == 1, na.rm = TRUE),
  )

app_df <- 
  app_df %>% 
  left_join(n_app_hh_all, by = "zip") 

app_df$n_app_pre_hh_all[is.na(app_df$n_app_pre_hh_all)] <- 0
app_df$n_app_post_hh_all[is.na(app_df$n_app_post_hh_all)] <- 0

#-----------------------------------------------------

write_rds(app_df, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds")
write_csv(app_df, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.csv")

#-----------------------------------------------------
# Also prepare individual-level datasets

# Write dataset here for later robustness check across
# time cutoff for individual data
write_rds(df, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/indiv_robustness_time_apps.rds")
write_rds(run_df_clean, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/run_df_clean.rds")

indiv <- df %>%  # applications
  select(application_id, Status, Approved,
         `Household Size`, `Submission Date`, `Payment Amount`,
         zip = `Property Zipcode`,
         most_recent_status_date,
         post_FSP,
         `Applicant Race`,
         `Applicant Ethnicity`,
         `Is Applicant Veteran`,
         `Does Applicant Have Disability`,
         `AMI Bucket`) %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(run_df_clean, by = "zip") %>% 
  mutate(running_variable = med_inc - 66950,
         running_variable = -1 * running_variable,
         fsp = ifelse(med_inc <= 66950, 1, 0)) %>% 
  # still drop applications from other programs
  filter(!(zip %in% zctas_to_drop)) %>%
  # Have to re-create same date variables
  mutate(submission_date_clean = 
           `Submission Date` %>% 
           mdy(),
         processing_date_clean = 
           most_recent_status_date %>% 
           substr(1, 10) %>% 
           mdy()) %>% 
  left_join(proc_times %>% 
              select(application_id, proc_time),
            by = "application_id") %>% 
  filter(`Household Size` <= 3) %>%
  filter(submission_date_clean >= mdy("12/22/2020")) %>% 
  filter(submission_date_clean < mdy("6/10/2021")) %>% 
  filter(processing_date_clean >= mdy("6/10/2021")) %>%
  mutate(proc_time_win = Winsorize(proc_time, na.rm = TRUE))

write_rds(indiv, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/indiv.rds")
