######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Final Cleaning Code
# 0_build_sample.R
# 
# Merges and prepares analytic dataset.
# 
# All steps correspond to 2301 Analysis Plan,
# starting on pg. 11
#---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

######################################################################
# Step 0: Setup, read three datasets
#---------------------------------------------------------------------

# Dataset #1: Tenant Applications Data
app <- read_csv("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_00_raw_data/VA_application_data/Deval Submission Dates.csv")

# confirm number of applications
nrow(app)

# Dataset #2: Application Status
status <- read_csv("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_00_raw_data/VA_application_data/Public_-_Most_Recent_Application_Status_with_Details (OES).csv")

nrow(status)
length(unique(status$application_id))

# Dataset #3: Payment Status
pay <- read_csv("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_00_raw_data/VA_application_data/Paid_Payments_with_Details (OES).csv")

nrow(pay)

######################################################################
# Step 1: Join application status data
#--------------------------------------------

# FLAG: First remove 278 error duplicates
# These applications are reported to have SUBMITTED
# with an additional status update.
# These duplicate SUBMITTED rows are a mistake, 
# and should be removed. 

dup_vec <- app %>% 
  left_join(status, by = "application_id") %>% 
  group_by(application_id) %>% 
  count() %>% filter(n > 1) %>% 
  pull(application_id)

length(dup_vec)

# Within app ID, which columns have different values?
# Only Status, Denied, and Approved
dup_df <- status %>% 
  filter(application_id %in% dup_vec) 

#----------------------------------------------------
# Exploratory look into those duplicates
# Not necessary for re-analysis, but keeping for posterity
dup_df %>% 
  group_by(application_id) %>%
  summarise(subcount = sum(Status == "SUBMITTED")) %>% 
  pull(subcount) %>% table()

write_csv(dup_df, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/dup_status.csv")
#----------------------------------------------------

orig_nrow <- nrow(status)

# Drop 278 mistakenly duplicated rows
status <- status %>% 
  filter(!(application_id %in% dup_vec & Status == "SUBMITTED"))

# Check this process removed 278 rows
stopifnot((orig_nrow - nrow(status)) == 278)

# Confirm all RMRP application IDs are now unique
status %>% 
  filter(application_id %in% app$application_id) %>% 
  group_by(application_id) %>% 
  count()  %>% pull(n) %>% table()

nrow(app)

# Create "merged-application date-status data"
app <- app %>% 
  left_join(status, by = "application_id")

# Check number of applications has not changed
nrow(app)

######################################################################
# Step 2: Join Payments Data
#---------------------------------------------------------------------

# Check overlap
# Not all applications will be paid, as expected
table(app$application_id %in% pay$`Application ID`)

# And not all paid applications come from RMRP, as expected
table(pay$`Application ID` %in% app$application_id)

# Choose relevant payment columns to attach
colnames(pay)

#---------------------------------------------------------------------
# Covariate updating check: make sure covariate values in paid data are same
# 
# Not necessary for re-analysis, but checked while making 
# 
# Covariates for unpaid (FALSE) applications from previous step
# Covariates can be updated for paid (TRUE) applications here, so:
# 
# Break into two steps:
# 1. Split application data into paid and unpaid, 
# merge separately.
# Leave unpaid applications alone.
# Update covariates for paid applications in pay data.
# 2. Recombine paid and unpaid into original size. 

rerun_check <- FALSE

if (rerun_check) {
  
  paid <- app %>% filter(application_id %in% pay$`Application ID`)
  unpaid <- app %>% filter(!(application_id %in% pay$`Application ID`))
  
  # Everyone is still accounted for
  stopifnot(nrow(app)== (nrow(paid) + nrow(unpaid)))
  
  # Add updated covariates
  paid <- paid %>% 
    left_join(pay,
              suffix = c("_app", "_pay"),
              by = c("application_id" = "Application ID"))
  
  # Duplicated column names will have "_app" or "_pay" depending on whether they
  # come from the app or pay datasets
  colnames(paid)
  
  # Make sure duplicate column values are exactly equal
  # Some will be missing due to NAs
  dup_column_vec <- colnames(pay)[colnames(pay) %in% colnames(app)]

  lapply(dup_column_vec, function(x) {
    x_name <- paste0(x, "_app")
    y_name <- paste0(x, "_pay")
    
    x_vals <- paid %>% pull(x_name)
    y_vals <- paid %>% pull(y_name)

    # Standardize capitalization
    if (is_character(x_vals[1])) {
      table(
        toupper(x_vals) == toupper(y_vals)
      )
    } else {
      table(x_vals == y_vals)
    } 
    
  })
  
  # Remove old covariate columns (.x)
  paid <- paid %>% 
    select(-contains("_app"))
  
  colnames(paid) <- gsub(x = colnames(paid), 
                         pattern = "_pay",
                         replacement = "")
  
  # Make column names consistent
  colnames(paid)
  colnames(unpaid)
  
  colnames(unpaid) <- c("application_id", "submission_date", 
                        "FSP", "CE", "ami_bucket", "assistance_type",
                        "grant_admin", "grant_admin_name",
                        "p_city", "p_county", "p_jurisdiction", "p_region_type",
                        "p_big8", "p_zip", "p_state",
                        "gender", "race", "ethnicity",
                        "veteran", "disability", 
                        "hh_size", "most_recent_status", 
                        "Status", "Pending", "Pending_DHCD",
                        "Denied", "Started", "Approved", "Other", "Count",
                        "most_recent_status_date")
  
  colnames(paid) <- c("application_id", "submission_date", 
                      "most_recent_status", 
                      "Status", "Pending", "Pending_DHCD", "Denied", "Started",
                      "Approved", "Other", "Count",
                      "most_recent_status_date", 
                      "grant_admin", "grant_admin_name",
                      "payment_status", "payment_amount", 
                      "payment_date_bucket", "payment_date",
                      "FSP", "CE", "ami_bucket", "assistance_rent",
                      "p_city", "p_county", "p_jurisdiction", "is_ci",
                      "p_region_type", "p_big8", "p_zip", "p_state",
                      "gender", "race", "ethnicity", 
                      "veteran", "disability", "hh_size")
  
  # Make classes consistent
  unpaid$p_zip <- as.character(unpaid$p_zip)
  paid$p_zip <- as.character(paid$p_zip)
  
  analysis_df <- bind_rows(paid, unpaid)
  
  sum(is.na(analysis_df$FSP))
  
  sum(is.na(analysis_df$payment_amount))
}

#---------------------------------------------------------------------
# Attach payment covariates

pay <- pay %>% 
  select(c("Application ID", "Payment Status", 
           "Payment Amount", "Payment Date Bucket", 
           "Payment Date"))

# Check duplicates
pay %>% 
  group_by(`Application ID`) %>% 
  count() %>% 
  pull(n) %>% 
  table()

# They exist, but none in tenant data
pay %>% 
  filter(`Application ID` %in% app$application_id) %>% 
  group_by(`Application ID`) %>% 
  count() %>% 
  pull(n) %>% 
  table()

# Finalize step 2, join payments data
app <- app %>% 
  left_join(pay, by = c("application_id" = "Application ID"))

# Final sample size for merged applications
nrow(app)

write_rds(app, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/merged_precollapse.rds")
write_csv(app, "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/merged_precollapse.csv")
