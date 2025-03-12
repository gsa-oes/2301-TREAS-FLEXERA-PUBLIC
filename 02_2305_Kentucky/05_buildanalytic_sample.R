######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Final Cleaning Code
# 
# Merges and prepares analytic dataset.
#---------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(readxl)
library(tigris)
library(ggplot2)
library(data.table)
library(sf)
library(here)
library(rpart)
library(rpart.plot)

source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))

######################################################################
# Step 0: Read in data 
#---------------------------------------------------------------------

# Dataset #1: Tenant Applications Data
app <- read_excel(paste0(BASE_DIR, APP_DATA), range = "A1:R83454")


# Clean colnames 
new_names <- gsub("\\s+", "_", tolower(colnames(app)))
colnames(app) <- new_names

# Do some descriptives regarding duplication
# we deal with duplicated claims later
sprintf("There are %s rows and %s claim ids",
        nrow(app), 
        length(unique(app$claim_id)))

# Dataset #3: marketing indicator
# sep campaign: read txt file and get zips / remove header line
campaign_sep <- readLines(paste0(BASE_DIR, "2305_raw_data/marketing/Targeted Zip Codes.txt"))
campaign_sep_zips <- grep("[[:digit:]]+", campaign_sep, value = TRUE)

# july campaign: read txt file and get zips/remove headers 
# incomplete line for first one but seems fine
campaign_jul <- readLines(paste0(BASE_DIR, "2305_raw_data/marketing/Zip Codes for HHERF Marketing.txt"))
campaign_jul_zips <- grep("[[:digit:]]+", campaign_jul, value = TRUE) 

# december campaign
campaign_dec <- read_xlsx(paste0(BASE_DIR, "2305_raw_data/marketing/Wave 3 Zip Codes.xlsx"))
campaign_dec_zips <- campaign_dec$`Zip Codes`

## some zip codes overlap - for now, i did hierarchical coding based on
## earliest campaign
length(intersect(campaign_sep_zips, campaign_jul_zips))
length(intersect(campaign_sep_zips, campaign_dec_zips))

# Dataset #4: zip code-places mapping to identify exclusion areas
# this refers to KY using its state-level fips code (21)
ky_zips <- readRDS(paste0(BASE_DIR, "2305_raw_data/ACS/zctas_FIP21.RDS"))
zip_zcta <- read_excel(paste0(BASE_DIR, "2305_raw_data/rurality/ZIPCodetoZCTACrosswalk2021UDS.xlsx"))
ky_places <- readRDS(paste0(BASE_DIR, "2305_raw_data/ACS/places_FIP21.RDS"))
ky_counties <- readRDS(paste0(BASE_DIR, "2305_raw_data/ACS/counties_FIP21.RDS"))

# Dataset #5: merged data created in script 05
zcta_txindicator <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/zlevel_df_merged.csv")) %>%
              mutate(zcta_id = as.character(zcta_id))


# Dataset #6: renter income and MOE for weights construction
acs_income_zips <- readRDS(paste0(BASE_DIR, "2305_raw_data/ACS/acs_income_zips.RDS"))
rent_ests <- acs_income_zips %>% 
  filter(variable == "S2503_C05_013")



######################################################################
# Step 1: Create variables in the individual-level application data
#--------------------------------------------

app <- app %>%
  mutate(
    approval = !is.na(approved_amount), 
    
    missing_x_low_income = case_when(ami_info == "No Match" | is.na(ami_info) ~ TRUE, 
                                     .default = FALSE),
    missing_yearlyinc = case_when(is.na(yearly_income) ~ TRUE, 
                                  .default = FALSE),
    x_low_income = case_when(ami_info == "<30%AMI" ~TRUE,
                             missing_x_low_income ~ NA,
                             .default = FALSE),
    missing_poc = case_when(race == "Refuse to Answer" | 
                              is.na(race) | 
                              ethnicity == "Refuse to Answer" | 
                              is.na(ethnicity) ~ TRUE, 
                            .default = FALSE),
    poc = case_when(!missing_poc & 
                    race == "White" &
                    ethnicity == "Non-Hispanic/Non-Latino" ~ FALSE, 
                    !missing_poc ~ TRUE, 
                    .default = NA),
    zip = as.character(zip), # converting to character format
    missing_zip = is.na(zip),
    missing_rural = missing_zip,
    missing_receivedt = is.na(received_dt),
    mark_campaign_jul = case_when(zip %in% campaign_jul_zips & 
                                    received_dt >= as.POSIXct("2021-07-01") ~ TRUE, 
                                  .default = FALSE),
    mark_campaign_sep = case_when(zip %in% campaign_sep_zips &
                                    received_dt >= as.POSIXct("2021-09-01") ~ TRUE, 
                                  .default = FALSE),
    mark_campaign_dec =  case_when(zip %in% campaign_dec_zips &
                                     received_dt >= as.POSIXct("2021-12-01") ~ TRUE, 
                                   .default = FALSE),
    # marketing flag ---since zips not exclusive to one campaign,
    # codes it to the first campaign observed between july, sep, dec
    marketing = case_when(mark_campaign_jul ~ mark_campaign_jul,
                          mark_campaign_sep ~ mark_campaign_sep, 
                          mark_campaign_dec ~ mark_campaign_dec,
                          .default = FALSE),
    days_to_payment = case_when(is.na(payment_date) ~ NA_real_,
                                .default = as.numeric(difftime(payment_date, received_dt, 
                                                               units = "days"))),
    pre_post = case_when(is.na(received_dt) ~ NA,
                         received_dt >= as.POSIXct("2021-06-01") ~ TRUE, # after is 1 or true
                         received_dt < as.POSIXct("2021-06-01") ~ FALSE), # before is 0 or false
    inc_doc_sub_initial = case_when(
      is.na(income_doc_submitted) ~ FALSE,
      income_doc_submitted == "Tenant cannot provide complete income documentation" ~ FALSE,
      income_doc_submitted == "Income Eligible by Fact Proxy" ~ FALSE,
      .default = TRUE
    ), 
    inc_doc_verified = case_when(
      is.na(yearly_income) ~ FALSE, 
      .default = TRUE
    )) 



######################################################################
# Step 2: construct analytic sample of ZCTAs by excluding ones from 
# excluded counties but including ones with zero applications 
#--------------------------------------------

ky_zips$in_lex <-  st_overlaps(ky_zips, ky_places %>% filter(NAME == "Lexington-Fayette")) %>% lengths() > 0
lex_zips <-  ky_zips$ZCTA5CE10[ky_zips$in_lex]


zcta_includedzips <- zcta_txindicator %>%
          filter(!zcta_id %in% lex_zips)


######################################################################
# Step 3: IPTW weights 
#--------------------------------------------

################## Step 1: add rent and moe estimates and use empirical cdf

# left join onto other data and clean
zcta_touse <- zcta_includedzips %>%
  left_join(rent_ests, by = c("zcta_id" = "GEOID")) %>%
  mutate(S = moe/1.645, 
         # Remove NAs from S by mean-imputation 
         S = ifelse(is.na(S), sqrt(mean(S^2, na.rm = TRUE)), S))

zcta_touse$empirical_cdf <- pnorm(q = zcta_touse$county_ami_touse, mean = zcta_touse$final_rent_inc, 
                      sd = zcta_touse$S)

zcta_touse$empirical_cdf_svm <- pnorm(q = zcta_touse$county_ami_touse, mean = zcta_touse$final_rent_inc_svm, 
                                  sd = zcta_touse$S)
zcta_touse$empirical_cdf_xgboost <- pnorm(q = zcta_touse$county_ami_touse, mean = zcta_touse$final_rent_inc_xgboost, 
                                      sd = zcta_touse$S)
zcta_touse$empirical_cdf_knn <- pnorm(q = zcta_touse$county_ami_touse, mean = zcta_touse$final_rent_inc_knn, 
                                      sd = zcta_touse$S)

################## Step 2: decision tree for probability of suppression

set.seed(91988)
n_train <- round(nrow(zcta_touse)*0.8)
id_train <- zcta_touse %>% 
  group_by(suppressed) %>%
  slice_sample(prop = 0.8) %>%
  ungroup() %>%
  pull(zcta_id) 
id_test <- setdiff(zcta_touse$zcta_id, id_train)

## model estimated on training data 
dt_fit <- rpart(suppressed ~ renter_pop,
                data = zcta_touse %>% filter(zcta_id %in% id_train),
                method = 'class') 
rpart.plot(dt_fit, extra = 106, digits = 4)


## construct test data and out of sample prediction
test_df <- zcta_touse %>% filter(zcta_id %in% id_test)
test_df_wpred <- cbind.data.frame(test_df, prob_dt = predict(dt_fit, 
                                                             newdata = test_df,
                                                             type = "prob"), 
                                  class_dt = predict(dt_fit, 
                                                     newdata = test_df,
                                                     type = "class"))

## confusion matrices
table(test_df_wpred$class_dt, test_df_wpred$suppressed, dnn= list("Predicted Suppression", "Actual Suppression"))



################## Step 3: add back predictions

zcta_wall <- cbind.data.frame(zcta_touse,
            phat_suppress = predict(dt_fit, newdata = zcta_touse, type = "prob")[, "TRUE"])

zcta_wall <- zcta_wall %>%
  mutate(exclude_cdf = ifelse(empirical_cdf == 1 | empirical_cdf == 0, TRUE, FALSE),
         q = empirical_cdf * (1-phat_suppress),
         exclude_q = ifelse(q == 1 | q == 0, TRUE, FALSE),
         ipw = get_ipw(q, fsp),
         ipw_svm = get_ipw(empirical_cdf_svm * (1-phat_suppress), fsp),
         ipw_xgboost = get_ipw(empirical_cdf_xgboost * (1-phat_suppress), fsp),
         ipw_knn = get_ipw(empirical_cdf_knn * (1-phat_suppress), fsp))


######################################################################
# Step 5: merge treatment indicator / some controls created in previous script
#--------------------------------------------

app_zips_notfound <- setdiff(unique(app$zip[!app$missing_zip]), 
                           zcta_wall$zcta_id)

## separate apps into two categories:
## (1) ones that can be merged based on current zcta id
## (2) ones that cannot
## note that this excludes applications missing a zip since we need to discard
## anyways 
app_found <- app %>% filter(zip %in% zcta_wall$zcta_id) %>% mutate(zip_status = "zcta_id")
app_notfound <- app %>% filter(!zip %in% zcta_wall$zcta_id) 

## for ones not found, get the zcta id from a crosswalk
## and write over using that id 
app_zips_incwalk <- intersect(app_notfound$zip, zip_zcta$ZIP_CODE)
app_notfound_cwalk <- app_notfound %>%
                    left_join(zip_zcta %>% select(ZIP_CODE, ZCTA),
                              by = c("zip" = "ZIP_CODE")) %>%
                    mutate(zip = ZCTA, zip_status = "zipcode") %>%
                    select(-ZCTA)

## rowbind again
app_tomergetx <- rbind.data.frame(app_found, app_notfound_cwalk)

## left join with tx - 
app_wtx <- app_tomergetx %>%
          left_join(zcta_wall, 
                    by = c("zip" = "zcta_id"))


######################################################################
# Step 6: filter to analytic sample 
#--------------------------------------------


## 1. Applications from jefferson and fayette if within a given time window 
## and from lexington if anytime

ky_zips$in_jefferson <- st_overlaps(ky_zips, ky_counties %>% filter(NAME == "Jefferson")) %>% lengths() > 0
ky_zips$in_fayette <- st_overlaps(ky_zips, ky_counties %>% filter(NAME == "Fayette")) %>% lengths() > 0
jefferson_zips <- ky_zips$ZCTA5CE10[ky_zips$in_jefferson]
fayette_zips <- ky_zips$ZCTA5CE10[ky_zips$in_fayette]

# Lexington ran its own program the entire time, so exclude
# all apps from there regardless of app date
app_wtx <- app_wtx %>%
  mutate(exclude_lexington = zip %in% lex_zips,
         state_admin = received_dt >= as.POSIXct("2022-05-01") & received_dt <= as.POSIXct("2022-12-22"),
         exclude_jefferson = case_when(zip %in% jefferson_zips &
                                         state_admin ~ FALSE, 
                                       zip %in% jefferson_zips ~ TRUE, 
                                       .default = FALSE),
         exclude_fayette = case_when(zip %in% fayette_zips &
                                      state_admin ~ FALSE, 
                                     zip %in% fayette_zips ~ TRUE, 
                                     .default = FALSE))

## 2. Drop duplicates at random
app_wtx$row_id <- rownames(app_wtx) # create row id to identify distinct rows (since claim id not distinct)
set.seed(1234)
duplicate_apps_to_drop <- app_wtx %>%
  group_by(claim_id) %>% 
  filter(n() > 1) %>%
  mutate(ran_draw = runif(n()), 
         keep = max(ran_draw) == ran_draw) %>% 
  ungroup() %>% 
  filter(!keep) %>% 
  pull(row_id)

app_nodup <- app_wtx %>%
  filter(!row_id %in% duplicate_apps_to_drop) # keeps all non-dup ids and the one id from each duplicate

stopifnot(nrow(app_nodup) == length(app_nodup$claim_id))


######################################################################
# Step 7: aggregate to zip code level, add zip-level variables, and write individual-level 
# and zip-level files 
#--------------------------------------------

## individual-level data
app_analytic_i <- app_nodup %>%
  filter(!exclude_lexington & !exclude_jefferson & !exclude_fayette & !is.na(received_dt) &
           !is.na(zip))

## aggregate to zctas 
## (1) among zips with 1+ application, group by zip 
## and sum 
app_analytic_z_wide <- app_analytic_i %>%
  group_by(zip) %>%
  summarise(total_paid_pre = sum(approved_amount[!pre_post], na.rm = TRUE), # NA are non-approved; sums non-NA amounts
            total_paid_post = sum(approved_amount[pre_post], na.rm = TRUE),
            n_app_pre = length(unique(claim_id[!pre_post])),
            n_app_post = length(unique(claim_id[pre_post])),
            total_approved = sum(approval),
            prop_approved = sum(approval)/length(unique(claim_id))) %>%
  # left join other zip-level attributes used in z-code level regs 
  left_join(app_analytic_i %>% select(zip, rent_inc, final_rent_inc,
                                      renter_pop, suppressed, fsp, running_variable,
                                      ipw) %>%             
              distinct(), 
            by = "zip") 

# find zips to impute as zero by zips that (1) are present in the ZCTA-level
## data (excludes lexington but includes jefferson and fayette depending on time)
## and (2) have zero applications in application data before filtering 
zips_zeroapps_prefilter <- unique(setdiff(zcta_wall$zcta_id, app_nodup$zip))
stopifnot(length(intersect(zips_zeroapps_prefilter, unique(app_nodup$zip[app_nodup$exclude_lexington]))) == 0) # make sure no lexington zips in final set
length(zips_zeroapps_prefilter)
nrow(app_analytic_z_wide)


app_analytic_z_wide_zero <- 
  app_analytic_z_wide %>% 
  bind_rows(
    zcta_wall %>% filter(zcta_id %in% zips_zeroapps_prefilter) %>% 
      mutate(total_paid_pre = 0, total_paid_post = 0, n_app_pre = 0, n_app_post = 0, 
             total_approved = 0, prop_approved = 0,
             zip = as.character(zcta_id)) %>% 
      select(zip, renter_pop, suppressed, fsp, running_variable,
             total_paid_pre, total_paid_post, n_app_pre, n_app_post,
             rent_inc, final_rent_inc, prop_approved, ipw)) 


stopifnot(sum(is.na(app_analytic_z_wide_zero$final_rent_inc)) == 0)

## write results 
write.csv(app_analytic_i, paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"),
          row.names = FALSE) 
write.csv(app_analytic_z_wide_zero, 
          paste0(BASE_DIR, "2305_clean_data/analytic_ziplevel_wide.csv"),
          row.names = FALSE)
