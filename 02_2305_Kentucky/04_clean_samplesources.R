######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Pulls geographic data used to construct AMI flags / implement county exclusions
# (rather than for the median income among rental-occupied predictions)
#---------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(readxl)
library(here)
library(tidyr)
library(stringr)

source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))


######################################################################
# Step 1: Read in data on AMI classifications for the country as a whole
#--------------------------------------------

ami <- read_xlsx(paste0(BASE_DIR, "2305_raw_data/AMI/Section8-FY19.xlsx"))

######################################################################
# Step 2: Read in original HHERF program data that classifies ZIP codes
# as FSP eligible or not based on county AMI
#--------------------------------------------

fsp_hherf <- read_xlsx(paste0(BASE_DIR, "2305_raw_data/FSP/HHERF_2.0_FSP_ZIPCode_add_JeffCo_2022-4-14.xlsx"), 
                       range = "A2:J771")

names(fsp_hherf) <- names(fsp_hherf) %>% 
  tolower() %>% 
  gsub(" |-|\\%|\\(|\\)|\\$|\\?|\\*|\\^", "_", x = .) %>% 
  gsub("__", "_", x = .) %>% 
  paste0("hherf_",.)

######################################################################
# Step 3: Read in data containing predicted rental income for suppressed ZIP codes
#--------------------------------------------

acs_wrent <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/ky_zctas_complete.csv")) %>%
            mutate(county_id = as.character(county_id),
                   zcta_id = as.character(zcta_id)) %>%
            select(-X)


######################################################################
# Step 4: Read and merge indicator for rural ZIP codes to ZCTAs
#--------------------------------------------

## uses zip codes
rural_zips <- read_excel(paste0(BASE_DIR, "2305_raw_data/rurality/forhp-eligible-zips.xlsx"))

## crosswalk to merge with zctas 
zip_zcta <- read_excel(paste0(BASE_DIR, "2305_raw_data/rurality/ZIPCodetoZCTACrosswalk2021UDS.xlsx"))

rural_zips_wzt <- rural_zips %>%
  left_join(zip_zcta, by = "ZIP_CODE", 
            suffix = c("_rural", "_cw")) %>%
  rename(zcta_id = ZCTA)



######################################################################
# Step 5: merge in HUD AMI data
#--------------------------------------------

# Create merge data from AMI dataset
# by filtering to kentucky and creating merge variables
ami_hherf <- 
  ami %>% 
  filter(State == "21") %>% 
  mutate(county_3_digits = sprintf("%03d", County), 
         county_id = paste0(State, county_3_digits),
         county_name = County_Name,
         # This is how KY appears to have calculated the AMI limit
         county_ami = median2019 * .8,
         county_ami_hud = l80_4
  ) %>% 
  select(county_id, county_ami, county_ami_hud, county_name)

# before merging, check overlap 
inacs_noami <- setdiff(acs_wrent$county_id, ami_hherf$county_id)
stopifnot(length(inacs_noami) == 0)
inami_noacs <- setdiff(ami_hherf$county_id, acs_wrent$county_id)
stopifnot(length(inami_noacs) == 0) # matches perfectly 


# Merge AMI cutoff - uses hherf zcta:county mapping 
acs_wami <- acs_wrent %>% mutate(county_id_spatial = as.character(county_id_spatial)) %>%
        left_join(ami_hherf,
                  by = "county_id") %>%
        left_join(ami_hherf %>% select(county_id, county_ami, county_ami_hud) %>% rename(county_ami_hherf_spatial = county_ami, county_ami_hud_spatial = county_ami_hud) , 
                  by = c("county_id_spatial" = "county_id")) #using county id from spatial merge to join with hud ami 


######################################################################
# Step 6: merge in HHERF definitions
#--------------------------------------------

fsp_hherf_to_merge <- 
  fsp_hherf %>% select(
    zcta_id = hherf_zip_code,
    pop_size_all_hherf = hherf_estimated_number_of_households,
    rent_inc_hherf = hherf_renter_occupied_housing_units_median_household_income_dollars_, 
    county_ami_hherf = hherf_2019_county_80_ami_,
    fsp_hherf = hherf_unit_zip_is_income_eligible_, 
    county_name_hherf = hherf_county) %>% 
  mutate(fsp_hherf = fsp_hherf == "PROXY ELIGIBLE", 
         suppressed_hherf = is.na(rent_inc_hherf) | rent_inc_hherf == "2,500-", 
         rent_inc_hherf = case_when(suppressed_hherf ~ NA_real_,
                                    .default = as.numeric(rent_inc_hherf)), 
         zcta_id = as.character(zcta_id))

## join to ami data and construct our own suppressed and med inc cat categories
acs_wami_whherf <- 
  acs_wami %>% 
  left_join(fsp_hherf_to_merge,
            by = "zcta_id",
            suffix = c("_hud", "_hherf")) %>%
  mutate(suppressed = is.na(rent_inc) | rent_inc == 2499, # code the suppression indicator; to match FSP, coding the 2500- as NA
         med_inc_cat = ifelse(!is.na(rent_inc), 
                              sprintf("decile: %s", ntile(rent_inc, 10)),
                              "suppressed"))



######################################################################
# Step 7: merge in alternate predictions of median renter income (robustness check)
#--------------------------------------------

new_predictions <- read.csv(paste0(BASE_DIR, "2305_intermediate_data//ky_zctas_complete_wnewpredictions.csv")) %>% mutate(zcta_id = as.character(zcta_id))
acs_wami_whherf_wnewpred <- acs_wami_whherf %>%
            left_join(new_predictions %>% select(zcta_id, contains("final_rent_inc")),
                      by = "zcta_id")


######################################################################
# Step 8: create FSP indicator based on spatial merge 
#--------------------------------------------



acs_wami_whherf_wnewpred <- acs_wami_whherf_wnewpred %>%
            mutate(fsp_spatialmerge_hherfami = case_when(suppressed ~ FALSE, 
                                                rent_inc < county_ami_hherf_spatial ~ TRUE, 
                                                .default = FALSE),
                   fsp_spatialmerge_hudami = case_when(suppressed ~ FALSE, 
                                                         rent_inc < county_ami_hud_spatial ~ TRUE, 
                                                         .default = FALSE)) 
# compare
sum(acs_wami_whherf_wnewpred$fsp_hherf != acs_wami_whherf_wnewpred$fsp_spatialmerge_hherfami)
sum(acs_wami_whherf_wnewpred$fsp_hherf != acs_wami_whherf_wnewpred$fsp_spatialmerge_hudfami) # still no differences 

######################################################################
# Step 9: clean up dataset 
#--------------------------------------------

# Create canonical versions of the analytic variables: fsp, county_ami, 
# running_variable, etc. Remove dup obs and all the variables we don't use
fsp_oes_clean <- 
  acs_wami_whherf_wnewpred %>% 
  # We use HHERF definitions for treatment indicators and other identifiers 
  rename(fsp = fsp_hherf, county_ami_touse = county_ami_hherf, county_name_touse = county_name_hherf) %>% 
  # We select only the variables we need for the analysis -- including HHERF definition of renter income for reference
  select(zcta_id,
         rent_inc,
         renter_pop,
         total_pop,
         rent_inc_hherf,
         rent_predicted, # preserve predicted rent
         contains("final_rent_inc"), # mix of observed and predicted- includes predictive model robustness check  
         county_ami_touse,
         fsp,
         fsp_spatialmerge,
         county_name_touse,
         suppressed,
         missing_rentinc,
         med_inc_cat,
         is_same_spatialhherf,
         county_id_spatial) %>% 
  # Create running variable and rurality indicator
  mutate(running_variable = (as.numeric(final_rent_inc) - county_ami_touse) * -1, 
         running_variable_svm = (as.numeric(final_rent_inc_svm) - county_ami_touse) * -1,
         running_variable_xgboost = (as.numeric(final_rent_inc_xgboost) - county_ami_touse) * -1,
         running_variable_knn = (as.numeric(final_rent_inc_knn) - county_ami_touse) * -1,
         rural = zcta_id %in% rural_zips_wzt$zcta_id)


######################################################################
# Step 10: write mostly cleaned data (next script constructs vars)
#--------------------------------------------

write.csv(fsp_oes_clean, paste0(BASE_DIR, "2305_intermediate_data/zlevel_df_merged.csv"),
          row.names = FALSE)

