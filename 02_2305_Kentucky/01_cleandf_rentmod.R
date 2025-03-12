######################################################################
# 2305: ERA flexibilities 
# 01_cleandf_rentmod
#
# Clean input data for model predicting median rental income
######################################################################

rm(list = ls())

# Packages
library(data.table)
library(dplyr)
library(here)
library(logr)
library(stringr)
library(readxl)
select <- dplyr::select

# source constants
source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))

#

######################################################################
# Step 1: Read in ZCTA and county-level ACS attributes from previous script +
# hherf mapping
#-------------------------------------------------------------------

acs_income_zips <- readRDS(paste0(BASE_DIR, "2305_raw_data/ACS/acs_income_zips.RDS"))
acs_income_counties <- readRDS(paste0(BASE_DIR, "2305_raw_data/ACS/acs_income_counties.RDS"))
hherf_mapping <-  read_xlsx(paste0(BASE_DIR, "2305_raw_data/FSP/HHERF_2.0_FSP_ZIPCode_add_JeffCo_2022-4-14.xlsx"), 
                                        range = "A2:J771")

names(hherf_mapping) <- names(hherf_mapping) %>% 
  tolower() %>% 
  gsub(" |-|\\%|\\(|\\)|\\$|\\?|\\*|\\^", "_", x = .) %>% 
  gsub("__", "_", x = .) %>% 
  paste0("hherf_",.)

log_open(here(LOG_DIR, "01_cleandf_rentmod.log"))
log_print("Read in data")


######################################################################
# Step 2: clean ZCTA-level and county-level predictors for median rental income prediction model
#-------------------------------------------------------------------

# Clean ZCTA-level predictors
# by reshaping from long to wide
zcta_covars <- acs_income_zips %>%
    pivot_wider(id_cols = GEOID:NAME,
                names_from = "variable",
                values_from = estimate:moe)

# log how many are missing the income estimate
log_print(sprintf("Out of %s ZCTAs nationwide, %s are missing median renter income",
          length(unique(acs_income_zips$GEOID)),
          sum(is.na(zcta_covars$estimate_S2503_C05_013))))


# give more informative names
zcta_covars <- zcta_covars %>%
        rename(total_pop = estimate_B03002_001,
               white_pop = estimate_B03002_003,
               med_inc   = estimate_S1901_C01_012,
               rent_inc  = estimate_S2503_C05_013,
               renter_pop = estimate_B25003_003,
               renter_pop_denom = estimate_B25003_001) %>%
        select(-starts_with("moe"))

# Clean county-level predictors
# by reshaping from long to wide
cty_covars <- acs_income_counties %>%
    pivot_wider(id_cols = GEOID:NAME,
                names_from = "variable",
                values_from = estimate:moe)


# give more informative names 
cty_covars <- cty_covars %>%
    rename(cty_total_pop = estimate_B03002_001,
           cty_white_pop = estimate_B03002_003,
           cty_med_inc   = estimate_S1901_C01_012,
           cty_rent_inc  = estimate_S2503_C05_013,
           cty_renter_pop = estimate_B25003_003,
           cty_renter_pop_denom = estimate_B25003_001) %>%
    select(-starts_with("moe")) %>%
    mutate(county_name_clean = tolower(gsub("\\s.*", "", NAME)),
           state_id = as.character(substr(GEOID, 1, 2)))


######################################################################
# Step 3: construct and save a county:ZCTA level crosswalk using the 
# spatial overlap approach and the zcta and block-level data
#-------------------------------------------------------------------

# get same universe of states used in previous script (all minus territories)
data("fips_codes")
fips_analytic <- unique(fips_codes$state_code[!fips_codes$state %in% c("AS",
                                                                       "GU",
                                                                       "MP",
                                                                       "PR",
                                                                       "UM", 
                                                                       "VI")]) # all fips codes minus territories 


# construct crosswalk 
fname <- paste0(BASE_DIR, "2305_intermediate_data/", "zcta_county_crosswalk.csv")
if(!file.exists(fname)){
  
  list_cwalk <- lapply(fips_analytic, intersect_zcta_blocks_onestate)
  df_cwalk <- do.call(rbind.data.frame, list_cwalk)
  data.table::fwrite(df_cwalk, fname)
} else{
  nat_zcta_cwalk <- data.table::fread(fname) %>%
          mutate(county_id = str_pad(string = county_pop_sum, 
                                     width = 5,
                                     pad = "0",
                                     side = "left"),
                 zcta5 = as.character(zcta5)) %>%
          select(-county_pop_sum)
}


######################################################################
# Step 4: clean up and use crosswalk to merge covars 
#-------------------------------------------------------------------

# see nrow() > unique zctas so do further coding to highest pop county
nat_zcta_cwalk <- nat_zcta_cwalk %>%
  group_by(zcta5) %>%
  filter(total_block_pop == max(total_block_pop)) %>%
  ungroup() %>%
  mutate(state_id = as.character(substr(county_id, 1, 2)))

# separate into KY and not KY 
cwalk_ky <- nat_zcta_cwalk %>%
          filter(state_id == "21")
cwalk_nonky <- nat_zcta_cwalk %>%
          filter(state_id != "21") 

## first, map hherf county names to county FIPS code using
## the crosswalk
hherf_wid <- cty_covars %>%
        filter(state_id == "21") %>%
        select(GEOID, county_name_clean) %>%
        left_join(hherf_mapping  %>% mutate(county_name_clean = tolower(hherf_county)) %>%
                    select(hherf_zip_code, county_name_clean),
                  by = "county_name_clean") %>%
        rename(county_id = GEOID) %>%
        mutate(hherf_zip_code = as.character(hherf_zip_code))

## before merging, check overlap
inspatial_nohherf <- setdiff(cwalk_ky$zcta5, hherf_wid$hherf_zip_code)
inhherf_nospatial <- setdiff(hherf_wid$hherf_zip_code, cwalk_ky$zcta5)
stopifnot(length(inspatial_nohherf) == 0 & 
          length(inhherf_nospatial) == 0)

## left join onto spatial crosswalk to compare
cwalk_ky_whherf <- hherf_wid %>% 
            left_join(cwalk_ky, 
                      by = c("hherf_zip_code" = "zcta5"),
                      suffix = c("_hherf", "_spatial")) %>%
            mutate(is_same_spatialhherf = county_id_hherf == county_id_spatial)

## look for duplicates- see one from spatial- write
## code to 
log_print("Checking for duplicate matches in ZCTA:county, we see:-------------------------------")
cwalk_ky_whherf %>% group_by(hherf_zip_code) %>%
  filter(n() > 1)

## for discrepant ZCTA, code to the county that hherf uses
cwalk_ky_whherf_torbind <- cwalk_ky_whherf %>%
              mutate(keep = case_when(hherf_zip_code == "42764" & is_same_spatialhherf ~ TRUE, 
                                      hherf_zip_code == "42764" & !is_same_spatialhherf ~ FALSE,
                                      .default = TRUE)) %>%
              filter(keep) %>%
              select(-keep, -county_name_clean) %>%
              rename(county_id = county_id_hherf,
                     zcta5 = hherf_zip_code)


## check for duplicates again
log_print("Checking for duplicate matches in the clean data:-------------------------------")
cwalk_ky_whherf_torbind   %>% group_by(zcta5) %>%
  filter(n() > 1)

## record to log
log_print("Breakdown of similarities/differences bewteen HHERF ZCTA:counties and spatial ZCTA:counties- true is same; false different--------------------------------")
print(table(cwalk_ky_whherf_torbind$is_same_spatialhherf))


## add variables to the non KY crosswalk
extra_vars <- setdiff(colnames(cwalk_ky_whherf_torbind),
                      colnames(cwalk_nonky))
cwalk_nonky <- cwalk_nonky %>%
            mutate(county_id_spatial = NA,
                   is_same_spatialhherf = NA) # NA for the KY-specific columns 

## rbind
cwalk_both <- rbind.data.frame(cwalk_ky_whherf_torbind, 
                               cwalk_nonky)

# diagnose zctas missing from crosswalk
zcta_nocwalk <- setdiff(zcta_covars$GEOID,
                        cwalk_both$zcta5)

# see if they're in the other crosswalk
log_print(sprintf("There are %s ZCTAs that couldn't be mapped to a county using block:ZCTA intersect approach, or %s of total",
                  length(zcta_nocwalk),
                  round(length(zcta_nocwalk)/nrow(cwalk_both), 3)),
          hide_notes = TRUE)


zctas <- zcta_covars %>% select(-NAME) %>%
    left_join(cwalk_both, by = c("GEOID" = "zcta5")) %>%
    left_join(cty_covars %>% select(-state_id),
              by = c("county_id" = "GEOID")) %>% # note for KY, we're using the hherf county (renamed to just county_id) and not pulling covars for the non-heerf
    rename(zcta_id = GEOID) 

log_print(sprintf("There are %s zctas nationwide before further preprocessing",
                  length(unique(zctas$zcta_id))),
          hide_notes = TRUE)

## one remaining non-ky duplicate - keeping both in 
dup_zctas <- zctas %>% group_by(zcta_id) %>% filter(n() > 1)

zctas <- zctas %>%
      mutate(is_duplicated_spatmerge = zcta_id %in% dup_zctas,
             in_ky = state_id == 21) # 1 zctas match exactly on population to two different counties - dealt with ky by looking at which matches hherf 


######################################################################
# Step 5: construct analytic sample of ZCTAs nationwide by removing missing /et c
#-------------------------------------------------------------------

# construct indicator for missing on any of the features/predictors or outcome
outcome <- "rent_inc"
features <- c("cty_total_pop", "cty_white_pop",
              "cty_renter_pop_denom",
              "cty_renter_pop",
              "cty_med_inc",
              "cty_rent_inc",
              "med_inc",
              "renter_pop",
              "renter_pop_denom",
              "total_pop",
              "white_pop",
              "county_id") # even though county not a predictor, exclude ZCTAs that dont map to any counties since dont have county-level covars

zctas$any_missing <- rowMeans(is.na(zctas[, c(outcome, features)])) > 0

zctas_complete <- zctas %>% filter(!any_missing)

zctas_complete$any_missing <- rowMeans(is.na(zctas_complete[, c(outcome, features)])) > 0

stopifnot(all(zctas_complete$any_missing) == FALSE)

log_print(sprintf("After removing ZCTAs missing any features or predictors, go from %s to %s zctas nationwide",
                  length(unique(zctas$zcta_id)),
                  length(unique(zctas_complete$zcta_id))),
          hide_notes = TRUE)

log_print(sprintf("After removing ZCTAs missing any features or predictors, go from %s to %s zctas in Kentucky",
                  length(unique(zctas$zcta_id[zctas$in_ky])),
                  length(unique(zctas_complete$zcta_id[zctas_complete$in_ky]))),
          hide_notes = TRUE)


######################################################################
# Step 6: write two datasets
#-------------------------------------------------------------------

## Dataset 1: all ZCTAs before filtering out missing ones
write.csv(zctas, here(BASE_DIR, "2305_intermediate_data/zcta_attributes_forpred_withmissing.csv"),
          row.names = FALSE)

write.csv(zctas_complete, here(BASE_DIR, "2305_intermediate_data/zcta_attributes_forpred.csv"),
          row.names = FALSE)

log_close()
