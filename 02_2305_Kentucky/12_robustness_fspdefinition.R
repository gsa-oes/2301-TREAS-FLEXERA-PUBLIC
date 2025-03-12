######################################################################
#---------------------------------------------------------------------
# 2305: Robustness check for creation of FSP indicator
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
# Step 4: construct different ZCTA:county matches beyond spatial one
#--------------------------------------------

## have two sets of county ids:
## (1) county_id: based on hherf
## (2) county_id_spatial: based on spatial overlap in earlier script
## add alternate county ids

geocorr <- read.csv(paste0(BASE_DIR,"2305_raw_data/geocorr2018_2311601291.csv"))
geocorr <- geocorr[-1,] # first row is longer names

zcta_cwalk_land <- geocorr %>%
  filter(grepl("^21.*", county)) %>% # filter to KY only (fips code 21)
  # afact is a proportion that measures the surface area of the ZIP that 
  # overlaps the surface area of the county
  mutate(afact = as.numeric(afact)) %>%
  # within ZIPs, we choose the county with the maximum overlap
  group_by(zcta5) %>%
  filter(afact == max(afact)) %>%
  ungroup() %>%
  select(zcta5, county_id_land = county, afact) 

zcta_cwalk_pop <- geocorr %>%
  filter(grepl("^21.*", county)) %>%
  mutate(pop10 = as.numeric(pop10)) %>%
  group_by(zcta5) %>%
  filter(pop10 == max(pop10)) %>%
  ungroup() %>%
  select(zcta5, county_id_pop = county, pop10) 

## merge onto existing county ids
acs_wrent_wcountyids <- acs_wrent %>%
              left_join(zcta_cwalk_land, by = c("zcta_id" = "zcta5")) %>%
              left_join(zcta_cwalk_pop, by = c("zcta_id" = "zcta5")) %>%
              mutate(all_same_countyid = (county_id == county_id_spatial) & 
                                        (county_id_spatial == county_id_land) &
                                        (county_id_land == county_id_pop)) 


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
  ) %>% 
  select(county_id, county_ami, county_name)


# Merge AMI cutoff using the four different zcta:county matches
acs_wami <- acs_wrent_wcountyids %>% mutate(county_id_spatial = as.character(county_id_spatial)) %>%
        left_join(ami_hherf %>% select(county_id, county_ami) %>% rename(county_ami_hherf = county_ami),
                  by = "county_id") %>%
        left_join(ami_hherf %>% select(county_id, county_ami) %>% rename(county_ami_spatial = county_ami),
                  by = c("county_id_spatial" = "county_id")) %>%
        left_join(ami_hherf %>% select(county_id, county_ami) %>% rename(county_ami_land = county_ami),
            by = c("county_id_land" = "county_id")) %>%
        left_join(ami_hherf %>% select(county_id, county_ami) %>% rename(county_ami_pop = county_ami),
            by = c("county_id_pop" = "county_id")) %>%
      mutate(all_same_countyami = (county_ami_hherf == county_ami_spatial) & 
           (county_ami_spatial == county_ami_land) &
           (county_ami_land == county_ami_pop)) 
          

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

## because warning about nas from casting rent inc as numeric
## verify that there are no missing ones among the nonsuppressed
stopifnot(sum(is.na(fsp_hherf_to_merge$rent_inc_hherf[!fsp_hherf_to_merge$suppressed_hherf])) == 0)

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
# Step 7: create different FSP indicators 
#--------------------------------------------


acs_wami_whherf <- acs_wami_whherf %>%
            mutate(fsp_spatialmerge = ifelse(test = !suppressed & rent_inc <= county_ami_spatial,
                                             yes = TRUE,
                                             no = FALSE),
                   fsp_popmerge = ifelse(test = !suppressed & rent_inc <= county_ami_pop,
                                         yes = TRUE,
                                         no = FALSE),
                   fsp_landmerge = ifelse(test = !suppressed & rent_inc <= county_ami_land,
                                         yes = TRUE,
                                         no = FALSE)) 


######################################################################
# Step 8: merge with main analytic dataset 
#--------------------------------------------

app_analytic_i <- read.csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))

app_analytic_wfsp <- app_analytic_i %>% mutate(zip = as.character(zip)) %>% 
              left_join(acs_wami_whherf %>% select(zcta_id, fsp_popmerge),
                        by = c("zip" = "zcta_id"))

write.csv(app_analytic_wfsp, 
          paste0(BASE_DIR, "2305_clean_data/analytic_applevel_wfsprobust.csv"),
          row.names = FALSE)
