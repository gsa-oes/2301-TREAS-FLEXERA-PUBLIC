######################################################################
# 2305: ERA flexibilities 
# 00_pullgeo_data.R
#
# Pull all ACS data for both 
# renter income prediction model and
# later analyses
######################################################################

rm(list = ls())

# Packages
library(dplyr)
library(tidycensus)
library(tigris)
library(yaml)
library(here)
library(logr)

# source constants
source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))

######################################################################
# Download data for models and for 
#-------------------------------------------------------------------

# Open log file
log_open(here(LOG_DIR, "00_pullgeo_data.log"))

# Download Census county, block, and ZCTA geographies
# Do this for all states and not just kentucky since 
# want to use all shapefiles for the overlap for the ZCTA-level predictions
# note: did RDS bc of geometry field / to preserve spatial
# but could try csv for better compability
creds <- read_yaml(here("creds.yaml")) # creds 
census_api_key(creds$census_api)
data("fips_codes")
fips_analytic <- unique(fips_codes$state_code[!fips_codes$state %in% c("AS",
                                                                  "GU",
                                                                  "MP",
                                                                  "PR",
                                                                  "UM", 
                                                                  "VI")]) # all fips codes minus territories 

# iterate over fips codes and pull/write
lapply(fips_analytic, pull_write_onestate, year = 2020)


# Inputs to median household renter income prediction model 
# estimate_S1901_C01_012 # median income overall
# estimate_S2503_C05_013 # median renter income (outcome)
# total pop  B03002_001
# white pop  B03002_003
# total renter pop B25003_003
# denom for total renter pop B25003_003
# Pull at: (1) ZCTA level and (2) county level
acs_income_zips <- tidycensus::get_acs(
      geography = "zcta",
      variables = c("S1901_C01_012",
                    "S2503_C05_013",
                    "B03002_001",
                    "B03002_003",
                    "B25003_001",
                    "B25003_003"),
      year = 2019,
      cache_table = TRUE,
      survey = "acs5"
  )
  
# County predictors -same but at county level 
acs_income_counties <- tidycensus::get_acs(
      geography = "county",
      variables = c("S1901_C01_012",
                    "S2503_C05_013",
                    "B03002_001",
                    "B03002_003",
                    "B25003_001",
                    "B25003_003"),
      year = 2019,
      cache_table = TRUE,
      survey = "acs5"
  )
  
## write the two ACS files as csv and RDS 
saveRDS(acs_income_zips,
        paste0(BASE_DIR, sprintf("2305_raw_data/ACS/acs_income_zips.RDS")))
write.csv(acs_income_zips,
          paste0(BASE_DIR, sprintf("2305_raw_data/ACS/acs_income_zips.csv")))
saveRDS(acs_income_counties,
        paste0(BASE_DIR, sprintf("2305_raw_data/ACS/acs_income_counties.RDS")))
write.csv(acs_income_counties,
          paste0(BASE_DIR, sprintf("2305_raw_data/ACS/acs_income_counties.csv")))

  
log_print("downloaded and stored data")
  
