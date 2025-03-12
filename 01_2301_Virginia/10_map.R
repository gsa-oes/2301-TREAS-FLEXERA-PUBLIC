# Set up ------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(tidycensus)
library(data.table)
library(yaml)
library(tigris)

# Setup census data tool credentials --------------------------------------

# Get Census API credentials
creds <- read_yaml("G:/Shared drives/OES data 2113 ERA/2113_00_raw_data/geocoding_creds.yaml")
census_api_key(creds$census$api_key)

# Get codebook for ACS
acs_vars <- load_variables(year = 2019, dataset = "acs5", cache = TRUE)

# Load zips data ----------------------------------------------------------

fsp_zips <- read.csv("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_00_raw_data/VA_program_documents/fsp_zips.csv")
names(fsp_zips) <- "zip"

# Check for duplicate zips ------------------------------------------------

fsp_zips %>% pull(zip) %>% duplicated() %>% sum()

# Pull VA zips from online ------------------------------------------------

va_zips <- zctas(state = "VA", year = 2010)

# Create variable for membership in FSP -----------------------------------

chesterfield <- 
  c("23112",
    "23234",
    "23225",
    "23803",
    "23832",
    "23831",
    "23235",
    "23834",
    "23237",
    "23113",
    "23236",
    "23114",
    "23838",
    "23836",
    "23120",
    "23297",
    "23806")

fairfax <- c(
  "22003",
  "22030",
  "20171",
  "22015",
  "20170",
  "20120",
  "22033",
  "22309",
  "22079",
  "22306",
  "22031",
  "22042",
  "22312",
  "22310",
  "22153",
  "22032",
  "22315",
  "22152",
  "20191",
  "20121",
  "22101",
  "22150",
  "22041",
  "22182",
  "22043",
  "20151",
  "22180",
  "22102",
  "22311",
  "20190",
  "22124",
  "22046",
  "22151",
  "22039",
  "22066",
  "20124",
  "22303",
  "22181",
  "22308",
  "22044",
  "20194",
  "22307",
  "22060",
  "22027",
  "22185",
  "22035",
  "20122",
  "20153",
  "20172",
  "20193",
  "20192",
  "20195",
  "20196",
  "22009",
  "22037",
  "22036",
  "22047",
  "22067",
  "22081",
  "22092",
  "22082",
  "22095",
  "22096",
  "22103",
  "22107",
  "22106",
  "22109",
  "22108",
  "22118",
  "22116",
  "22120",
  "22119",
  "22122",
  "22121",
  "22158",
  "22156",
  "22160",
  "22159",
  "22161",
  "22183",
  "22184",
  "22199",
  "20511"
)


# Check overlap
mean(fsp_zips$zip %in% va_zips$ZCTA5CE10)

# Code FSP
va_zips <- 
  va_zips %>% 
  mutate(fsp = case_when(
    ZCTA5CE10 %in% fsp_zips$zip ~ "FSP", 
    ZCTA5CE10 %in% c(chesterfield, fairfax) ~ "Different ERA\nprogram",
    TRUE ~ "No FSP"
  ))

table(va_zips$fsp)

fsp_map <- 
  ggplot(data = va_zips, 
         aes(fill = fsp)) + 
  geom_sf(color = "black", size = .05) + 
  theme_bw() +
  theme(
    line = element_blank(), panel.border = element_blank(),
    axis.text = element_blank(), legend.position = "bottom"
  ) +
  scale_fill_manual(name = "", values = c("grey64", "#156b5c", "white"))  

fsp_map

ggsave(filename = "~/Google Drive/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/fsp_map.png", 
       width = 7, height = 5)


