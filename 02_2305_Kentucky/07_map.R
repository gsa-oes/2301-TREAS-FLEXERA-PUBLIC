
library(tidyverse)
library(tidycensus)
library(tigris)
library(geomander)
library(yaml)
library(readxl)
library(censable)
library(cli)
library(glue)
library(sf); sf_use_s2(FALSE)


# estimate_S1901_C01_012 # median income overall
# estimate_S2503_C05_013 # outcome

BASE_DIR <- "G:/Shared drives/OES data 2305 ERA Grantee Flexibilities KY/"

acs_income <- tidycensus::get_acs(
  geography = "zcta",
  variables = c("S2503_C05_013"),
  year = 2019,
  cache_table = TRUE,
)

zcta_covars <- acs_income %>% 
  pivot_wider(id_cols = GEOID:NAME,
              names_from = "variable",
              values_from = estimate:moe)

sum(is.na(zcta_covars$estimate_S2503_C05_013))

# Read in Excel Data
  # Skip (header) and
  # Drop last three rows
zip <- read_xlsx(paste0(BASE_DIR, "2305_raw_data/FSP/HHERF_2.0_FSP_ZIPCode_add_JeffCo_2022-4-14.xlsx"), skip = 1) %>% 
  filter(row_number() <= n()-4)
zip$zip <- substr(zip$`Zip Code`, 1, 5)

table(zip$`UNIT ZIP IS INCOME ELIGIBLE?`)
zip$eligible <- ifelse(zip$`UNIT ZIP IS INCOME ELIGIBLE?` == "PROXY ELIGIBLE",
                       1, 0)
table(zip$eligible, useNA = "always")

ky_zips <- tigris::zctas(state = "KY", year = 2010)
glue(nrow(ky_zips), " KY ZCTAs")

table(zip$zip %in% ky_zips$ZCTA5CE10)
table(ky_zips$ZCTA5CE10 %in% zcta_covars$GEOID)

ky_zips <- ky_zips %>% 
  left_join(zcta_covars, by = c("ZCTA5CE10" = "GEOID")) %>% 
  left_join(zip, by = c("ZCTA5CE10" = "zip"))

ky_zips$suppressed <- ifelse(is.na(ky_zips$estimate_S2503_C05_013),
                             1, 0)
table(ky_zips$suppressed, useNA = "always")

#----------------------------------------------------------
# find Lexington zip codes
ky_places <- tigris::places(state = "KY", year = "2020")

lex <- ky_places %>% 
  filter(NAME == "Lexington-Fayette")

overlap_list <- sf::st_overlaps(ky_zips, lex)
ky_zips$in_lex <- overlap_list %>% lengths() > 0

# identify variable for fill type
ky_zips <- ky_zips %>% 
  mutate(fill_type = case_when(in_lex == TRUE ~ "Lexington",
                               eligible == 1 ~ "FSP Eligible",
                               TRUE ~ "FSP Ineligible"))

#----------------------------------------------------------
# Deal with cities
ky_cities <- maps::us.cities %>% 
  filter(country.etc == "KY")

ky_cities <- sf::st_as_sf(ky_cities, coords = c("long", 
                                    "lat"), 
                    crs = 4326, agr = "constant")

ky_cities$name <- gsub(" KY", "", ky_cities$name)
#----------------------------------------------------------

lex <- ky_zips %>% 
  filter(fill_type == "Lexington") %>% 
  summarize(geometry = st_union(geometry))

ky_state <- tigris::states()

ky_state <- ky_state %>% filter(NAME == "Kentucky")

p <- ky_zips %>% 
  ggplot() + 
  geom_sf(color = "black",
          aes(fill = fill_type)) + 
  geom_sf(data = lex, lwd = 1.5, fill = NA, color = "black") +
  geom_sf_label(data = ky_cities %>% 
                  filter(name == "Lexington"), 
                size = 4,
                aes(label = name), fontface = "bold") +
  geom_sf(data = ky_state, fill = NA, lwd = 2, color = "black") +
  ggthemes::theme_map() +
  scale_fill_manual(name = "",
                    values = c("#156b5c",
                               "white", "#adadad"),
                    breaks = c("Lexington", "FSP Ineligible",
                               "FSP Eligible"),
                    labels = c("Lexington", "FSP Ineligible",
                               "FSP Eligible")) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15, face = "bold")) 
ggsave(paste0(BASE_DIR, "2305_clean_data/results/map.png"), p, width = 10, height = 5, dpi = 600) 
ggsave(paste0(BASE_DIR, "2305_clean_data/results/map.svg"), p, width = 10, height = 5, dpi = 600) 


# Alternative map consistent with VA --------------------------------------

p <- ky_zips %>% 
  ggplot() + 
  geom_sf(color = "black",
          aes(fill = fill_type)) + 
  geom_sf(data = lex, lwd = 1, fill = NA, color = "black") +
  geom_sf_label(data = ky_cities %>% 
                  filter(name == "Lexington"), 
                size = 4,
                aes(label = name), fontface = "bold") +
  geom_sf(data = ky_state, fill = NA, lwd = 1, color = "black") +
  ggthemes::theme_map() +
  scale_fill_manual(name = "",
                    values = c("#adadad",
                               "white", "#156b5c"),
                    breaks = c("Lexington", "FSP Ineligible",
                               "FSP Eligible"),
                    labels = c("Lexington", "FSP Ineligible",
                               "FSP Eligible")) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15, face = "bold")) 

ggsave(paste0(BASE_DIR, "2305_clean_data/results/map_new.png"), p, width = 10, height = 5, dpi = 600) 



