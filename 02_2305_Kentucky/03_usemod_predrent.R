######################################################################
# 2305: KY
# 01_rent_final_predictions.R
#
# Create renter med-inc predictions using validated models.
######################################################################

rm(list=ls())

library(dplyr)
library(tidymodels)
library(readr)
library(here)
library(logr)

source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))

log_open(here(LOG_DIR, "03_usemod_predrent.log"))


######################################################################
# Step 1: read in data and specifications for random forest model
#-------------------------------------------------------------------

# read original ZCTA data that contains missing values
zctas_all <- read.csv(here(BASE_DIR, 
                                "2305_intermediate_data/zcta_attributes_forpred_withmissing.csv")) %>%
                mutate(zcta_id = str_pad(zcta_id, width = 5, side = "left", pad = "0"),
                       state_id = as.character(state_id),
                       missing_rentinc = is.na(rent_inc)) 

zctas_all_ky <- zctas_all %>% filter(in_ky) #  t, f, or na - na are ones that we couldn't map to counties via the block:zcta overlap

log_print(sprintf("Nationwide: loaded complete ZCTA data, which has %s unique ZCTAs and %s are missing median rental income",
                  length(unique(zctas_all$zcta_id)),
                  sum(is.na(zctas_all$rent_inc))))

log_print(sprintf("In KY: Loaded complete ZCTA data, which has %s unique ZCTAs and %s are missing median rental income",
                  length(unique(zctas_all_ky$zcta_id)),
                  sum(is.na(zctas_all_ky$med_inc))))

rf_best <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/", "rf_best_specs.csv"))
last_rf_mod <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/",
                            "last_rf_mod.RDS"))

######################################################################
# Step 2: prepare data for generating predictions by separating
# out KY missing renter income ZCTAs as test set
#-------------------------------------------------------------------


log_print(sprintf("There are %s unique ZCTAs that meet the criteria for the test sample (KY missing renter income)",
        length(unique(zctas_all_ky$zcta_id[zctas_all_ky$missing_rentinc]))),
        hide_notes= TRUE)

# separate into two datasets:
# 1. the one we need to impute renter hh income for
# 2. the one we don't 
zcta_yesimpute <- zctas_all_ky %>% filter(missing_rentinc)
zcta_impute_idkeep <- zcta_yesimpute$zcta_id
zcta_noimpute <- zctas_all %>% filter(!missing_rentinc &
                                          !is.na(county_id)) 

# remove non-predictor columns 
nonfeatures <-  c("zcta_id", "NAME", "county_id", "any_missing",
                "is_duplicated_spatmerge",
                "in_ky", "is_same_spatialhherf", 
                "county_name_clean", "county_id_spatial",
                  "missing_rentinc")
zcta_yesimpute_formod <- zcta_yesimpute %>% select(-all_of(nonfeatures))
zcta_noimpute_formod <- zcta_noimpute %>% select(-all_of(nonfeatures))


######################################################################
# Step 3: estimate best-fitting model again and generate predictions in test set
#-------------------------------------------------------------------
zcta_rec <- recipe(rent_inc ~ ., data = zcta_noimpute_formod) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_naomit(all_numeric()) %>%
  step_string2factor(state_id) %>%
  step_dummy(state_id) %>% 
  prep(training = zcta_noimpute_formod, retain = TRUE)


rf_mod_forpred <- last_rf_mod %>%
  fit(
    rent_inc ~ ., data = bake(zcta_rec, new_data = NULL),
  )


zcta_yesimpute_formod <- bake(zcta_rec, new_data = zcta_yesimpute_formod, all_predictors())

apply(zcta_yesimpute_formod, 2, function(x) {
  sum(is.na(x))
})


# some med_inc missing in test data, even though it's a good predictor
# so impute those to the median across the test sample 
zcta_yesimpute_formod <- zcta_yesimpute_formod %>%
              mutate(med_inc = ifelse(is.na(med_inc),
                                      median(med_inc, na.rm = TRUE),
                                      med_inc))

## generate predictions with the best-fitting model (rf_mod)
## but in the test set
predictions <- predict(rf_mod_forpred, new_data = zcta_yesimpute_formod) %>%
          rename(rent_predicted = .pred)

## before binding, throw error if the number of predictions differs from
## number of rows 
stopifnot(length(predictions$rent_predicted) == nrow(zcta_yesimpute_formod))


######################################################################
# Step 4: add predicted rental income values back to the other ZCTA-level covariates 
#-------------------------------------------------------------------

## create df with predictions to join to main data
zcta_imputed_tojoin <- cbind.data.frame(predictions, zcta_id = zcta_impute_idkeep)


## left join onto main ky data
zctas_all_ky_wpred <- zctas_all_ky %>%
                left_join(zcta_imputed_tojoin,
                          by = "zcta_id") # warning about 1:many from the one duplicate ZCTA- maybe look how KY classified them? 


# 1. final_rent_inc = final rent_inc column, no missingness
# 2. rent_inc = original rent_inc column, missingness
# 3. rent_predicted = binary indicator, was rent_inc predicted
zctas_all_ky_wpred <- zctas_all_ky_wpred %>% 
  mutate(final_rent_inc = case_when(is.na(rent_inc) ~ rent_predicted,
                                    .default = rent_inc)) %>%
    select(-in_ky, -any_missing) 

log_print(sprintf("Prepared data to write; %s ZCTAs; %s with observed renter income; %s with predicted renter income",
                  nrow(zctas_all_ky_wpred),
                  sum(!is.na(zctas_all_ky_wpred$rent_inc)),
                  sum(!is.na(zctas_all_ky_wpred$rent_predicted))))

######################################################################
# Step 5: write 
#-------------------------------------------------------------------

write.csv(zctas_all_ky_wpred,
          paste0(BASE_DIR, "2305_intermediate_data/ky_zctas_complete.csv"))

log_close()
