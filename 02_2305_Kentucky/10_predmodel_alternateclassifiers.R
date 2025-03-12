

######################################################################
# 2305: KY
# 03_predmodel_robust.R
#
# Test other predictive models for median renter 
######################################################################

rm(list=ls())

library(tidymodels)
library(dplyr)
library(readr)
library(here)
library(ranger)
library(patchwork)
library(logr)
library(baguette)
library(xgboost)
library(kknn)
library(LiblineaR)
library(stringr)

# source utils and constants
source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))

######################################################################
# Step 1: read in recipe and best performing model results (to compare RMSE)
#-------------------------------------------------------------------

final_rf_fit <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/",
                             "final_rf_fit.RDS"))

rf_percpred_inband <- find_percpred_inband(final_rf_fit %>%
                                             collect_predictions(), 10000)


## recipe and split object
zcta_rec <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/",
                           "model_recipe.RDS"))
split <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/split.RDS"))
train <- training(split)
test  <- testing(split)

val_set <- validation_split(train,
                            prop = 0.80)



######################################################################
# Neural net estimation (ensemble/bagging)
#-------------------------------------------------------------------

ESTIMATE_NNET <- FALSE 
if(ESTIMATE_NNET){
  one_model <- bag_mlp(penalty = tune(), hidden_units = tune()) %>%
    set_engine("nnet", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  one_workflow <-
    workflow() %>%
    add_model(one_model) %>%
    add_recipe(zcta_rec)
  
  # validating parameters with a grid size of 10 
  one_model_tuned <-
    one_workflow %>%
    tune_grid(val_set,
              grid = 10,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
  model_best <-
    one_model_tuned %>%
    select_best(metric = "rmse")
  
  final_mod <-
    bag_mlp(penalty = model_best$penalty, 
            hidden_units = model_best$hidden_units) %>%
    set_engine("nnet", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  # the last workflow
  final_workflow <-
    one_workflow %>%
    update_model(final_mod)
  
  # Now create tidymodels "fit" (make predictions)
  set.seed(345)
  final_fit_nnet <-
    final_workflow %>%
    last_fit(split)
  
  ## save objects
  saveRDS(final_mod,
          paste0(BASE_DIR, "2305_intermediate_data/finalmod_nnet.RDS"))
  saveRDS(final_fit_nnet,
          paste0(BASE_DIR, "2305_intermediate_data/finalfit_nnet.RDS"))
  
}else {
  final_fit_nnet <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/finalfit_nnet.RDS"))
}

######################################################################
# XGboost estimation
#-------------------------------------------------------------------
ESTIMATE_XGBOOST <- FALSE
if(ESTIMATE_XGBOOST){
  one_model <- boost_tree(mtry = tune(), trees = tune(),
                          min_n = tune(),
                          tree_depth = tune(),
                          learn_rate = tune()) %>%
    set_engine("xgboost", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  one_workflow <-
    workflow() %>%
    add_model(one_model) %>%
    add_recipe(zcta_rec)
  
  # validating parameters with a grid size of 10 
  one_model_tuned <-
    one_workflow %>%
    tune_grid(val_set,
              grid = 10,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
  model_best <-
    one_model_tuned %>%
    select_best(metric = "rmse")
  
  final_mod <-
    boost_tree(mtry = model_best$mtry, trees = model_best$trees,
               min_n = model_best$min_n,
               tree_depth = model_best$tree_depth,
               learn_rate = model_best$learn_rate) %>%
    set_engine("xgboost", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  # the last workflow
  final_workflow <-
    one_workflow %>%
    update_model(final_mod)
  
  # Now create tidymodels "fit" (make predictions)
  set.seed(345)
  final_fit <-
    final_workflow %>%
    last_fit(split)
  
  ## save objects
  saveRDS(final_mod,
          paste0(BASE_DIR, "2305_intermediate_data/finalmod_xgboost.RDS"))
  saveRDS(final_fit,
          paste0(BASE_DIR, "2305_intermediate_data/finalfit_xgboost.RDS"))
  write.csv(model_best,
            paste0(BASE_DIR, "2305_intermediate_data/modelbest_xgboost.csv"))
} else{
  final_fit_xgboost <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/finalfit_xgboost.RDS"))
}

######################################################################
# KNN
#-------------------------------------------------------------------
ESTIMATE_KNN <- FALSE
if(ESTIMATE_KNN){
  one_model <- nearest_neighbor(neighbors = tune(), 
                                weight_func = "optimal",
                                dist_power = tune()) %>%
    set_engine("kknn", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  one_workflow <-
    workflow() %>%
    add_model(one_model) %>%
    add_recipe(zcta_rec)
  
  # validating parameters with a grid size of 10 
  one_model_tuned <-
    one_workflow %>%
    tune_grid(val_set,
              grid = 10,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
  model_best <-
    one_model_tuned %>%
    select_best(metric = "rmse")
  
  final_mod <-
    nearest_neighbor(neighbors = model_best$neighbors,
                     dist_power = model_best$dist_power,
                     weight_func = "optimal") %>%
    set_engine("kknn", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  # the last workflow
  final_workflow <-
    one_workflow %>%
    update_model(final_mod)
  
  # Now create tidymodels "fit" (make predictions)
  set.seed(345)
  final_fit <-
    final_workflow %>%
    last_fit(split)
  
  ## save objects
  saveRDS(final_mod,
          paste0(BASE_DIR, "2305_intermediate_data/finalmod_knn.RDS"))
  saveRDS(final_fit,
          paste0(BASE_DIR, "2305_intermediate_data/finalfit_knn.RDS"))
  write.csv(model_best,
          paste0(BASE_DIR, "2305_intermediate_data/modelbest_knn.csv"))
  
} else{
  final_fit_knn <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/finalfit_knn.RDS"))
  
}

######################################################################
# SVM
#-------------------------------------------------------------------
ESTIMATE_SVM <- FALSE
if(ESTIMATE_SVM){
  one_model <- svm_linear(cost = tune(),
                          margin = tune()) %>%
    set_engine("LiblineaR", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  one_workflow <-
    workflow() %>%
    add_model(one_model) %>%
    add_recipe(zcta_rec)
  
  # validating parameters with a grid size of 10 
  one_model_tuned <-
    one_workflow %>%
    tune_grid(val_set,
              grid = 10,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
  model_best <-
    one_model_tuned %>%
    select_best(metric = "rmse")
  
  final_mod <-
    svm_linear(cost = model_best$cost,
               margin = model_best$margin) %>%
    set_engine("LiblineaR", nthreads = 4) %>%
    set_mode("regression") %>%
    translate()
  
  # the last workflow
  final_workflow <-
    one_workflow %>%
    update_model(final_mod)
  
  # Now create tidymodels "fit" (make predictions)
  set.seed(345)
  final_fit <-
    final_workflow %>%
    last_fit(split)
  
  ## save objects
  saveRDS(final_mod,
          paste0(BASE_DIR, "2305_intermediate_data/finalmod_svm.RDS"))
  saveRDS(final_fit,
          paste0(BASE_DIR, "2305_intermediate_data/finalfit_svm.RDS"))
  write.csv(model_best,
          paste0(BASE_DIR, "2305_intermediate_data/modelbest_svm.csv"))

} else{
  final_fit_svm <- readRDS(paste0(BASE_DIR, "2305_intermediate_data/finalfit_svm.RDS"))
}


######################################################################
# Compare regression models 
#-------------------------------------------------------------------

all_mods_percinband <- lapply(list(final_fit_knn,
                                   final_fit_nnet,
                                   final_fit_svm,
                                   final_fit_xgboost,
                                   final_rf_fit),
                          function(x) find_percpred_inband(x %>% collect_predictions(),
                                                           10000))
all_mods_percinband_5000 <- lapply(list(final_fit_knn,
                                   final_fit_nnet,
                                   final_fit_svm,
                                   final_fit_xgboost,
                                   final_rf_fit),
                              function(x) find_percpred_inband(x %>% collect_predictions(),
                                                               5000))

all_mods_percinband_df <- data.frame(model = c("knn", "nnet",
                                               "svm", "xgboost", 
                                               "rf (original)"),
                                     perc_within10000 = 
                                    unlist(all_mods_percinband),
                                    perc_within5000 = 
                                   unlist(all_mods_percinband_5000)) %>%
                    arrange(perc_within10000)
write.csv(all_mods_percinband_df,
          paste0(BASE_DIR, "2305_model_output/compareaccuracy_predrentinc.csv"),
          row.names  = FALSE) 

######################################################################
# Clean up data to merge predictions to
#-------------------------------------------------------------------

# read original ZCTA data that contains missing values
zctas_all <- read.csv(here(BASE_DIR, 
                           "2305_intermediate_data/zcta_attributes_forpred_withmissing.csv")) %>%
  mutate(zcta_id = str_pad(zcta_id, width = 5, side = "left", pad = "0"),
         state_id = as.character(state_id),
         missing_rentinc = is.na(rent_inc)) 

zctas_all_ky <- zctas_all %>% filter(in_ky) #  t, f, or na - na are ones that we couldn't map to counties via the block:zcta overlap

# separate into two datasets:
# 1. the one we need to impute renter hh income for
# 2. the one we don't 
zcta_yesimpute <- zctas_all_ky %>% filter(missing_rentinc)
zcta_impute_idkeep <- zcta_yesimpute$zcta_id
zcta_noimpute <- zctas_all %>% filter(!missing_rentinc &
                                        !is.na(county_id)) # both exclude those missing median renter incoem and those not mapped to counties 

# remove non-predictor columns 
nonfeatures <-  c("zcta_id", "NAME", "county_id", "any_missing",
                  "is_duplicated_spatmerge",
                  "in_ky", "is_same_spatialhherf", 
                  "county_name_clean", "county_id_spatial",
                  "missing_rentinc")
zcta_yesimpute_formod <- zcta_yesimpute %>% select(-all_of(nonfeatures))
zcta_noimpute_formod <- zcta_noimpute %>% select(-all_of(nonfeatures))

zcta_yesimpute_formod <- zcta_yesimpute_formod %>%
                        mutate(med_inc = ifelse(is.na(med_inc),
                        median(med_inc, na.rm = TRUE),
                        med_inc))

zcta_rec <- recipe(rent_inc ~ ., data = zcta_noimpute_formod) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_naomit(all_numeric()) %>%
  step_string2factor(state_id) %>%
  step_dummy(state_id) %>% 
  prep(training = zcta_noimpute_formod, retain = TRUE)


zcta_yesimpute_formod <- bake(zcta_rec, new_data = zcta_yesimpute_formod, all_predictors())


######################################################################
# For ones within range of accuracy,  generate predictions
#-------------------------------------------------------------------

predict_onemod <- function(one_mod_object, one_modbest, one_mod_name){
  model_best <- one_modbest
  mod_forpred <- one_mod_object %>%
    fit(
      rent_inc ~ ., data = bake(zcta_rec, new_data = NULL),
    )
  predictions <- predict(mod_forpred, new_data = zcta_yesimpute_formod)
  colnames(predictions) <- paste0("predictions_", one_mod_name)
  return(predictions)
  
}

predictions_svm <- predict_onemod(one_mod_object = readRDS(paste0(BASE_DIR,
                                                  "2305_intermediate_data/finalmod_svm.RDS")),
                                  one_modbest = read.csv(paste0(BASE_DIR,
                                                "2305_intermediate_data/modelbest_svm.csv")),
                                  one_mod_name = "svm") 
predictions_xgboost <- predict_onemod(one_mod_object = readRDS(paste0(BASE_DIR,
                                                                  "2305_intermediate_data/finalmod_xgboost.RDS")),
                                  one_modbest = read.csv(paste0(BASE_DIR,
                                                                "2305_intermediate_data/modelbest_xgboost.csv")),
                                  one_mod_name = "xgboost") 
predictions_knn <- predict_onemod(one_mod_object = readRDS(paste0(BASE_DIR,
                                                                      "2305_intermediate_data/finalmod_knn.RDS")),
                                      one_modbest = read.csv(paste0(BASE_DIR,
                                                                    "2305_intermediate_data/modelbest_knn.csv")),
                                      one_mod_name = "knn") 


######################################################################
# Merge predictions, generate rnter income variable, and write file 
#-------------------------------------------------------------------

## create df with predictions to join to main data
zcta_imputed_tojoin <- cbind.data.frame(predictions_svm,
                                        predictions_xgboost,
                                        predictions_knn, zcta_id = zcta_impute_idkeep)


## left join onto main ky data
zctas_all_ky_wpred <- zctas_all_ky %>%
  left_join(zcta_imputed_tojoin,
            by = "zcta_id") # warning about 1:many from the one duplicate ZCTA- maybe look how KY classified them? 


# 1. final_rent_inc = final rent_inc column, no missingness
# 2. rent_inc = original rent_inc column, missingness
# 3. rent_predicted = binary indicator, was rent_inc predicted
zctas_all_ky_wpred <- zctas_all_ky_wpred %>% 
  mutate(final_rent_inc_svm = case_when(is.na(rent_inc) ~ predictions_svm,
                                    .default = rent_inc),
         final_rent_inc_xgboost = case_when(is.na(rent_inc) ~ predictions_xgboost,
                                            .default = rent_inc),
         final_rent_inc_knn = case_when(is.na(rent_inc) ~ predictions_knn,
                                        .default = rent_inc)) %>%
  select(-in_ky, -any_missing) 

write.csv(zctas_all_ky_wpred,
          paste0(BASE_DIR, "2305_intermediate_data/ky_zctas_complete_wnewpredictions.csv"))
