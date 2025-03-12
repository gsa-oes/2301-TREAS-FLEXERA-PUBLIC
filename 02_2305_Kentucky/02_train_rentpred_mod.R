######################################################################
# 2305: KY
# 02_train_rentpred_mod.R
#
# Validate model to create renter med-inc predictions.
######################################################################

rm(list=ls())

library(tidymodels)
library(dplyr)
library(readr)
library(here)
library(ranger)
library(patchwork)
library(logr)

# source utils and constants
source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))

# open up log
log_open(here(LOG_DIR, "02_train_rentpred_mod.log"))


######################################################################
# Step 1: read in data and prep for modeling 
#-------------------------------------------------------------------

zcta <- read.csv(here(paste0(BASE_DIR, "2305_intermediate_data/", 
                            "zcta_attributes_forpred.csv"))) %>%
        mutate(state_id = as.character(state_id)) # make sure state_id treated as a character rather than numeric

log_print(sprintf("Read in cleaned ZCTA data restricted to %s ZCTAs nationwide with non-missing outcome/predictors",
          length(unique(zcta$zcta_id))),
          hide_notes = TRUE)

# remove non covars
nonfeatures <- c("zcta_id", "NAME", "county_id", "any_missing",
                 "is_duplicated_spatmerge",
                 "in_ky", "is_same_spatialhherf", 
                 "county_name_clean", "county_id_spatial")
zcta_forpred <- zcta %>%
        select(-all_of(nonfeatures))

######################################################################
# Step 2: split data 
#-------------------------------------------------------------------

# test split
set.seed(12345)
split <- initial_split(zcta_forpred, prop = 0.8)
train <- training(split)
test  <- testing(split)

val_set <- validation_split(train,
                            prop = 0.80)

log_print(sprintf("After splitting, we have %s zctas in training; %s in test; and %s in validation",
          round(nrow(train)*0.8),
          nrow(test),
          round(nrow(train)*0.2)),
          hide_notes = TRUE)


######################################################################
# Step 3: construct general recipe to use across models
#-------------------------------------------------------------------

# construct recipe with rental income at zcta as dv
zcta_rec <- recipe(rent_inc ~ ., data = train) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_naomit(all_numeric()) %>%
  step_string2factor(state_id) %>%
  step_dummy(state_id)


log_print("General modeling recipe is:---------------------------")
log_print(zcta_rec,
          hide_notes = TRUE)

#----------------------------------------------------------
# Step 4: estimate lasso and find best set of hyperparameters 
#---------------------------------------


lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# tidymodels asks for a "workflow"
# e.g. add model and recipe to steps specified above
lr_workflow <-
  workflow() %>%
  add_model(lasso_spec) %>%
  add_recipe(zcta_rec)

# setup wide penalty grid
lr_reg_grid <- tibble(penalty = 10^seq(-4, 1, length.out = 30))

lr_res <-
  lr_workflow %>%
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))

# get top performing which happens to be largest penalty
# see that rmse pretty similar though across penalty
# terms which means penalty isn't the most important 
lr_best <- lr_res %>%
      collect_metrics() %>%
      filter(mean == min(mean)) 

# create a final workflow object
# using the general workflow and the 
# top-performing hyperparameters
final_lasso_workflow <- finalize_workflow(
  lr_workflow,
  lr_best
)  

# generate final predictions in the test test
final_lasso_fit <- last_fit(
  final_lasso_workflow,
  split
  ) 

bandwidth <- 10000
lr_percpred_inband <- find_percpred_inband(final_lasso_fit %>%
                                          collect_predictions(), 10000)
log_print(sprintf("Best performing lasso had penalty of %s; %s proportion of predictions within 10k of actual renter income",
          lr_best$penalty,
          round(lr_percpred_inband, 4)),
          hide_notes = TRUE)

# write figure
lasso_fig <- (final_lasso_fit %>% collect_predictions() %>% ggplot(aes(x = .pred, y = rent_inc,
             fill = abs(.pred - rent_inc) < 10000,
             alpha = abs(.pred - rent_inc) < 10000),
         color = "black") +
  geom_point(pch = 21) + 
  scale_fill_manual(values = c("black", "#f9834a"),
                    name = "Within $10k?") + 
  scale_alpha_manual(values = c(0.2, 0.8)) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  guides(alpha = "none") +
  labs(x = "Predicted \n Renter Renter Income",
       y = "Actual \n Median Renter Income",
       title = "LASSO Predictions") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)))

ggsave(here("00_2305_Kentucky/figs/lasso_rentpred.png"),
       plot = lasso_fig,
       device = "png",
       dpi = 300)
  

#----------------------------------------------------------
# Step 5: estimate random forest and find best set of hyperparameters 
#---------------------------------------

# Similar to above: specify a RF forest model
# tune() means these parameters will be tuned through
# validation below, trees = 100 is pre-specified rather than 
# a hyperparaemter we iterate over 
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 100,
                       mode = "regression") %>%
  set_engine("ranger")

rf_workflow <-
  workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(zcta_rec)

# Validating all parameters above
# with "grid" size of 25 options.
rf_res <-
  rf_workflow %>%
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))

# Select best model
rf_best <-
  rf_res %>%
  select_best(metric = "rmse")

# Create new best RF model given best performance
last_rf_mod <-
  rand_forest(mtry = rf_best$min_n,
              min_n = rf_best$min_n,
              trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("regression") 


# the last workflow
final_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)

# Now create tidymodels "fit" (make predictions)
set.seed(345)
final_rf_fit <-
  final_rf_workflow %>%
  last_fit(split)

rf_fig <- 
  (final_rf_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = rent_inc,
             fill = abs(.pred - rent_inc) < 10000,
             alpha = abs(.pred - rent_inc) < 10000),
         color = "black") +
  geom_point(pch = 21) + 
  scale_fill_manual(values = c("black", "#f9834a"),
                    name = "Within $10k?") + 
  scale_alpha_manual(values = c(0.2, 0.8)) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  guides(alpha = "none") +
  labs(x = "Predicted \n Renter Renter Income",
       y = "Actual \n Median Renter Income",
       title = "Random Forest Predictions") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5))
)

ggsave(here("00_2305_Kentucky/figs/rf_rentpred.png"),
       plot = rf_fig,
       device = "png",
       dpi = 300)


rf_percpred_inband <- find_percpred_inband(final_rf_fit %>%
                                collect_predictions(), 10000)

#----------------------------------------------------------
# Step 6: summarize across models
#---------------------------------------

summary_performance <- data.frame(model = c("lasso", "rf"),
                                  perc_within10k = 
                                  c(lr_percpred_inband,
                                    rf_percpred_inband))

log_print("Comparing the two classifiers, we see the following performance------------------",
          hide_notes = TRUE)
print(summary_performance)

# Joint plot for results 
summary_fig <- lasso_fig + rf_fig + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
ggsave(here("00_2305_Kentucky/figs/rf_lasso_rentpred.png"),
       plot = summary_fig,
       device = "png",
       dpi = 300)



#----------------------------------------------------------
# Step 7: compare predictive accuracy across states for RF
#---------------------------------------

# get orig data
orig_df <- split$data %>%
        mutate(row = 1:n()) 

# rowbind predictions back with original data
test_set_preds <- final_rf_fit %>%
  collect_predictions() %>%
  left_join(orig_df,
            by = c(".row" = "row"),
            suffix = c("_frommod", "_orig"))

# Check by-state accuracy for KY, 
# similar to other states.
test_set_preds %>% 
  group_by(state_id) %>% 
  summarize(in_10k = mean(abs(.pred - rent_inc_frommod < 10000)),
            in_1sd = mean(abs(.pred - rent_inc_frommod < sd(zcta_forpred$rent_inc))),
            n = n()) %>% 
  mutate(ky_first = ifelse(state_id == "21", 1, 0)) %>% 
  arrange(desc(ky_first))


#----------------------------------------------------------
# Step 7: write parameters for best performing model
# and the model object itself to generate predictions
#---------------------------------------


# write the best-performing model for use to 
# impute missing renter income
write.csv(rf_best, paste0(BASE_DIR, "2305_intermediate_data/", 
                              "rf_best_specs.csv"))

## write rf model objects
saveRDS(final_rf_fit, paste0(BASE_DIR, "2305_intermediate_data/",
                             "final_rf_fit.RDS"))

saveRDS(last_rf_mod, paste0(BASE_DIR, "2305_intermediate_data/",
                            "last_rf_mod.RDS"))


#----------------------------------------------------------

log_close()
