######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Exploratory Analysis Code
# 
# Conducts exploratory analyses
# 
# All steps correspond to 2301 Analysis Plan,
# pg. 18-19
#---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(estimatr)
library(rdrobust)
library(broom)
library(knitr)

df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds")

# Source in helper functions
source("00_2301_misc/analysis_helpers.R")

# Applications from Underserved Groups ------------------------------------

# Loop estimator through all demographic groups
demographic_group_ests <- 
  lapply(X = c("n_app_ami_0_30",
             "n_app_ami_31_50",
             "n_app_ami_51_80",
             "n_app_white",
             "n_app_black_or_aa",
             "n_app_asian",
             "n_app_nhpi",
             "n_app_aian",
             "n_app_multi",
             "n_app_latinx",
             "n_app_not_latinx",
             "n_app_veteran",
             "n_app_disability"),
       FUN = get_rdd_afc_ests, 
       df = df) %>% 
  do.call(what = rbind, args = .)


# Processing times --------------------------------------------------------

proc_time_rdd <- rdrobust(y = df$proc_time_hh_3_win, 
                          x = df$running_variable, 
                          c = 0, 
                          bwselect = "mserd")

df$proc_time_hh_3_win <- as.numeric(df$proc_time_hh_3_win)

proc_time_lm <- lm_robust(
  formula = proc_time_hh_3_win ~ fsp + med_inc, 
  data = df) 

oes_prep(proc_time_lm, treatment_vars = "fsp") %>%
  mutate(term = ifelse(term == "Treatment", "FSP", "No FSP"),
         estimate = round(estimate, 2)) %>% 
  oes_plot() + 
  labs(title = "Days Spent Processing",
       y = NULL) + 
  theme(plot.title = element_text(face = "bold"))

df_res <- lapply(X = c("proc_time_hh_3_win",
             "proc_time_hh_3",
             "proc_time_status_all",
             "proc_time_status_all_win",
             "proc_time_status_noninactive",
             "proc_time_status_noninactive_win",
             "proc_time_status_inactive",
             "proc_time_status_inactive_win",
             "proc_time_status_list",
             "proc_time_status_list_win"
             ),
       FUN = get_proc_time_ests, 
       df = df) %>% 
  do.call(what = rbind, args = .)

df_res <- df_res %>% 
  mutate(outcome_n = factor(outcome, 
                          levels = rev(unique(outcome))),
       outcome_n = as.numeric(outcome_n))

strip_data <- tibble(xpos = 1:10,
                     xmin = xpos - 0.5,
                     xmax = xpos + 0.5,
                     ymin = 0,
                     ymax = 10,
                     fill = c(rep(c("a", "b"), length.out=10)))

df_res %>% 
  ggplot() +
  geom_rect(data = strip_data, inherit.aes = FALSE,
            aes(xmin=xmin, xmax=xmax,
                ymin=-150, ymax=150, fill = fill)) +
  geom_errorbar(aes(x = outcome_n, 
                    y = estimate, 
                    ymin = estimate-1.96*std.error,
                    ymax = estimate+1.96*std.error,
                    group = model, color = p.value < 0.05),
                position = position_dodge2(width = 1)) +
  geom_point(aes(x = outcome_n, 
                 y = estimate, 
                 group = model, shape = model,
                 color = p.value < 0.05), fill = NA,
             position = position_dodge2(width = 1)) +
  scale_fill_manual(values = c("#f7e0ad", "white")) +
  scale_color_manual(values = c("#adadad", "black")) +
  scale_x_continuous(expand = c(0,0),
                     breaks = df_res$outcome_n,
                     labels = gsub(pattern = "proc_time_", "", df_res$outcome)) +
  coord_flip() + 
  geom_hline(yintercept = 0, col = "red", lty = "dashed") + 
  theme_bw() + 
  guides(fill = "none")


proc_time_rdd %>% tidy %>% kable(x = ., format = "simple", digits = 3)
proc_time_lm %>% tidy %>% kable(x = ., format = "simple", digits = 3)

# save models for coef plot
t1 <- tidy(proc_time_rdd) %>% mutate(model = "RD")
t2 <- tidy(proc_time_lm) %>% mutate(model = "AFC")

mod_proc <- bind_rows(t1, t2) %>% 
  mutate(outcome = "proc_time")
write_rds(mod_proc, "D:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/proc.rds")

# total paid --------------------------------------------------------------

df$log_paid_post <- log1p(df$total_paid_post)
df$log_paid_pre <- log1p(df$total_paid_pre)

total_paid_rdd <- rdrobust(y = df$log_paid_post, 
                           x = df$running_variable, 
                           c = 0,
                           covs = df$log_paid_pre,
                           bwselect = "mserd")


total_paid_lm <- lm_robust(
  formula = log_paid_post ~ fsp + med_inc + log_paid_pre, 
  data = df) 

total_paid_rdd %>% tidy %>% kable(x = ., format = "simple", digits = 3)
total_paid_lm %>% tidy %>% kable(x = ., format = "simple", digits = 3)

predict(total_paid_rdd)

# save models for coef plot
t1 <- tidy(total_paid_rdd) %>% mutate(model = "RD")
t2 <- tidy(total_paid_lm) %>% mutate(model = "AFC")

mod_paid <- bind_rows(t1, t2) %>% 
  mutate(outcome = "log_paid")
write_rds(mod_paid, "D:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/log_paid.rds")



# Marginal effects --------------------------------------------------------

# Pull out polynomial predictions from the model used for the rdplot
poly_preds <- rdplot(y = df$log_paid_post, 
                     x = df$running_variable, 
                     c = 0, 
                     covs = df$log_paid_pre)$vars_poly
# Pull the polynomial predictions at the threshold (first is control, second treatment)
poly_preds <- poly_preds$rdplot_y[poly_preds$rdplot_x == 0]
# Exponentiate and subtract 1
poly_preds <- exp(poly_preds) - 1
# Compute predicted difference
poly_preds[2] - poly_preds[1]


# For AFC, use predict function
preds <- predict(total_paid_lm, 
                 newdata = data.frame(fsp = c(1,0), 
                                      med_inc = rep(66950, 2), 
                                      log_paid_pre = rep(mean(df$log_paid_pre),2)))
# Exponentiate
preds <- exp(preds) - 1
# Compute predicted difference
preds[1] - preds[2]











