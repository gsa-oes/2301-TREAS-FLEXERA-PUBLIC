######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Confirmatory Analysis Robustness
# to Non-Linear Models
# 
# Runs confirmatory analyses with poisson, negative binomial, and
# zero-inflated models.
#---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(MASS) # glm.nb
library(pscl) # zeroinfl
library(AER)  # overdispersion test

df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds")

run_nonlinear_models <- function(outcome = "main") {
  
  if (outcome == "main") {
    df$out_var <- df$n_app_post
    df$pre_var <- df$n_app_pre
  }
  
  if (outcome == "hh_all") {
    df$out_var <- df$n_app_post_hh_all
    df$pre_var <- df$n_app_pre_hh_all
  }
  
  #---------------------------------
  # Poisson
  # Notable assumptions:
  #   1. Outcome variance equal to the mean
  #   2. Linear relationship
  
  m1 <- glm(out_var ~ fsp + med_inc + pre_var, 
            family="poisson", data=df)
  
  m1df <- tidy(m1) %>% 
    mutate(mod = "Poisson")
  
  #---------------------------------
  # Negative Binomial
  # Notable assumptions:
  #   1. Outcome variance does not need to equal the mean
  #   2. Linear relationship
  m2 <- glm.nb(out_var ~ fsp + med_inc + pre_var,
         data = df)
  
  m2df <- tidy(m2) %>% 
    mutate(mod = "Neg. Bin")
  
  #---------------------------------
  # Zero-inflated Poisson / Negative Binomial
  # Notable assumptions:
  #   1. Those above, plus
  #   2. Excess zeros are generated from separate process
  
  m3 <- zeroinfl(out_var ~ fsp + pre_var | pre_var,
                 data = df, dist = "poisson")
  
  m3df <- tidy(m3) %>% 
    mutate(mod = "Zero-Infl. Poisson")
  
  m4 <- zeroinfl(out_var ~ fsp + pre_var | pre_var,
                 data = df, dist = "negbin")
  
  m4df <- tidy(m4) %>% 
    mutate(mod = "Zero-Infl. NB")

  bind_rows(m1df, m2df, m3df, m4df) %>% 
    mutate(outcome = outcome)
  
}


df_res <- run_nonlinear_models(outcome = "main") %>% 
  bind_rows(run_nonlinear_models(outcome = "hh_all")) %>% 
  filter(term == "fsp")

strip_data <- tibble(xpos = 1:2,
                     xmin = xpos - 0.5,
                     xmax = xpos + 0.5,
                     ymin = 0,
                     ymax = 100,
                     fill = c(rep(c("a", "b"), length.out=2)))

df_res <- df_res %>% 
  mutate(outcome_n = factor(outcome, 
                            levels = rev(unique(df_res$outcome))),
         outcome_n = as.numeric(outcome_n))

df_res %>% 
  ggplot() +
  geom_rect(data = strip_data, inherit.aes = FALSE,
            aes(xmin=xmin, xmax=xmax, 
                ymin=-1, ymax=1, fill = fill)) +
  geom_errorbar(aes(x = outcome_n, 
                    y = estimate, 
                    ymin = estimate-1.96*std.error,
                    ymax = estimate+1.96*std.error,
                    group = mod, color = p.value < 0.05,
                    width = 0.5),
                position = position_dodge2(width = 1)) +
  geom_point(aes(x = outcome_n, 
                 y = estimate, 
                 shape = outcome,
                 color = p.value < 0.05), fill = NA,
             position = position_dodge2(width = 1)) +
  facet_wrap(~mod) +
  scale_fill_manual(values = c("#f7e0ad", "white")) +
  scale_color_manual(values = c("#adadad", "black")) +
  scale_x_continuous(expand = c(0,0),
                     breaks = df_res$outcome_n,
                     labels = df_res$outcome) +
  coord_flip() + 
  geom_hline(yintercept = 0, col = "red", lty = "dashed") + 
  theme_bw() + 
  guides(fill = "none")
  
# Test for overdispersion
m1 <- glm(n_app_post ~ fsp + med_inc + n_app_pre, 
          family="poisson", data=df)
dispersiontest(m1,trafo=1)
