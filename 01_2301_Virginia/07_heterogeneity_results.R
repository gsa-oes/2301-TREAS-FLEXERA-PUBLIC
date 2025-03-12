######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Plot subgroup results from ZIP and individual models.
# 8_heterogeneity_results.R
#---------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(gt)

zip <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/hetero_zip_level.rds")
indiv <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/hetero_indiv_level.rds")

bind_rows(bind_rows(zip),
          bind_rows(indiv)) %>% 
  relocate(model:group, .before = term) %>% 
  select(-outcome, -term) %>% 
  arrange(model, column, group) %>% 
  mutate(across(is.numeric, round, 3),
         ci = paste0("[", conf.low, ", ", conf.high, "]")) %>% 
  select(-conf.low, -conf.high, -df) %>% 
  gt() %>% 
  cols_label(
    model = md("**Outcome**"),
    column = md("**Characteristic**"),
    group = md("**Group**"),
    estimate = md("**estimate**"),
    std.error = md("**SE**"),
    statistic = md("**Test Stat.**"),
    p.value = md("**pval**"),
    ci = md("**CI**")
  )
