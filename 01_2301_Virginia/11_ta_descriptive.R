######################################################################
#---------------------------------------------------------------------
# 2301: ERA Flexibilities
# 
# Descriptive Analysis for Technical Appendix
# ta_descriptive.R
# 
# Calculates descriptive statistics for technical appendix.
#---------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(DescTools)
library(gt)
library(patchwork)

df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/merged_precollapse.rds")
zctas_to_drop <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_01_intermediate_data/zctas_to_drop.rds")
app_df <- read_rds("G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_02_clean_data/zip_collapse.rds") %>% 
  select(zip, med_inc, n_app_post, total_paid_post)

# Histograms for log vs. non-logged outcomes
app_df %>% 
  mutate(logcount = log1p(n_app_post),
         logpaid  = log1p(total_paid_post)) %>% 
  pivot_longer(cols = n_app_post:logpaid) %>%
  mutate(name = case_when(name == "logcount" ~ "Log(App. Count + 1)",
                          name == "logpaid" ~ "Log(Total Paid + 1)",
                          name == "n_app_post" ~ "App. Count",
                          name == "total_paid_post" ~ "Total Paid"),
         name = factor(name,
                       levels = c("App. Count", "Total Paid",
                                  "Log(App. Count + 1)",
                                  "Log(Total Paid + 1)"))) %>% 
  ggplot(aes(x = value)) + 
    geom_histogram(fill = "#c64e2a") + 
    facet_wrap(~name, scales = "free_x") + 
    theme_bw() + 
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))

# Merge income limits
df <- df %>% 
  mutate(zip = as.character(df$`Property Zipcode`)) %>% 
  left_join(app_df, by = "zip")

demog_table <- function(var, name) {
  
  demog_var <- df %>% pull(var)
  
  cat("Recoding", 
      sum(is.na(demog_var)), 
      "missing values to NA.", 
      "\n")

  demog_var <- ifelse(is.na(demog_var), 
                      glue::glue(name, ": Not Reported"),
                                 demog_var)
  
  cat("Making Table...")
  
  # Total
  full_tab <- df %>% 
    mutate(demog = demog_var) %>% 
    group_by(demog) %>% 
    summarize(app_count = n(),
              approved = sum(str_detect(Status, "APPROVED")),
              approv_rate = (approved / app_count) * 100,
              approv_rate = paste0(round(approv_rate, 1), "%"),
              avg_hh_size = round(
                mean(`Household Size`, na.rm = TRUE), 1
                ))
  
  # Merge on Approved Number
  app_count_df <- full_tab %>% select(demog, app_count)
  
  # Eligible for FSP
  fsp_tab <- df %>% 
    mutate(demog = demog_var,
           processing_date_clean = 
             most_recent_status_date %>% 
             substr(1, 10) %>% 
             mdy()) %>% 
    filter(!(`Property Zipcode` %in% zctas_to_drop),
           `Household Size` <= 3,
           processing_date_clean >= mdy("6/10/2021"),
           med_inc < 66950) %>% 
    group_by(demog) %>% 
    summarize(fsp_app_count = n()) %>% 
    left_join(app_count_df, by = "demog") %>% 
    mutate(fsp_app_prop = fsp_app_count / app_count) %>% 
    select(-app_count)

  full_tab %>% 
    left_join(fsp_tab, by = "demog") %>% 
    mutate(demog = tools::toTitleCase(tolower(demog)),
           fsp_app_prop = paste0(round(fsp_app_prop * 100, 1), "%")) %>%
    select(-fsp_app_count)
}

# Recoding
df$Disability <- ifelse(df$`Does Applicant Have Disability`, "Disability", "No Disability")
df$Veteran <- ifelse(df$`Is Applicant Veteran`, "Veteran", "Not Veteran")

t1 <- demog_table(var = "Applicant Race", name = "Race")
t2 <- demog_table(var = "Applicant Ethnicity", name = "Ethnicity")
t3 <- demog_table(var = "Disability", name = "Disability Status")
t4 <- demog_table(var = "AMI Bucket", name = "AMI")
t5 <- demog_table(var = "Veteran", name = "Veteran Status")

t4 <- t4 %>% 
  mutate(demog = gsub("Ami", "AMI", demog),
         demog = ifelse(demog == "over Income",
                        "Over Income Threshold", demog))

full_table <- bind_rows(t1, t2, t3, t4, t5) 

colnames(full_table)
  
# Column tables for TA table
# colnames(full_table) <- c("Group", "# App. Submitted", 
#                           "# App. Approved",
#                           "Approval Rate",
#                           "Avg. Household Size",
#                           "% App. FSP Eligible")

full_table %>%
  mutate(app_count = prettyNum(app_count, big.mark = ","),
         approved = prettyNum(approved, big.mark = ",")) %>%  
  gt() %>% 
  cols_label(
    demog = md("**Group**"),
    app_count = md("**# App. Submitted**"),
    approved = md("**# App. Approved**"),
    approv_rate = md("**Approval Rate**"),
    avg_hh_size = md("**Avg. HH Size**"),
    fsp_app_prop = md("**% App. FSP Eligible**")
  ) 
