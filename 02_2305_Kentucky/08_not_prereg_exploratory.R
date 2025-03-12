######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Exploratory analyses that were not pre-registered
#---------------------------------------------------------------------
rm(list = ls())

library(tidyverse)       # for data manipulation
library(knitr)
library(estimatr)
library(zoo)
source("00_2301_misc/analysis_helpers.R")

BASE_DIR <- "G:/Shared drives/OES data 2305 ERA Grantee Flexibilities KY/"
df <- read_csv(paste0(BASE_DIR, "2305_clean_data/analytic_applevel.csv"))


# Code new variables ------------------------------------------------------

# Code detailed race variable that breaks out white into white-only and 
# latino / hispanic white
df <- df %>% 
  mutate(race_eth = 
           case_when(
             race == "White" & ethnicity == "Hispanic/Latino" ~ "Hispanic/Latino White",
             race == "White" & ethnicity == "Non-Hispanic/Non-Latino" ~ "White only",
             race == "White" & ethnicity == "Refuse to Answer" ~ NA_character_,
             race == "Refuse to Answer" ~ NA_character_,
             race != "White" & race != "Refuse to Answer" ~ race
           ), 
         
         asian = race == "Asian",
         
         # Code variable for different payment outcomes
         app_outcome = 
           case_when(
             status == "Applications Withdrawn" ~ "Withdrawn",
             status == "Application Reopened" ~ "Never paid",
             status == "Applications Denied" ~ "Denied",
             status == "Applications Denied - Suspected Fraud" ~ "Denied",
             status == "Applications in Review" ~ "Never paid",
             status == "Applications Incomplete" ~ "Never paid",
             status == "Applications Ready for Review Assigned" ~ "Never paid",
             status == "Applications ReSubmitted" ~ "Never paid",
             status == "Applications Withdrawn" ~ "Withdrawn",
             status == "EDP - In Review" ~ "Never paid",
             status == "EDP - Paid and Closed" ~ "Paid",
             status == "EDP - Ready for QC" ~ "Never paid",
             status == "File Review Complete" ~ "Never paid",
             status == "Landlord Refusal" ~ "Never paid",
             status == "Landlord Refusal - In Review" ~ "Never paid",
             status == "Landlord Refusal - Paid and Closed" ~ "Paid",
             status == "Payment Sent to Landlord" ~ "Paid",
             status == "Preview - Batch Updated" ~ "Never paid",
             status == "Special Review Required" ~ "Never paid",
             status == "Special Review Required - Batch Updated" ~ "Never paid"
           ), 
         
         app_denied = app_outcome == "Denied",
         app_never_paid = app_outcome == "Never paid",
         app_paid = app_outcome == "Paid",
         app_withdrawn = app_outcome == "Withdrawn", 
         
         app_processed = case_when(
           app_denied | app_paid ~ TRUE,
           app_never_paid ~ FALSE,
           app_withdrawn ~ NA
         ),
         
         month_year = received_dt %>% as.Date() %>% format("%Y-%m"), 
         week = received_dt %>% as.Date() %>% format("%w-%Y"), 
         
         app_fraud = status == "Applications Denied - Suspected Fraud", 
         
         fsp_flag_1 = inc_eli_by_fb_proxy == "Yes", 
         fsp_flag_2 = income_doc_submitted == "Income Eligible by Fact Proxy", 
         fsp_flag_1_or_2 = fsp_flag_1 | fsp_flag_2
         
         
  )


# Descriptive analysis of main groups -------------------------------------

df$poc
df$rural
df$x_low_income


non_fsp_only <- rbind(
  df %>% 
    filter(fsp == 0) %>% 
    group_by(poc) %>% 
    summarize(approval_rate = mean(approval, na.rm = TRUE), 
              n = n()) %>% 
    mutate(
      facet = "poc",
      labels = case_when(
        is.na(poc) ~ "No response",
        poc ~ "Person of\ncolor", 
        !poc ~ "Not person\nof color"), 
      color = case_when(
        is.na(poc) ~ "#757575",
        poc ~ "#757575", 
        !poc ~ "#000000") 
    )%>% 
    ungroup() %>% 
    select(-poc), 
  df %>% 
    filter(fsp == 0) %>% 
    group_by(rural) %>% 
    summarize(approval_rate = mean(approval, na.rm = TRUE), 
              n = n()) %>% 
    mutate(
      facet = "rural",
      labels = case_when(
        is.na(rural) ~ "No response",
        rural ~ "Rural", 
        !rural ~ "Not rural"), 
      color = case_when(
        is.na(rural) ~ "#757575",
        rural ~ "#757575", 
        !rural ~ "#000000") 
    )%>% 
    ungroup() %>% 
    select(-rural), 
  
  df %>% 
    filter(fsp == 0) %>% 
    group_by(x_low_income) %>% 
    summarize(approval_rate = mean(approval, na.rm = TRUE), 
              n = n()) %>% 
    mutate(
      facet = "x_low_income",
      labels = case_when(
        is.na(x_low_income) ~ "No response",
        x_low_income ~ "Extremely\nlow income", 
        !x_low_income ~ "Not extremely\nlow income"), 
      color = case_when(
        is.na(x_low_income) ~ "#757575",
        x_low_income ~ "#757575", 
        !x_low_income ~ "#000000") 
    ) %>% 
    ungroup() %>% 
    select(-x_low_income)
) %>% 
  mutate(labels = factor(x = labels, 
                         levels = c("No response", 
                                    "Not extremely\nlow income",
                                    "Not rural", 
                                    "Not person\nof color", 
                                    "Extremely\nlow income", 
                                    "Rural", 
                                    "Person of\ncolor"
                                    )))

all_subgroups <- rbind(
  df %>% 
    group_by(poc) %>% 
    summarize(approval_rate = mean(approval, na.rm = TRUE), 
              n = n()) %>% 
    mutate(
      facet = "poc",
      labels = case_when(
        is.na(poc) ~ "No response",
        poc ~ "Person of\ncolor", 
        !poc ~ "Not person\nof color"), 
      color = case_when(
        is.na(poc) ~ "#757575",
        poc ~ "#757575", 
        !poc ~ "#000000") 
    )%>% 
    ungroup() %>% 
    select(-poc), 
  df %>% 
    filter(fsp == 0) %>% 
    group_by(rural) %>% 
    summarize(approval_rate = mean(approval, na.rm = TRUE), 
              n = n()) %>% 
    mutate(
      facet = "rural",
      labels = case_when(
        is.na(rural) ~ "No response",
        rural ~ "Rural", 
        !rural ~ "Not rural"), 
      color = case_when(
        is.na(rural) ~ "#757575",
        rural ~ "#757575", 
        !rural ~ "#000000") 
    )%>% 
    ungroup() %>% 
    select(-rural), 
  
  df %>% 
    filter(fsp == 0) %>% 
    group_by(x_low_income) %>% 
    summarize(approval_rate = mean(approval, na.rm = TRUE), 
              n = n()) %>% 
    mutate(
      facet = "x_low_income",
      labels = case_when(
        is.na(x_low_income) ~ "No response",
        x_low_income ~ "Extremely\nlow income", 
        !x_low_income ~ "Not extremely\nlow income"), 
      color = case_when(
        is.na(x_low_income) ~ "#757575",
        x_low_income ~ "#757575", 
        !x_low_income ~ "#000000") 
    ) %>% 
    ungroup() %>% 
    select(-x_low_income)
) %>% 
  mutate(labels = factor(x = labels, 
                         levels = c("No response", 
                                    "Not extremely\nlow income",
                                    "Not rural", 
                                    "Not person\nof color", 
                                    "Extremely\nlow income", 
                                    "Rural", 
                                    "Person of\ncolor"
                         )))
  
colors <- non_fsp_only %>% filter(labels != "No response") %>% 
  pull(color)

names(colors) <- non_fsp_only %>% filter(labels != "No response") %>% 
  pull(labels)

# pre-calculate difference gaps
dpoc <- non_fsp_only$approval_rate[non_fsp_only$labels == "Person of\ncolor"] -
  non_fsp_only$approval_rate[non_fsp_only$labels == "Not person\nof color"]

dr <- non_fsp_only$approval_rate[non_fsp_only$labels == "Rural"] -
  non_fsp_only$approval_rate[non_fsp_only$labels == "Not rural"]

deli <- non_fsp_only$approval_rate[non_fsp_only$labels == "Extremely\nlow income"] -
  non_fsp_only$approval_rate[non_fsp_only$labels == "Not extremely\nlow income"]

fct_text <- tibble(facet = c("poc", "rural", "x_low_income"),
       labels = c("Person of\ncolor", "Rural", "Extremely\nlow income"),
       approval_rate = c(0.55, 0.46, 0.49),
       label = c(dpoc, dr, deli) %>% round(3) * 100) %>%  
  mutate(label = ifelse(label > 0, paste0("+", label, "pp"),
                        ifelse(label < 0, paste0(label, "pp"), NA)))

(subgroups_non_fsp_plot <- 
  ggplot(data = non_fsp_only %>% filter(labels != "No response"), 
       aes(x = labels, group = facet, 
           y = approval_rate, 
           fill = labels)) +
  geom_bar(stat = "identity", 
           width = .8,
           position = position_dodge(width = .5, preserve = "total")) +
  geom_text(data = fct_text,
            aes(label= label),
            position=position_dodge(width=0.2),
            fontface = "bold") +
  facet_wrap(~facet, scales = "free_x") +
  scale_fill_manual(values = colors, guide = "none") + 
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", labels = scales::percent) +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        # panel.border = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.spacing.y = unit(0, "cm"),
        # legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)))

subgroups_all_plot <- 
  ggplot(data = all_subgroups %>% filter(labels != "No response"), 
         aes(x = labels, group = facet, 
             y = approval_rate, 
             fill = labels)) +
  geom_bar(stat = "identity", 
           width = .8,
           position = position_dodge(width = .5, preserve = "total")) +
  facet_wrap(~facet, scales = "free_x") +
  scale_fill_manual(values = colors, guide = "none") + 
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", labels = scales::percent) +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        # panel.border = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.spacing.y = unit(0, "cm"),
        # legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  
ggsave(filename = paste0(BASE_DIR, "2305_clean_data/results/subgroup_approval_rates_non_fsp.png"), 
       plot = subgroups_non_fsp_plot,
       width = 4, height = 2.5)
  
ggsave(filename = paste0(BASE_DIR, "2305_clean_data/results/subgroup_approval_rates_all.png"), 
       plot = subgroups_all_plot,
       width = 4, height = 2)




# Timeline graphs ---------------------------------------------------------

# Total applications x fsp over time
# proportion paid x fsp over time

min_day <- df %>% pull(received_dt) %>% as.Date() %>% min
max_day <- df %>% pull(received_dt) %>% as.Date() %>% max

k <- 50

days <- 
  data.frame(day = seq(from = min_day, to = max_day, by = "day")) %>% 
  left_join(
    y = df %>% mutate(day = received_dt %>% as.Date()) %>% 
      group_by(day, fsp) %>% 
      summarize(
        n_apps = n(),
        prop_paid = mean(approval, na.rm = TRUE), 
        prop_fraud = mean(app_fraud, na.rm = TRUE), 
        init_doc = mean(inc_doc_sub_initial, na.rm = TRUE),
        days_pay = mean(days_to_payment, na.rm = TRUE), 
        fsp_flag = mean(fsp_flag_1_or_2, na.rm = TRUE)
      ),
    by = "day"
  ) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  group_by(fsp) %>% 
  mutate(
    n_apps_rollmean = rollmean(n_apps, k = k, fill = NA), 
    prop_paid_rollmean = rollmean(prop_paid, k = k, fill = NA), 
    prop_fraud_rollmean = rollmean(prop_fraud, k = k, fill = NA), 
    init_doc_rollmean = rollmean(init_doc, k = k, fill = NA),
    days_pay_rollmean = rollmean(days_pay, k = k, fill = NA),
    fsp_flag_rollmean = rollmean(fsp_flag, k = k, fill = NA)
  ) 


fsp_implemented <- as.Date("2021-06-01")

# Overall number of applications
days %>%
  ggplot(aes(x = day, y = n_apps_rollmean, group = fsp, linetype = fsp)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")

# For abstract:
(timeline_for_abs <- days %>%
  ggplot(aes(x = day, y = n_apps_rollmean,
             group = fsp, linetype = fsp)) +
  geom_line(size = 0.75) +
  annotate("label", x = as.Date("2022-02-20"), y = 125,
           label = "FSP ZIPs", fontface = "bold") +
  annotate("label", x = as.Date("2022-02-20"), y = 20,
            label = "Non-FSP ZIPs", fontface = "bold") +
  scale_x_date(name = "", date_breaks = "6 months") + 
  theme_bw() +
  # geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications\n(rolling mean)") +
  scale_linetype_manual(guide = "none",
                          values = c("TRUE" = "dashed", "FALSE" = "solid")) + 
  theme(legend.position = "bottom") + 
  theme(panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.spacing.y = unit(0, "cm"),
        # legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 13)))

ggsave(filename = paste0(BASE_DIR, "2305_clean_data/results/apps_by_day.png"), 
       plot = timeline_for_abs,
       width = 6, height = 3)

# Proportion of applications paid
days %>%
  ggplot(aes(x = day, y = prop_paid_rollmean, group = fsp, linetype = fsp)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "% applications paid") +
  theme(legend.position = "bottom")

# Proportion of applications with initial income documentation
days %>%
  ggplot(aes(x = day, y = init_doc_rollmean, group = fsp, linetype = fsp)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "% applications with income doc") +
  theme(legend.position = "bottom")

# Average days to payment
days %>%
  ggplot(aes(x = day, y = days_pay_rollmean, group = fsp, linetype = fsp)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "number of days to payment") +
  theme(legend.position = "bottom")

# Proportion of applications denied due to suspected fraud
days %>%
  ggplot(aes(x = day, y = prop_fraud_rollmean, group = fsp, linetype = fsp)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "% denied for suspected fraud") +
  theme(legend.position = "bottom")


# Proportion of applications that used fsp flag
days %>%
  ggplot(aes(x = day, y = fsp_flag_rollmean, group = fsp, linetype = fsp)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "% applications flagged as having benefitted from FSP", limits = c(0,1)) +
  theme(legend.position = "bottom")


# Demographics over time

days_race <-  
  data.frame(day = seq(from = min_day, to = max_day, by = "day")) %>% 
  left_join(
    y = df %>% 
      ungroup() %>% 
      mutate(day = received_dt %>% as.Date()) %>% 
      group_by(day, race_eth) %>% 
      summarize(
        n_apps = n()
      ),
    by = "day"
  ) %>% 
  filter(!is.na(race_eth)) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  group_by(race_eth) %>% 
  mutate(
    n_apps_rollmean = rollmean(n_apps, k = k, fill = NA)
  ) 

days_race %>%
  filter(race_eth %in% c("White only", "Black or African American")) %>% 
  ggplot(aes(x = day, y = n_apps_rollmean, group = race_eth, linetype = race_eth)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")

days_race %>%
  filter(!race_eth %in% c("White only", "Black or African American")) %>% 
  ggplot(aes(x = day, y = n_apps_rollmean, group = race_eth, linetype = race_eth, color = race_eth)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")



days_gender <-  
  data.frame(day = seq(from = min_day, to = max_day, by = "day")) %>% 
  left_join(
    y = df %>% 
      ungroup() %>% 
      mutate(day = received_dt %>% as.Date()) %>% 
      group_by(day, gender) %>% 
      summarize(
        n_apps = n()
      ),
    by = "day"
  ) %>% 
  filter(!is.na(gender)) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(
    n_apps_rollmean = rollmean(n_apps, k = k, fill = NA)
  ) 

days_gender %>%
  filter(gender %in% c("Female", "Male")) %>% 
  ggplot(aes(x = day, y = n_apps_rollmean, group = gender, linetype = gender)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")

days_gender %>%
  filter(!gender %in% c("Female", "Male")) %>% 
  ggplot(aes(x = day, y = n_apps_rollmean, group = gender, linetype = gender, color = gender)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")


days_disability <-  
  data.frame(day = seq(from = min_day, to = max_day, by = "day")) %>% 
  left_join(
    y = df %>% 
      ungroup() %>% 
      mutate(day = received_dt %>% as.Date()) %>% 
      group_by(day, disability) %>% 
      summarize(
        n_apps = n()
      ),
    by = "day"
  ) %>% 
  filter(!is.na(disability)) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  group_by(disability) %>% 
  mutate(
    n_apps_rollmean = rollmean(n_apps, k = k, fill = NA)
  ) 

days_disability %>%
  filter(!disability %in% c("Refuse to Answer", "No")) %>% 
  ggplot(aes(x = day, y = n_apps_rollmean, group = disability, linetype = disability)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")



days_veteran <-  
  data.frame(day = seq(from = min_day, to = max_day, by = "day")) %>% 
  left_join(
    y = df %>% 
      ungroup() %>% 
      mutate(day = received_dt %>% as.Date()) %>% 
      group_by(day, veteran) %>% 
      summarize(
        n_apps = n()
      ),
    by = "day"
  ) %>% 
  filter(!is.na(veteran)) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  group_by(veteran) %>% 
  mutate(
    n_apps_rollmean = rollmean(n_apps, k = k, fill = NA)
  ) 

days_veteran %>%
  filter(!veteran %in% c("Refuse to Answer", "No")) %>% 
  ggplot(aes(x = day, y = n_apps_rollmean, group = veteran, linetype = veteran)) +
  geom_line(size = 1) +
  scale_x_date(name = "", date_breaks = "4 months") + 
  theme_bw() +
  geom_vline(xintercept = fsp_implemented) +
  scale_y_continuous(name = "Number of applications") +
  theme(legend.position = "bottom")



# Descriptive analysis of documentation burdens ---------------------------

# Now look at gaps in burdens
assess_admin_burdens <- function(var_name, dat = df){
  dat$group_var <- dat %>% pull(var_name)
  dat %>% 
    mutate(N = n()) %>%
    filter(!is.na(group_var)) %>% 
    group_by(group_var) %>% 
    summarize(
      n = n(),
      prop_total = n/unique(N),
      inc_doc_initial = mean(inc_doc_sub_initial, na.rm = TRUE),
      inc_doc_verified = mean(inc_doc_verified, na.rm = TRUE),
      processed = mean(app_processed, na.rm = TRUE), 
      paid = mean(app_paid, na.rm = TRUE), 
      never_paid = mean(app_never_paid, na.rm = TRUE)
    ) %>% 
    arrange(-prop_total) 
}




# First: establish some facts about the importance of initial document submission

# There is no paid application where income is not verified:
with(df, table(inc_doc_verified, app_paid, useNA = "always"))

# Initial income doc being submitted increases prob of having income verified
# and being paid. This is true at every month in the program and in every county
(m1 <- lm_robust(app_paid ~ inc_doc_sub_initial, df))
(m2 <- lm_robust(app_paid ~ inc_doc_sub_initial, fixed_effects = ~ county_name_touse, df))
(m3 <- lm_robust(app_paid ~ inc_doc_sub_initial, fixed_effects = ~ county_name_touse + month_year, df))
(m4 <- lm_robust(app_paid ~ inc_doc_sub_initial + fsp, fixed_effects = ~ county_name + month_year, df))

# Descriptive stats on Race
# Looking just at pre-fsp period
desc_race <- assess_admin_burdens("race_eth", dat = df %>% filter(pre_post== 0)) 

desc_race$plot_order <- order(desc_race$inc_doc_initial)

plot_dat_race <- 
  with(desc_race %>% arrange(inc_doc_initial),
       data.frame(
         race_eth = factor(c(group_var, group_var),levels = group_var, ordered = TRUE), 
         statistic = c(rep(x = "Initial income\ndocumentation accepted", nrow(desc_race)), 
                       rep(x = "Application paid", nrow(desc_race))), 
         estimate = c(inc_doc_initial, paid)
       )  )

ggplot(plot_dat_race, 
       aes(y = estimate, 
           group = statistic, 
           x = race_eth, 
           fill = statistic)) +
  geom_bar(position = "dodge", stat = "identity", width = .8) +
  scale_y_continuous(name = "",limits = c(0,1), labels = scales::percent) +
  scale_x_discrete("") +
  geom_text(aes(x = race_eth, y = estimate + .05, 
                label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(width = .8)) +
  coord_flip() +
  scale_fill_discrete(name = "") +
  theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) 


# Regression of "paid" on timing and race
m3 <- lm_robust(app_paid ~  race_eth, df %>% filter(pre_post== 0), fixed_effects = ~county_name_touse + month_year)
m4 <- lm_robust(app_paid ~  race_eth + inc_doc_sub_initial, df %>% filter(pre_post== 0), fixed_effects = ~county_name_touse + month_year)

# Tables 
assess_admin_burdens("race_eth") 
assess_admin_burdens("gender") 
assess_admin_burdens("disability")
assess_admin_burdens("veteran")
