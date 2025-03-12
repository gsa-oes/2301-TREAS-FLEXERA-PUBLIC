

######################################################################
#---------------------------------------------------------------------
# 2305: ERA Flexibilities
# 
# Coefficient plot 
#---------------------------------------------------------------------

rm(list = ls())

library(cowplot)
library(dplyr)
library(ggplot2)
library(extrafont)
library(stringr)
library(here)


source(here("00_2305_Kentucky/01_analysis/constants.R"))
source(here("00_2305_Kentucky/01_analysis/utils.R"))
options(scipen = 999)



######################################################################
# Step 1: read in model output from parametric models
#-------------------------------------------------------------------

model_outputs <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/summaries_forgraph_ipw_zlevelRDD.csv"))


######################################################################
# Step 2: read in model output from bootstrapped regs 
#-------------------------------------------------------------------

model_outputs_bootstrapped <- read.csv(paste0(BASE_DIR, "2305_intermediate_data/RDD_bootstrap_SEs.csv"))


######################################################################
# Step 3: combine
#-------------------------------------------------------------------

model_outputs_bs_clean <- model_outputs_bootstrapped %>%
              filter(model %in% c("approval - covs",
                                  "fraud - covs") & bootstrap_method == "within-cluster resample") %>% 
              mutate(outcome = case_when(grepl("approval", model) ~ "Approval",
                                         grepl("fraud", model) ~ "Fraud detection"),
                     model = "RD: applicant-level", # consistent model summary
                     unit = "Applicant") %>%
              rename(est = observed_est,
                     ci_lower = lower_ci,
                     ci_upper = upper_ci) %>%
              select(-X, -bootstrap_method, -se) 

model_outputs_all <- rbind.data.frame(model_outputs, model_outputs_bs_clean) %>%
          filter(!outcome %in% c("Total paid", "Total applications"))


######################################################################
# Step 4: prep data to plot 
#-------------------------------------------------------------------

plot_data <- 
  model_outputs_all %>% 
  mutate(
    model_shape = ifelse(grepl("IPTW", model), "IPTW",
                         ifelse(model == "Difference-in-differences", "DiD", "RD")),
    het_group = str_extract(model, "rural|poc|xli"),
    het_group_clean = case_when(het_group == "poc" ~ "Person of color",
                                het_group == "xli" ~ "Extremely low income",
                                het_group == "rural" ~ "Rural",
                                TRUE ~ ""),
    level = factor(unit, levels = c("Zip","Applicant")),
    model_order = factor(model, 
                         levels = c(
                           "IPTW-weighted regression",
                           "RD: applicant-level",
                           "RD: zip-level",
                           "Difference-in-differences"
                         ), ordered = TRUE),
    outcome_label = case_when(outcome == "Approval" ~ "\nChange in probability \n of application approval",
                              outcome == "Approval: heterogeneous effects" ~ "\nAdditional change in \n approval probability for specific groups",
                              outcome == "Fraud detection" ~ "\nChange in probability \n of detecting fraud"),
    outcome_label = factor(
      x = outcome_label, 
      levels = c(
        "\nChange in probability \n of application approval",
        "\nAdditional change in \n approval probability for specific groups",
        "\nChange in probability \n of detecting fraud"
      )
    )
  ) 

# Circle = OLS
# Triangle = RD
# Dark blue = ZIP
# Yellow = individual
# Order: 

(p1 <-
    plot_data %>% 
    # Drop Zip Level Estimate + DiD
    filter(outcome == "Approval",
           level == "Applicant", model != "Difference-in-differences") %>% 
    ggplot(aes(x = est, y = model_order), color = "#966f22") + 
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(size = 3, aes(shape = model_shape)) + 
    annotate("text", x = 0.13258477, y = 2.4,
             label = "+13.2pp", fontface = "bold") +
    annotate("text", x = 0.07504242, y = 0.6,
             label = "+7.5pp", fontface = "bold") +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, height = 0)) +
    facet_wrap(~ outcome_label,
               ncol = 1, scales = "free",
               strip.position = "top") +
    labs(x= NULL, y= "", title="") + 
    scale_y_discrete(limit = rev, expand = c(0.8, 0.8)) +
    # scale_x_continuous(breaks = c(0, 0.05, 0.10, 0.15),
    #                    labels = c("+0pp", "+5pp", "+10pp", "+15pp")) +
    scale_shape_manual(name = "Shape",values= c("IPTW" = 16, "RD" = 17, "DiD" = 15),
                       labels= c("IPTW" = "Weighted Regression (IPTW)",
                                 "RD"= "Regression Discontinuity (RD)")) +
    theme_bw() +
    guides(color = NULL, 
           shape = NULL) +
    theme(panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          # conserve space in PDF
          plot.margin = grid::unit(c(0,0,0,0), "mm"),
          # panel.border = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "left",
          legend.margin = margin(0,0,0,0, unit="cm"),
          legend.box.spacing = margin(0, 0, 0, 0, unit = "cm"),
          legend.spacing.y = unit(0, "cm"),
          # legend.title = element_blank(),
          strip.background = element_blank(),
          #strip.text.x = element_blank(),
          strip.text = element_text(size = 13, face = "bold"),
          strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside",
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(filename = 
         here("00_2305_Kentucky/figs/coef_1.png"), 
       plot = p1, 
       width = 5, 
       height = 3)

ggsave(filename = 
         paste0(BASE_DIR, "2305_clean_data/results/coef_main.png"), 
       plot = p1, 
       width = 5, 
       height = 3)

ggsave(filename = 
         paste0(BASE_DIR, "2305_clean_data/results/coef_main.svg"), 
       plot = p1, 
       width = 5, 
       height = 3)


(p2 <-
    plot_data %>% 
    filter(outcome == "Fraud detection") %>% 
    ggplot(aes(x = est, y = model_order), color = "#966f22") + 
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(size = 2, aes(shape = model_shape)) + 
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, height = 0)) +
    facet_wrap(~ outcome_label,
               ncol = 1, scales = "free",
               strip.position = "top") +
    labs(x= NULL, y= "", title="") + 
    scale_y_discrete(limit = rev, expand = c(0.8, 0.8)) +
    scale_shape_manual(name = "Shape",values= c("IPTW" = 16, "RD" = 17),
                       labels= c("IPTW" = "Inverse-propensity of Treatment Weighted Regression (IPTW)",
                                 "RD"= "Regression Discontinuity (RD)")) +
    theme_bw() +
    guides(color = NULL, 
           shape = NULL) +
    theme(panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          # conserve space in PDF
          plot.margin = grid::unit(c(0,0,0,0), "mm"),
          # panel.border = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "left",
          legend.margin = margin(0,0,0,0, unit="cm"),
          legend.box.spacing = margin(0, 0, 0, 0, unit = "cm"),
          legend.spacing.y = unit(0, "cm"),
          # legend.title = element_blank(),
          strip.background = element_blank(),
          #strip.text.x = element_blank(),
          strip.text = element_text(size = 13, face = "bold"),
          strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside",
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)))

het_plot <- plot_data %>% 
  filter(outcome == "Approval: heterogeneous effects") %>% 
  ggplot(aes(x = est, y = het_group_clean), color = "#966f22") + 
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(size = 2, aes(shape = model_shape)) + 
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, height = 0)) +
  labs(x = NULL, y= "", title="") + 
  scale_y_discrete(limit = rev, expand = c(0.8, 0.8)) +
  scale_shape_manual(name = "Shape",values= c("IPTW" = 16, "RD" = 17),
                   labels= c("IPTW" = "Inverse-propensity of Treatment Weighted Regression (IPTW)",
                         "RD"= "Regression Discontinuity (RD)")) +
  theme_bw() +
  facet_wrap(~ outcome_label,
             ncol = 1, scales = "free",
             strip.position = "top") +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # conserve space in PDF
        plot.margin = grid::unit(c(0,0,0,0), "mm"),
        # panel.border = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.spacing.y = unit(0, "cm"),
        # legend.title = element_blank(),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = NULL, shape = NULL, model_shape = NULL) 




# vertical version 
legend_combined <- get_legend(p2)
grid_nolegend <- plot_grid(het_plot + theme(legend.position = "none"),
                           p2 + theme(legend.position = "none"),
                           ncol = 2,
          rel_widths = c(1.2, 0.8),
          align = "h")
pv <- plot_grid(grid_nolegend, legend_combined, ncol = 1,
                         rel_heights = c(1, 0.1))

ggsave(filename = 
         here("00_2305_Kentucky/figs/coef_2.png"), 
       plot = pv, 
       width = 10, 
       height = 3)
