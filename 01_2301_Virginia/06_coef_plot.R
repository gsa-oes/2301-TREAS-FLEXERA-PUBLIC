library(glue)
library(tidyverse)

long_path <- "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/model_objects/"

# Facet 1: log(n_apps)
# RD & AFC at zip code level

log_napps <- read_rds(glue(long_path, "log_napps.rds"))
log_napps$level <- "zip"

# 2: proc_time
# RD & AFC at zip code and individual level

proc_time_zip <- read_rds(glue(long_path, "proc.rds"))
proc_time_zip$level <- "zip"
proc_time_ind <- read_rds(glue(long_path, "proc_time_individ.rds"))
proc_time_ind$level <- "ind"


# 3: log(total_paid)
# RD & AFC at zip code level

log_paid <-  read_rds(glue(long_path, "log_paid.rds"))
log_paid$level <- "zip"

# 4: approved
# RD & AFC at zip code and individual level

approved_ind <- read_rds(glue(long_path, "approval.rds"))
approved_ind$level <- "ind"
approved_zip <- read_rds(glue(long_path, "prop_approval.rds"))
approved_zip$level <- "zip"


plot_data <- 
  bind_rows(log_napps,
            proc_time_zip,
            proc_time_ind,
            log_paid,
            approved_zip, 
            approved_ind) %>% 
  filter(term %in% c("fsp", "Robust")) %>% 
  mutate(
    level = factor(level, levels = c("zip","ind")),
    model2 = ifelse(model == "AFC", "OLS", "RD"),
    model_order = paste0(model2,"_",level),
    model_order = factor(model_order, 
                         levels = c(
                           "OLS_zip",
                           "OLS_ind",
                           "RD_zip",
                           "RD_ind"
                           ), ordered = TRUE),
    outcome_label = case_when(outcome == "approval" ~ "\nApplication Approved",
                              outcome == "approval_prop" ~ "\nApplication Approved",
                              outcome == "log_app_post" ~ "\nLog(Total Applications + 1)",
                              outcome == "log_paid" ~ "\nLog(Total Paid + 1)",
                              outcome == "proc_time" ~ "\nDays Spent Processing", 
                              outcome == "proc_time_individual" ~ "\nDays Spent Processing"), 
    outcome_label = factor(
      x = outcome_label, 
      levels = c(
        "\nDays Spent Processing",
        "\nApplication Approved",
        "\nLog(Total Applications + 1)",
        "\nLog(Total Paid + 1)"
      )
    )
  ) 


# Alternative
# Circle = OLS
# Triangle = RD
# Dark blue = ZIP
# Yellow = individual
# Order: 
# afc_zip
# afc_ind
# rd_zip
# rd_ind

(tall_plot <-
ggplot(plot_data, aes(x = estimate, y = model_order, color = level)) + 
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(size = 2, aes(shape = model2)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0)) +
  facet_wrap(~ outcome_label,
             ncol = 2, scales = "free",
             strip.position = "top") +
  labs(x= "Estimate of FSP impact \n with 95% confidence intervals", y= "", title="") + 
  scale_y_discrete(limit = rev, expand = c(0.8, 0.8)) +
  scale_shape_manual(name = "Shape",values= c("OLS" = 16, "RD" = 17),
                     labels= c("OLS" = "Ordinary Least Squares (OLS)",
                               "RD"= "Regression Discontinuity (RD)")) +
  scale_color_manual(name = "Color",values= c("ind" = "#966f22", "zip" = "#247f9b"),
                     labels= c("zip"= "ZIP Level","ind" = "Individual Level")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              keywidth = 0.5,
                              keyheight = 0.5), 
         shape = guide_legend(nrow = 2, 
                              byrow = TRUE, 
                              keywidth = 0.5,
                              keyheight = 0.5)) +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # conserve space in PDF
        plot.margin = unit(c(-1,1,1,1), "cm"),
        # panel.border = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.spacing.y = unit(0, "cm"),
        # legend.title = element_blank(),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(filename = 
         "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/coef_plot.png", 
       plot = tall_plot, 
       width = 4, 
       height = 4)

ggsave(filename = 
         "G:/Shared drives/OES data  2301 ERA Grantee Flexibilities/2301_03_results/coef_plot.svg", 
       plot = tall_plot, 
       width = 4, 
       height = 4)


















