# This function provides a tidy method for rdrobust
tidy.rdrobust <- 
  function(fit, conf.int = TRUE){
    ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci, c = fit$c)
    row.names(ret) <- NULL
    names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "cutoff")
    ret
  }

# tidy for zeroinfl
tidy.zeroinfl <- 
  function(fit){
    
    sfit <- summary(fit)
    
    ret <- data.frame(rownames(sfit$coefficients$count),
                      sfit$coefficients$count[,1],
                      sfit$coefficients$count[,2],
                      sfit$coefficients$count[,3],
                      sfit$coefficients$count[,4])
    row.names(ret) <- NULL
    colnames(ret) <- NULL
    names(ret) <- c("term", "estimate", 
                    "std.error", "statistic", "p.value")
    ret
  }

# This function runs the main analyses using an outcome provided as a string
get_rdd_afc_ests <- function(outcome, df){
  
  df <- data.frame(df)
  
  df$outcome_pre <- df[,paste0(outcome, "_pre")]
  df$outcome_post <- df[,paste0(outcome, "_post")]
  
  # rdrobust
  rdd <- rdrobust(y = df$outcome_post, x = df$running_variable, 
                  c = 0, covs = df$outcome_pre) %>% 
    tidy() %>% 
    subset(term == "Robust") %>% 
    mutate(outcome = outcome, model = "RDD")
  
  # lm_robust
  afc <- lm_robust(
    formula = outcome_post ~ outcome_pre + med_inc + fsp,
    data = df) %>% tidy() %>% 
    select(-outcome) %>% 
    subset(term == "fsp") %>% 
    mutate(outcome = outcome, model = "AFC")
  
  bind_rows(rdd, afc)
  
}

# Processing time analysis function, outcome as string
get_proc_time_ests <- function(outcome, df){
  
  df <- data.frame(df)
  
  df$outcome_col <- df[,paste0(outcome)]
  
  # rdrobust
  rdd <- rdrobust(y = df$outcome_col, x = df$running_variable, 
                  c = 0) %>% 
    tidy() %>% 
    subset(term == "Robust") %>% 
    mutate(outcome = outcome, model = "RDD")
  
  # lm_robust
  afc <- lm_robust(
    formula = outcome_col ~ med_inc + fsp,
    data = df) %>% tidy() %>% 
    select(-outcome) %>% 
    subset(term == "fsp") %>% 
    mutate(outcome = outcome, model = "AFC")
  
  bind_rows(rdd, afc)
  
}

