
library(tigris)
library(sf)

pull_write_onestate<- function(one_fips, year){
  
  ## pull and write counties if not already pulled
  c_path <- paste0(BASE_DIR, sprintf("2305_raw_data/ACS/counties_FIP%s.RDS", one_fips))
  if(!file.exists(c_path)){
    try(saveRDS(tigris::counties(state = one_fips, year = year),
            c_path))
  }
  
  ## pull and write ZCTAs if not already pulled - use 2010 since not available for 2020
  z_path <- paste0(BASE_DIR, sprintf("2305_raw_data/ACS/zctas_FIP%s.RDS", one_fips))
  if(!file.exists(z_path)){
    try(saveRDS(tigris::zctas(state = one_fips, year = 2010),
            z_path))
  }
  
  ## pull and write places 
  p_path <- paste0(BASE_DIR, sprintf("2305_raw_data/ACS/places_FIP%s.RDS", one_fips))
  if(!file.exists(p_path)){
    try(saveRDS(tigris::places(state = one_fips, year = year),
            p_path))
  }
  
  ## pull and write blocks 
  b_path <- paste0(BASE_DIR, sprintf("2305_raw_data/ACS/blocks_FIP%s.RDS", one_fips))
  if(!file.exists(b_path)){
    try(saveRDS(tigris::blocks(state = one_fips, year = year),
            b_path))
  }
  
}


intersect_zcta_blocks_onestate <- function(one_fips){
  
  # read in blocks
  blocks <- readRDS(paste0(BASE_DIR, 
                           sprintf("2305_raw_data/ACS/blocks_FIP%s.RDS", one_fips))) %>%
    mutate(county = paste0(STATEFP20, COUNTYFP20),
           row.id = 1:n())
  
  # read in zctas
  zctas <- readRDS(paste0(BASE_DIR, 
                          sprintf("2305_raw_data/ACS/zctas_FIP%s.RDS", one_fips))) %>%
    mutate(row.id = 1:n())
  
  # intersect and join on earlier information 
  zcta_block_intersect <- st_intersects(blocks, zctas, sparse = TRUE) # which zip(s) each block intersects with
  zcta_block_intersect_df <- as.data.frame(zcta_block_intersect) %>% # add back geo ids
    left_join(blocks, by = "row.id") %>%
    left_join(zctas %>% select(ZCTA5CE10, row.id), by = c("col.id" = "row.id"),
              suffix = c("_block", "_zip")) %>%
    arrange(ZCTA5CE10)
  
  # group by zcta and county and find the total pop
  zcta_block_sum <- zcta_block_intersect_df %>%
    group_by(ZCTA5CE10, county) %>%
    summarise(total_pop = sum(POP20))
  
  # code each zcta to its highest-population county
  zcta_cwalk_popblock <- zcta_block_sum %>%  
    group_by(ZCTA5CE10) %>%
    filter(total_pop == max(total_pop)) %>%
    ungroup() %>%
    select(zcta5 = ZCTA5CE10, county_pop_sum = county, total_block_pop = total_pop) 
  
  log_print(paste0("finished intersection for state fips: ",
                   one_fips),
            hide_notes = TRUE)
  return(zcta_cwalk_popblock)
}

# function for evaluating predictive range
find_percpred_inband <- function(predictions, bandwidth){
  perc <- predictions %>% 
    summarize(mean(abs(.pred - rent_inc < bandwidth)))
  return(perc[[1]])
}


# function for IPTW weights 
get_ipw <- function(prob, treat) {1/(prob * treat + (1 - prob) * (1 - treat))}

# function for tidying robust rdd outputs
tidy.rdrobust <- 
  function(fit, conf.int = TRUE){
    ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci, c = fit$c)
    row.names(ret) <- NULL
    names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "cutoff")
    ret
  }
