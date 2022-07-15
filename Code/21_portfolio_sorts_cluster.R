# Compute portfolio sorts
## This file is meant to be used on a cluster to run in parallel

# Setup -------------------------------------------------------------------

# Packages 
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(moments, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(sandwich, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(lmtest, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(lubridate, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# Data --------------------------------------------------------------------
load("Project_NSE/Data/data_sorts.RData")
load("Project_NSE/Data/data_grid.RData")

specifications_per_set <- 50
sets <- nrow(setup_grid) %/% specifications_per_set + 1

# Functions ---------------------------------------------------------------
# Data function #################################'
handle_data <- function(include_financials, include_utilities, 
                        drop_smallNYSE_at, drop_price_at, drop_stock_age_at, 
                        drop_earnings, drop_bookequity, 
                        sv_lag) {

  # Select dataset
  if(sv_lag == "FF") data_all <- data_FF
  if(sv_lag == "1m") data_all <- data_1m
  if(sv_lag == "3m") data_all <- data_3m
  if(sv_lag == "6m") data_all <- data_6m
  
  # Size filter based on NYSE percentile
  if(drop_smallNYSE_at > 0) {
    data_all <- data_all %>%
      group_by(month) %>%
      mutate(NYSE_breakpoint = quantile(mktcap_lag[exchange == "NYSE"], drop_smallNYSE_at)) %>%
      ungroup() %>%
      filter(mktcap_lag >= NYSE_breakpoint) %>%
      select(-NYSE_breakpoint)
  }
  
  # Exclude industries
  data_all <- data_all %>%
    filter(if(include_financials) TRUE else !grepl("Finance", industry)) %>%
    filter(if(include_utilities) TRUE else !grepl("Utilities", industry))
  
  # Price filter
  if(drop_price_at > 0) {
    data_all <- data_all %>%
      filter(filter_price >= drop_price_at)
  }
  
  # Stock age filter
  if(drop_stock_age_at > 0) {
    data_all <- data_all %>% 
      filter(filter_stock_age >= drop_stock_age_at)
  }
  
  # Earnings & book equity filter
  if(drop_earnings) {
    data_all <- data_all %>%
      filter(filter_earnings > 0)
  }
  
  if(drop_bookequity) {
    data_all <- data_all %>%
      filter(filter_be > 0)
  }
  
  # Define ME
  data_all <- data_all %>%
    mutate(me = mktcap_lag) %>%
    drop_na(me) %>%
    select(-starts_with("filter_"), -industry)
  
  # Return
  return(data_all)
}

# Portfolio assignment function #################################'
assign_portfolio <- function(data, sorting_variable, n_portfolios, exchanges) {
  # Escape small sets (i.e., less than 10 firms per portfolio)
  if(nrow(data) < n_portfolios * 10) return(NA)
  
  breakpoints <- data %>%
    filter(grepl(exchanges, exchange)) %>%
    summarize(breakpoint = quantile(x = cur_data() %>% pull({{ sorting_variable }}),
                                    probs = seq(0, 1, length.out = n_portfolios + 1),
                                    na.rm = TRUE)) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  data %>%
    mutate(portfolio = findInterval(cur_data() %>% pull({{ sorting_variable }}), breakpoints, all.inside = TRUE)) %>%
    pull(portfolio)
}

# Monthly portfolio return function #################################'
## Monthly single sort #-#-#-#-#-#-#-#-
compute_port_ret_monthly_sin <- function(data, sorting_variable, n_portfolios_main, exchanges, value_weighted) {
  data %>%
    group_by(month) %>%
    mutate(portfolio = assign_portfolio(sorting_variable = sorting_variable,
                                        n_portfolios = n_portfolios_main,
                                        exchanges = exchanges,
                                        data = cur_data())) %>%
    drop_na(portfolio) %>%
    group_by(month, portfolio) %>%
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              .groups = "drop_last") %>%
    summarize(premium = ret[portfolio == max(portfolio)] - ret[portfolio == min(portfolio)]) %>%
    ungroup()
}

## Monthly double sort independent #-#-#-#-#-#-#-#-
compute_port_ret_monthly_dbl_ind <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  data %>%
    group_by(month) %>%
    mutate(portfolio_secondary = assign_portfolio(sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges,
                                                  data = cur_data()),
           portfolio_main = assign_portfolio(sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges,
                                             data = cur_data()),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) %>%
    drop_na(portfolio_main, portfolio_secondary) %>%
    group_by(month, portfolio) %>%
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") %>%
    group_by(month, portfolio_main) %>%
    summarize(ret = mean(ret),
              .groups = "drop_last") %>%
    summarize(premium = ret[portfolio_main == max(portfolio_main)] - ret[portfolio_main == min(portfolio_main)]) %>%
    ungroup()
}

## Monthly double sort dependent #-#-#-#-#-#-#-#-
compute_port_ret_monthly_dbl_dep <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  data %>%
    group_by(month) %>%
    mutate(portfolio_secondary = assign_portfolio(sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges,
                                                  data = cur_data())) %>%
    drop_na(portfolio_secondary) %>%
    group_by(month, portfolio_secondary) %>%
    mutate(portfolio_main = assign_portfolio(sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges,
                                             data = cur_data()),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) %>%
    drop_na(portfolio_main) %>%
    group_by(month, portfolio) %>%
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") %>%
    group_by(month, portfolio_main) %>%
    summarize(ret = mean(ret),
              .groups = "drop_last") %>%
    summarize(premium = ret[portfolio_main == max(portfolio_main)] - ret[portfolio_main == min(portfolio_main)]) %>%
    ungroup()
}

# FF portfolio return function #################################'
## FF single sort #-#-#-#-#-#-#-#-
compute_port_ret_FF_sin <- function(data, sorting_variable, n_portfolios_main, exchanges, value_weighted) {
  # Assign portfolios
  data_sorting <- data %>%
    filter(month(month) == 7) %>%
    group_by(month) %>%
    mutate(portfolio = assign_portfolio(sorting_variable = sorting_variable,
                                        n_portfolios = n_portfolios_main,
                                        exchanges = exchanges,
                                        data = cur_data())) %>%
    ungroup() %>%
    select(permno, month, portfolio) %>%
    mutate(sorting_month = month) %>%
    drop_na(portfolio)
    
  # Compute portfolio returns
  data %>%
    left_join(data_sorting, by = c("permno", "month")) %>%
    group_by(permno) %>%
    arrange(month) %>%
    fill(portfolio, sorting_month) %>%
    filter(sorting_month >= month %m-% months(12)) %>%
    drop_na(portfolio) %>%
    group_by(month, portfolio) %>%
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              .groups = "drop_last") %>%
    summarize(premium = ret[portfolio == max(portfolio)] - ret[portfolio == min(portfolio)]) %>%
    ungroup()
}

## FF double sort independent #-#-#-#-#-#-#-#-
compute_port_ret_FF_dbl_ind <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  # Assign portfolios
  data_sorting <- data %>%
    filter(month(month) == 7) %>%
    group_by(month) %>%
    mutate(portfolio_secondary = assign_portfolio(sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges,
                                                  data = cur_data()),
           portfolio_main = assign_portfolio(sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges,
                                             data = cur_data()),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) %>%
    ungroup() %>%
    select(permno, month, portfolio_main, portfolio_secondary, portfolio) %>%
    mutate(sorting_month = month) %>%
    drop_na(portfolio_main, portfolio_secondary) 
  
  
  # Compute portfolio returns
  data %>%
    left_join(data_sorting, by = c("permno", "month")) %>%
    group_by(permno) %>%
    arrange(month) %>%
    fill(portfolio_main, portfolio_secondary, portfolio, sorting_month) %>%
    filter(sorting_month >= month %m-% months(12)) %>%
    drop_na(portfolio_main, portfolio_secondary) %>%
    group_by(month, portfolio) %>%
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") %>%
    group_by(month, portfolio_main) %>%
    summarize(ret = mean(ret),
              .groups = "drop_last") %>%
    summarize(premium = ret[portfolio_main == max(portfolio_main)] - ret[portfolio_main == min(portfolio_main)]) %>%
    ungroup()
}

## FF double sort dependent #-#-#-#-#-#-#-#-
compute_port_ret_FF_dbl_dep <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  # Assign portfolios
  data_sorting <- data %>%
    filter(month(month) == 7) %>%    
    group_by(month) %>%
    mutate(portfolio_secondary = assign_portfolio(sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges,
                                                  data = cur_data())) %>%
    drop_na(portfolio_secondary) %>%
    group_by(month, portfolio_secondary) %>%
    mutate(portfolio_main = assign_portfolio(sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges,
                                             data = cur_data()),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) %>%
    ungroup() %>%
    select(permno, month, portfolio_main, portfolio_secondary, portfolio) %>%
    mutate(sorting_month = month) %>%
    drop_na(portfolio_main) 
    
  # Compute portfolio returns
  data %>%
    left_join(data_sorting, by = c("permno", "month")) %>%
    group_by(permno) %>%
    arrange(month) %>%
    fill(portfolio_main, portfolio_secondary, portfolio, sorting_month) %>%
    filter(sorting_month >= month %m-% months(12)) %>%
    drop_na(portfolio_main, portfolio_secondary) %>%
    group_by(month, portfolio) %>%
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") %>%
    group_by(month, portfolio_main) %>%
    summarize(ret = mean(ret),
              .groups = "drop_last") %>%
    summarize(premium = ret[portfolio_main == max(portfolio_main)] - ret[portfolio_main == min(portfolio_main)]) %>%
    ungroup()
}

# Return evaluation function #################################
evaluate_performance <- function(data) {
  # Add FF
  data <- data %>%
    left_join(factors_ff_monthly, by = "month")
  
  # CAPM and FF3 TS regression tests
  ret <- coeftest(lm(premium ~ 1, data = data), vcov = NeweyWest)
  capm <- coeftest(lm(premium ~ 1 + mkt_excess, data = data), vcov = NeweyWest)
  ff3 <- coeftest(lm(premium ~ 1 + mkt_excess + smb + hml, data = data), vcov = NeweyWest)

  data %>%
    summarize(mean = mean(premium),
              sd = sd(premium),
              se = as.numeric(ret[1, 2]),
              t = as.numeric(ret[1, 3]),
              skew = skewness(premium),
              kurtosis = kurtosis(premium),
              alpha_CAPM = as.numeric(capm[1, 1]),
              se_CAPM = as.numeric(capm[1, 2]),
              t_CAPM = as.numeric(capm[1,3]),
              beta_M_CAPM = as.numeric(capm[2,1]),
              alpha_FF3 = as.numeric(ff3[1,1]),
              se_FF3 = as.numeric(ff3[1,2]),
              t_FF3 = as.numeric(ff3[1,3]),
              beta_M_FF3 = as.numeric(ff3[2,1]),
              beta_S_FF3 = as.numeric(ff3[3,1]),
              beta_V_FF3 = as.numeric(ff3[4,1]))
}

# Overall function #################################
execute_portfolio_functions <- function(sorting_variable,
                                        ID = as.numeric(NA),
                                        n_portfolios_main = 10,
                                        n_portfolios_secondary = 10,
                                        exchanges = "NYSE|NASDAQ|AMEX",
                                        value_weighted = TRUE,
                                        sorting_method = "single",
                                        formation_time = "monthly",
                                        include_financials = TRUE,
                                        include_utilities = TRUE,
                                        drop_smallNYSE_at = 0,
                                        drop_price_at = 0,
                                        drop_stock_age_at = 0,
                                        drop_earnings = FALSE,
                                        drop_bookequity = FALSE,
                                        sv_lag = "6m") {
  # Small message
  cat(paste("-", ID, "\n"))
  
  # Combine data
  data_sorts <- handle_data(include_financials = include_financials, 
                            include_utilities = include_utilities, 
                            drop_smallNYSE_at = drop_smallNYSE_at,  
                            drop_price_at = drop_price_at,
                            drop_stock_age_at = drop_stock_age_at,
                            drop_earnings = drop_earnings, 
                            drop_bookequity = drop_bookequity,
                            sv_lag = sv_lag) %>%
    drop_na({{ sorting_variable }})
  
  # Compute sorts
  ## Single sorts
  if(sorting_method == "single") {
    ### Monthly formation
    if(formation_time == "monthly") {
      data_return <- compute_port_ret_monthly_sin(data = data_sorts, 
                                                  sorting_variable = sorting_variable, 
                                                  n_portfolios_main = n_portfolios_main, 
                                                  exchanges = exchanges, 
                                                  value_weighted = value_weighted)
    }
    
    ### FF formation
    if(formation_time == "FF") {
      data_return <- compute_port_ret_FF_sin(data = data_sorts, 
                                             sorting_variable = sorting_variable, 
                                             n_portfolios_main = n_portfolios_main, 
                                             exchanges = exchanges, 
                                             value_weighted = value_weighted)
    }
  }
  
  ## Double, independent sorts 
  if(sorting_method == "dbl_ind") {
    ### Monthly formation
    if(formation_time == "monthly") {
      data_return <- compute_port_ret_monthly_dbl_ind(data = data_sorts, 
                                                      sorting_variable = sorting_variable, 
                                                      n_portfolios_main = n_portfolios_main, 
                                                      n_portfolios_secondary = n_portfolios_secondary,
                                                      exchanges = exchanges, 
                                                      value_weighted = value_weighted)
    }
    
    ### FF formation
    if(formation_time == "FF") {
      data_return <- compute_port_ret_FF_dbl_ind(data = data_sorts, 
                                                 sorting_variable = sorting_variable, 
                                                 n_portfolios_main = n_portfolios_main, 
                                                 n_portfolios_secondary = n_portfolios_secondary,
                                                 exchanges = exchanges, 
                                                 value_weighted = value_weighted)
    } 
  }
  
  ## Double, independent sorts 
  if(sorting_method == "dbl_dep") {
    ### Monthly formation
    if(formation_time == "monthly") {
      data_return <- compute_port_ret_monthly_dbl_dep(data = data_sorts, 
                                                      sorting_variable = sorting_variable, 
                                                      n_portfolios_main = n_portfolios_main, 
                                                      n_portfolios_secondary = n_portfolios_secondary,
                                                      exchanges = exchanges, 
                                                      value_weighted = value_weighted)
    }
    
    ### FF formation
    if(formation_time == "FF") {
      data_return <- compute_port_ret_FF_dbl_dep(data = data_sorts, 
                                                 sorting_variable = sorting_variable, 
                                                 n_portfolios_main = n_portfolios_main, 
                                                 n_portfolios_secondary = n_portfolios_secondary,
                                                 exchanges = exchanges, 
                                                 value_weighted = value_weighted)
    }
  }
  
  # Save returns
  saveRDS(data_return, file = paste0("Project_NSE/Timeseries/set_", ID, ".rds"))
  
  
  # Performance evaluation
  evaluate_performance(data_return)
}

# Mapping -----------------------------------------------------------------
# Get Cluster Job ID
cluster_id = as.integer(Sys.getenv("SGE_TASK_ID"))
cat(paste("Starting Esimation", cluster_id, " - ", Sys.time()), "\n")

# Check if file ran w/o problems before
files_ran <- list.files("Project_NSE/Results")
files_ran <- substr(files_ran, 5, nchar(files_ran)-4)

if(as.character(cluster_id) %in% files_ran & cluster_id != sets) {
  # File already ran w/o problems
  cat(paste("ID", cluster_id, "not needed"))
} else {
  # Check wether dates are non empty
  if(cluster_id <= sets) {
    # Loop Setup
    ids <- seq(specifications_per_set*(cluster_id - 1)+1, min(specifications_per_set*cluster_id, nrow(setup_grid)))
    
    # Compute
    data_results_pmap <- setup_grid %>%
      slice(ids) %>%
      mutate(performance = pmap(
        .l = list(
          sorting_variable,
          ID,
          n_portfolios_main,
          n_portfolios_secondary,
          exchanges,
          value_weighted,
          sorting_method,
          formation_time,
          include_financials,
          include_utilities,
          drop_smallNYSE_at,
          drop_price_at,
          drop_stock_age_at,
          drop_earnings,
          drop_bookequity,
          sv_lag
        ),
        .f = ~ execute_portfolio_functions(
          sorting_variable = ..1,
          ID = ..2,
          n_portfolios_main = ..3,
          n_portfolios_secondary = ..4,
          exchanges = ..5,
          value_weighted = ..6,
          sorting_method = ..7,
          formation_time = ..8,
          include_financials = ..9,
          include_utilities = ..10,
          drop_smallNYSE_at = ..11,
          drop_price_at = ..12,
          drop_stock_age_at = ..13,
          drop_earnings = ..14,
          drop_bookequity = ..15,
          sv_lag = ..16
        )
      ))
    
    # Unnest
    data_results <- data_results_pmap %>%
      unnest(performance)
    
    # Save
    saveRDS(data_results, file = paste0("Project_NSE/Results/set_", cluster_id, ".rds"))
    cat(paste("\n\nEstimation Done", cluster_id, " - ", Sys.time()), "\n")
    
    # Auto-run grouping
    if(cluster_id == sets) {
      done <- FALSE
      
      # Waiting (maximum 2h)
      for(wating in 1:120) {
        if(abs(length(list.files(paste0("Project_NSE/Results"))) - sets) < 1 & !done) {
          cat(paste("\n\n ---- \n", "Conducting Aggegation -", as.character(Sys.time()), "\n\n ---- \n\n"))
          Sys.sleep(30)
          source("Project_NSE/22_portfolio_sorts_cluster_aggregation.R")
          done <- TRUE
        } else {
          if(!done) { 
            Sys.sleep(60) 
            cat(".")
          } else next
        }
      }
    }
  } else {
    # Alternative output
    cat(paste(cluster_id, "- Not needed \n"))
  }
}