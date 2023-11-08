# Compute portfolio sorts
## This file is meant to be used on a cluster to run in parallel


# Setup ------------------------------------------------------------
# How many specifications to run per task ID & how many sets
specifications_per_set <- 100
sets <- 38708 # To compute: nrow(setup_grid) %/% specifications_per_set + 1

# Get Cluster Job ID
cluster_id = as.integer(Sys.getenv("SGE_TASK_ID"))

# Existing files
files_ran <- list.files("Project_NSE/Results")
files_ran <- substr(files_ran, 5, nchar(files_ran)-4)

# Check if file ran w/o problems before
if(as.character(cluster_id) %in% files_ran & cluster_id != sets) {
  # File already ran w/o problems
  cat(paste("ID", cluster_id, "not needed"))
  stop("Already completed")
}
  
# Packages 
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(moments, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(sandwich, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(lmtest, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(monotonicity, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

set.seed("20211021")

# Functions --------------------------------------------------------
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
    data_all <- data_all |> 
      group_by(month) |> 
      mutate(NYSE_breakpoint = quantile(mktcap_lag[exchange == "NYSE"], drop_smallNYSE_at)) |> 
      ungroup() |> 
      filter(mktcap_lag >= NYSE_breakpoint) |> 
      select(-NYSE_breakpoint)
  }
  
  # Exclude industries
  data_all <- data_all |> 
    filter(if(include_financials) TRUE else !grepl("Finance", industry)) |> 
    filter(if(include_utilities) TRUE else !grepl("Utilities", industry))
  
  # Price filter
  if(drop_price_at > 0) {
    data_all <- data_all |> 
      filter(filter_price >= drop_price_at)
  }
  
  # Stock age filter
  if(drop_stock_age_at > 0) {
    data_all <- data_all |> 
      filter(filter_stock_age >= drop_stock_age_at)
  }
  
  # Earnings & book equity filter
  if(drop_earnings) {
    data_all <- data_all |> 
      filter(filter_earnings > 0)
  }
  
  if(drop_bookequity) {
    data_all <- data_all |> 
      filter(filter_be > 0)
  }
  
  # Define ME
  data_all <- data_all |> 
    mutate(me = mktcap_lag) |> 
    drop_na(me) |> 
    select(-starts_with("filter_"), -industry)
  
  # Return
  return(data_all)
}

# Portfolio assignment function #################################'
assign_portfolio <- function(data, sorting_variable, n_portfolios, exchanges) {
  # Escape small sets (i.e., less than 10 firms per portfolio)
  if(nrow(data) < n_portfolios * 10) return(NA)
  
  # Compute breakpoints
  breakpoints <- data |> 
    filter(grepl(exchanges, exchange)) |> 
    pull(all_of(sorting_variable)) |> 
    quantile(probs = seq(0, 1, length.out = n_portfolios + 1),
             na.rm = TRUE,
             names = FALSE)
  
  
  # Check if sorting variable's breakpoints fill n_portfolios
  ## CASE 1: Multiple at lower end
  if(breakpoints[1] == breakpoints[2]) {
    ## Compute new breakpoints
    breakpoints_new <- data |> 
      filter(grepl(exchanges, exchange)) |> 
      filter(!!sym(sorting_variable) > breakpoints[1]) |> 
      pull(all_of(sorting_variable)) |> 
      quantile(probs = seq(0, 1, length.out = n_portfolios),
               na.rm = TRUE,
               names = FALSE)
    
    ## Combine breakpoints
    breakpoints <- c(breakpoints[1], breakpoints_new)
  }
  
  ## CASE 2: Multiple at upper end
  if(breakpoints[n_portfolios] == breakpoints[n_portfolios + 1]) {
    ## Compute new breakpoints
    breakpoints_new <- data |> 
      filter(grepl(exchanges, exchange)) |> 
      filter(!!sym(sorting_variable) < breakpoints[n_portfolios]) |> 
      pull(all_of(sorting_variable)) |> 
      quantile(probs = seq(0, 1, length.out = n_portfolios),
               na.rm = TRUE,
               names = FALSE)
    
    ## Combine breakpoints
    breakpoints <- c(breakpoints_new, breakpoints[n_portfolios])
  }
  
  # Assign portfolios
  portfolios <- data |> 
    mutate(portfolio = findInterval(pick(everything()) |> 
                                      pull(all_of(sorting_variable)), 
                                    breakpoints, 
                                    all.inside = TRUE)) |> 
    pull(portfolio)
  
  # Check if breakpoints are well defined
  if(length(unique(breakpoints)) == n_portfolios + 1) {
    return(portfolios)
  } else {
    cat(paste0("Breakpoint issue! SV", sorting_variable, " - ", as.Date(as.numeric(cur_group())), "\n"))
    print(breakpoints)
    cat("\n\n")
    return(portfolios)
  }
}

# Monthly portfolio return function #################################'
## Monthly single sort #-#-#-#-#-#-#-#-
compute_port_ret_monthly_sin <- function(data, sorting_variable, n_portfolios_main, exchanges, value_weighted) {
  data |> 
    group_by(month) |> 
    mutate(portfolio = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                        sorting_variable = sorting_variable,
                                        n_portfolios = n_portfolios_main,
                                        exchanges = exchanges)) |> 
    drop_na(portfolio) |> 
    group_by(month, portfolio) |> 
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              .groups = "drop")
}

## Monthly double sort independent #-#-#-#-#-#-#-#-
compute_port_ret_monthly_dbl_ind <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  data |>
    group_by(month) |>
    mutate(portfolio_secondary = assign_portfolio(data = pick(me, exchange),
                                                  sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges),
           portfolio_main = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                             sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    drop_na(portfolio_main, portfolio_secondary) |>
    group_by(month, portfolio) |>
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(month, portfolio_main) |>
    summarize(ret = mean(ret),
              .groups = "drop") |> 
    rename(portfolio = portfolio_main)
}

## Monthly double sort dependent #-#-#-#-#-#-#-#-
compute_port_ret_monthly_dbl_dep <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  data |>
    group_by(month) |>
    mutate(portfolio_secondary = assign_portfolio(data = pick(me, exchange),
                                                  sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges)) |>
    drop_na(portfolio_secondary) |>
    group_by(month, portfolio_secondary) |>
    mutate(portfolio_main = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                             sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    drop_na(portfolio_main) |>
    group_by(month, portfolio) |>
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(month, portfolio_main) |>
    summarize(ret = mean(ret),
              .groups = "drop") |> 
    rename(portfolio = portfolio_main)
}

# FF portfolio return function #################################'
## FF single sort #-#-#-#-#-#-#-#-
compute_port_ret_FF_sin <- function(data, sorting_variable, n_portfolios_main, exchanges, value_weighted) {
  # Assign portfolios
  data_sorting <- data |>
    filter(month(month) == 7) |>
    group_by(month) |>
    mutate(portfolio = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                        sorting_variable = sorting_variable,
                                        n_portfolios = n_portfolios_main,
                                        exchanges = exchanges)) |>
    ungroup() |>
    select(permno, month, portfolio) |>
    mutate(sorting_month = month) |>
    drop_na(portfolio)
    
  # Compute portfolio returns
  data |>
    left_join(data_sorting, by = c("permno", "month")) |>
    group_by(permno) |>
    arrange(month) |>
    fill(portfolio, sorting_month) |>
    filter(sorting_month >= month %m-% months(12)) |>
    drop_na(portfolio) |>
    group_by(month, portfolio) |>
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              .groups = "drop")
}

## FF double sort independent #-#-#-#-#-#-#-#-
compute_port_ret_FF_dbl_ind <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  # Assign portfolios
  data_sorting <- data |>
    filter(month(month) == 7) |>
    group_by(month) |>
    mutate(portfolio_secondary = assign_portfolio(data = pick(me, exchange),
                                                  sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges),
           portfolio_main = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                             sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    ungroup() |>
    select(permno, month, portfolio_main, portfolio_secondary, portfolio) |>
    mutate(sorting_month = month) |>
    drop_na(portfolio_main, portfolio_secondary) 
  
  
  # Compute portfolio returns
  data |>
    left_join(data_sorting, by = c("permno", "month")) |>
    group_by(permno) |>
    arrange(month) |>
    fill(portfolio_main, portfolio_secondary, portfolio, sorting_month) |>
    filter(sorting_month >= month %m-% months(12)) |>
    drop_na(portfolio_main, portfolio_secondary) |>
    group_by(month, portfolio) |>
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(month, portfolio_main) |>
    summarize(ret = mean(ret),
              .groups = "drop") |> 
    rename(portfolio = portfolio_main)
}

## FF double sort dependent #-#-#-#-#-#-#-#-
compute_port_ret_FF_dbl_dep <- function(data, sorting_variable, n_portfolios_main, n_portfolios_secondary, exchanges, value_weighted) {
  # Assign portfolios
  data_sorting <- data |>
    filter(month(month) == 7) |>    
    group_by(month) |>
    mutate(portfolio_secondary = assign_portfolio(data = pick(me, exchange),
                                                  sorting_variable = "me",
                                                  n_portfolios = n_portfolios_secondary,
                                                  exchanges = exchanges)) |>
    drop_na(portfolio_secondary) |>
    group_by(month, portfolio_secondary) |>
    mutate(portfolio_main = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                             sorting_variable = sorting_variable,
                                             n_portfolios = n_portfolios_main,
                                             exchanges = exchanges),
           portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    ungroup() |>
    select(permno, month, portfolio_main, portfolio_secondary, portfolio) |>
    mutate(sorting_month = month) |>
    drop_na(portfolio_main) 
    
  # Compute portfolio returns
  data |>
    left_join(data_sorting, by = c("permno", "month")) |>
    group_by(permno) |>
    arrange(month) |>
    fill(portfolio_main, portfolio_secondary, portfolio, sorting_month) |>
    filter(sorting_month >= month %m-% months(12)) |>
    drop_na(portfolio_main, portfolio_secondary) |>
    group_by(month, portfolio) |>
    summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(month, portfolio_main) |>
    summarize(ret = mean(ret),
              .groups = "drop") |> 
    rename(portfolio = portfolio_main)
}

# Return evaluation function #################################
evaluate_performance <- function(data, direction, ID) {
  # Seed for bootstrap consistency
  set.seed(2023)
  
  # Monotonicity test
  ## Arrange based on direction
  if(direction == 1) {
    monotonicity <- data |>
      arrange(month, portfolio)
  } else {
    monotonicity <- data |>
      arrange(month, desc(portfolio))
  }
  
  ## Perform test
  monotonicity <- monotonicity |> 
    pivot_wider(names_from = "portfolio", values_from = "ret") |> 
    drop_na() |> 
    select(-month) |> 
    as.matrix() |> 
    monoRelation(block_length = 6)
  
  # Force gc()
  gc()
  
  # Compute premium
  data_premium <- data |> 
    group_by(month) |> 
    summarize(premium = ret[portfolio == max(portfolio)] - ret[portfolio == min(portfolio)],
              log_premium = log(1 + ret[portfolio == max(portfolio)]) - log(1 + ret[portfolio == min(portfolio)]),
              .groups = "drop") |> 
    inner_join(factors_monthly, by = "month") |> 
    mutate(premium = premium * direction,
           log_premium = log_premium * direction)
  
  # Save premium
  saveRDS(data_premium |> select(month, premium),
          file = paste0("Project_NSE/Timeseries/set_", ID, ".rds"))
  
  # Premium, CAPM, and FF3 tests
  ## TS regression tests
  ret <- coeftest(lm(premium ~ 1, data = data_premium), vcov = NeweyWest)
  log_ret <- coeftest(lm(log_premium ~ 1, data = data_premium), vcov = NeweyWest)
  capm <- coeftest(lm(premium ~ 1 + mkt_excess, data = data_premium), vcov = NeweyWest)
  ff5 <- coeftest(lm(premium ~ 1 + mkt_excess + smb + hml + rmw + cma, data = data_premium), vcov = NeweyWest)
  q5 <- coeftest(lm(premium ~ 1 + q_mkt + q_me + q_ia + q_roe + q_eg, data = data_premium), vcov = NeweyWest)
  
  # Force gc()
  gc()
  
  # Output
  data_premium |> 
    summarize(mean = mean(premium),
              sd = sd(premium),
              se = as.numeric(ret[1, 2]),
              t = as.numeric(ret[1, 3]),
              p = as.numeric(ret[1, 4]),
              skew = skewness(premium),
              kurtosis = kurtosis(premium),
              alpha_CAPM = as.numeric(capm[1, 1]),
              se_CAPM = as.numeric(capm[1, 2]),
              t_CAPM = as.numeric(capm[1,3]),
              beta_M_CAPM = as.numeric(capm[2,1]),
              alpha_FF5 = as.numeric(ff5[1,1]),
              se_FF5 = as.numeric(ff5[1,2]),
              t_FF5 = as.numeric(ff5[1,3]),
              alpha_q5 = as.numeric(q5[1,1]),
              se_q5 = as.numeric(q5[1,2]),
              t_q5 = as.numeric(q5[1,3]),
              mono_adj = as.numeric(monotonicity[3,2]),
              mono_all = as.numeric(monotonicity[4,2]),
              log_mean = mean(log_premium),
              log_sd = sd(log_premium),
              log_se = as.numeric(log_ret[1, 2]),
              log_t = as.numeric(log_ret[1, 3]),
              log_p = as.numeric(log_ret[1, 4]),
              log_skew = skewness(log_premium),
              log_kurtosis = kurtosis(log_premium))
}

# Overall function #################################
execute_portfolio_functions <- function(sorting_variable,
                                        ID = NA_real_,
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
                            sv_lag = sv_lag) |>
    drop_na(all_of(sorting_variable))
  
  # Force gc()
  gc()
  
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
  
  # Force gc()
  gc()
  
  # SV direction
  direction_sv <- direction_hml_portfolio |> 
    filter(sv == sorting_variable) |> 
    pull(direction)
  
  # Performance evaluation
  evaluate_performance(data = data_return,
                       direction = direction_sv,
                       ID = ID)
}


# Mapping ----------------------------------------------------------
# Check whether sets are used
if(cluster_id <= sets) {
  # Short feedback
  cat(paste("Starting Esimation", cluster_id, " - ", Sys.time()), "\n")
  
  # Data -----------------------------------------------------------
  load("Project_NSE/Data/data_sorts.RData")
  load("Project_NSE/Data/data_grid.RData")

  # Loop Setup
  ids <- seq(specifications_per_set*(cluster_id - 1)+1, min(specifications_per_set*cluster_id, nrow(setup_grid)))
  
  # Restrict input grid
  setup_grid <- setup_grid |>
    slice(ids)
  
  # Restrict sorting data
  sv_lags_used <- unique(setup_grid$sv_lag)
  
  ## 1m
  if(any(sv_lags_used == "1m")) {
    data_1m <- data_1m |> 
      select(permno, month, exchange, industry, ret_excess, mktcap_lag, 
             starts_with("filter_"), 
             all_of(unique(setup_grid |> filter(sv_lag == "1m") |> pull(sorting_variable))))
  } else rm(data_1m)
  
  ## 3m
  if(any(sv_lags_used == "3m")) {
    data_3m <- data_3m |> 
      select(permno, month, exchange, industry, ret_excess, mktcap_lag,  
             starts_with("filter_"), 
             all_of(unique(setup_grid |> filter(sv_lag == "3m") |> pull(sorting_variable))))
  } else rm(data_3m)
  
  ## 6m
  if(any(sv_lags_used == "6m")) {
    data_6m <- data_6m |> 
      select(permno, month, exchange, industry, ret_excess, mktcap_lag, 
             starts_with("filter_"), 
             all_of(unique(setup_grid |> filter(sv_lag == "6m") |> pull(sorting_variable))))
  } else rm(data_6m)
  
  ## FF
  if(any(sv_lags_used == "FF")) {
    data_FF <- data_FF |> 
      select(permno, month, exchange, industry, ret_excess, mktcap_lag, 
             starts_with("filter_"), 
             all_of(unique(setup_grid |> filter(sv_lag == "FF") |> pull(sorting_variable))))
  } else rm(data_FF)
  
  # Force gc()
  gc()
  
  # Compute
  data_results_pmap <- setup_grid |>
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
  data_results <- data_results_pmap |>
    unnest(performance)
  
  # Save
  saveRDS(data_results, file = paste0("Project_NSE/Results/set_", cluster_id, ".rds"))
  cat(paste("\n\nEstimation Done", cluster_id, " - ", Sys.time()), "\n")
  
  # Auto-run grouping
  if(cluster_id == sets) {
    done <- FALSE
    
    # Waiting (maximum 2h)
    for(wating in 1:120) {
      if(abs(length(list.files("Project_NSE/Results")) - sets) < 1 & !done) {
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