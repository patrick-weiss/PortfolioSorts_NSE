# This file computes 69,120 portfolio sorts for a single sorting variable
# This is code is from a blog post on tidy-finance.org
# Adapt and use it to your needs, but cite Walter, Weber, and Weiss (2023)


# Packages --------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(RPostgres)
library(frenchdata)
library(furrr)

options(future.globals.maxSize= 891289600)


# Data: Database --------------------------------------------------------
# SQLite database
data_tidy_nse <- dbConnect(SQLite(), 
                           "data_nse.sqlite", 
                           extended_types = TRUE)

# Dates
start_date <- as.Date("1968-01-01")
end_date <- as.Date("2021-12-31")

# WRDS connection
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("user"), # put your information here
  password = Sys.getenv("password") # put your information here
)


# Data: Fama-French -----------------------------------------------------
# Load
factors_ff_monthly <- download_french_data("Fama/French 5 Factors (2x3)")$subsets$data[[1]] 

# Manipulate
factors_ff_monthly <- factors_ff_monthly |>
  transmute(
    month = floor_date(ymd(paste0(date, "01")), "month"),
    rf = as.numeric(RF) / 100
  ) |>
  filter(month >= start_date & month <= end_date)

# Store
factors_ff_monthly |>
  dbWriteTable(conn = data_tidy_nse, 
               name = "factors_ff_monthly", 
               value = _,
               overwrite = TRUE)


# Data: CRSP ------------------------------------------------------------
# Load
## Returns
msf_db <- tbl(wrds, in_schema("crsp", "msf"))

## Names
msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))

## Delisting
msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))

# CRSP
crsp_monthly <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  inner_join(msenames_db |>
               filter(shrcd %in% c(10, 11)) |>
               select(permno, ncusip, exchcd, siccd, namedt, nameendt), by = c("permno")) |>
  filter(date >= namedt & date <= nameendt) |>
  mutate(month = floor_date(date, "month")) |>
  left_join(msedelist_db |>
              select(permno, dlstdt, dlret, dlstcd) |>
              mutate(month = floor_date(dlstdt, "month")), by = c("permno", "month")) |>
  select(permno, cusip, ncusip, month, ret, retx, shrout, altprc, exchcd, siccd, dlret, dlstcd) |>
  mutate(month = as.Date(month)) |>
  collect() 

# Manipulate
# CRSP mktcap & lag
crsp_monthly <- crsp_monthly |>
  mutate(mktcap = shrout * 10^3 * abs(altprc) / 10^6) |>
  mutate(mktcap = if_else(mktcap == 0, NA_real_, mktcap))

mktcap_lag <- crsp_monthly |>
  mutate(month = month %m+% months(1)) |>
  select(permno, month, mktcap_lag = mktcap)

crsp_monthly <- crsp_monthly |>
  left_join(mktcap_lag, by = c("permno", "month"))

rm(mktcap_lag)

# Delisting returns
crsp_monthly <- crsp_monthly |>
  mutate(ret_adj = case_when(
    is.na(dlstcd) ~ ret,
    !is.na(dlstcd) & !is.na(dlret) ~ dlret,
    dlstcd %in% c(500, 520, 580, 584) |
      (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
    dlstcd == 100 ~ ret,
    TRUE ~ -1
  )) |>
  select(-c(dlret, dlstcd))

# Excess returns
crsp_monthly <- crsp_monthly |>
  left_join(factors_ff_monthly |> select(month, rf), by = "month") |>
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1)
  ) |>
  select(-ret_adj, -rf)

# Exchange codes
crsp_monthly <- crsp_monthly |>
  mutate(exchange = case_when(
    exchcd %in% c(1, 31) ~ "NYSE",
    exchcd %in% c(2, 32) ~ "AMEX",
    exchcd %in% c(3, 33) ~ "NASDAQ",
    TRUE ~ "Other"
  ))

# Industry codes
crsp_monthly <- crsp_monthly |>
  mutate(industry = case_when(
    siccd >= 1 & siccd <= 999 ~ "Agriculture",
    siccd >= 1000 & siccd <= 1499 ~ "Mining",
    siccd >= 1500 & siccd <= 1799 ~ "Construction",
    siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
    siccd >= 4000 & siccd <= 4899 ~ "Transportation",
    siccd >= 4900 & siccd <= 4999 ~ "Utilities",
    siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
    siccd >= 5200 & siccd <= 5999 ~ "Retail",
    siccd >= 6000 & siccd <= 6999 ~ "Finance",
    siccd >= 7000 & siccd <= 8999 ~ "Services",
    siccd >= 9000 & siccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))

# Drop NAs
crsp_monthly <- crsp_monthly |>
  drop_na(ret_excess, mktcap, mktcap_lag)

# Save 
crsp_monthly |>
  dbWriteTable(conn = data_tidy_nse, 
               name = "crsp_monthly", 
               value = _,
               overwrite = TRUE)


# Data: Compustat -------------------------------------------------------
# Load
compustat <- tbl(wrds, in_schema("comp", "funda")) |>
  filter(
    indfmt == "INDL" &
      datafmt == "STD" &
      consol == "C" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(
    gvkey, # Firm identifier
    datadate, # Date of the accounting data
    at, # Total assets
    ceq, # Total common/ordinary equity
    ib, # income before extraordinary items 
    itcb, # Investment tax credit
    lt, # Total liabilities
    pstkrv, # Preferred stock redemption value
    pstkl, # Preferred stock liquidating value
    pstk, # Preferred stock par value
    prcc_f, # price close
    seq, # Stockholders' equity
    txditc,  # Deferred taxes and investment tax credit
    txdb # Deferred taxes
  ) |>
  collect()

# Manipulate
compustat <- compustat |>
  mutate(year = year(datadate),
         month = floor_date(datadate, "month")) |>
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |>
  ungroup()

# Replace negative values of sales, total assets, capital expenditure and inventory with zero
compustat <- compustat |> 
  mutate(at = if_else(at < 0, NA_real_, at))

# Save
compustat |>
  dbWriteTable(conn = data_tidy_nse, 
               name = "compustat", 
               value = _,
               overwrite = TRUE)


# Data: Merge -----------------------------------------------------------
# Load linking table
ccmxpf_linktable <- tbl(wrds, in_schema("crsp", "ccmxpf_linktable")) |>
  collect()

# Manipulate
ccmxpf_linktable <- ccmxpf_linktable |>
  filter(linktype %in% c("LU", "LC") &
           linkprim %in% c("P", "C") &
           usedflag == 1) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  mutate(linkenddt = replace_na(linkenddt, Sys.Date()))

ccm_links <- crsp_monthly |>
  inner_join(ccmxpf_linktable, by = "permno", multiple = "all") |>
  filter(!is.na(gvkey) & (month >= linkdt & month <= linkenddt)) |>
  select(permno, gvkey, month)

# Add to crsp_monthly
crsp_monthly <- crsp_monthly |>
  left_join(ccm_links, by = c("permno", "month"))

# Save 
crsp_monthly |>
  dbWriteTable(conn = data_tidy_nse, 
               name = "crsp_monthly", 
               value = _,
               overwrite = TRUE)


# Data: Sorting variable ------------------------------------------------
# Lag variable
compustat_lag <- compustat |> 
  select(gvkey, year, at) |> 
  mutate(year = year + 1) |> 
  rename_with(.cols = at, ~ paste0(.x, "_lag"))

# Compute asset growth
compustat <- compustat |> 
  left_join(compustat_lag, by = c("gvkey", "year")) |> 
  mutate(sv_ag = (at - at_lag) / at_lag)

# Compute filters
compustat <- compustat |> 
  mutate(filter_be = coalesce(seq, ceq + pstk, at - lt) + coalesce(txditc, txdb + itcb, 0) - coalesce(pstkrv, pstkl, pstk, 0),
         filter_price = prcc_f,
         filter_earnings = ib)

# Select required variables
compustat <- compustat |> 
  select(gvkey, month, datadate, starts_with("filter_"), starts_with("sv_")) |> 
  drop_na()


# Data: CRSP filter -----------------------------------------------------
crsp_monthly_filter <- crsp_monthly |>
  group_by(permno) |>
  arrange(month) |>
  mutate(filter_stock_age = as.numeric(difftime(month, min(month), units = "days"))/365,
         month = month %m+% months(1)) |> 
  ungroup() |> 
  select(permno, month, filter_stock_age)

crsp_monthly <- crsp_monthly |> 
  select(permno, gvkey, month, industry, exchange, mktcap, mktcap_lag, ret_excess) |> 
  left_join(crsp_monthly_filter, by = c("permno", "month")) |> 
  drop_na()


# Data: Sorting grid ----------------------------------------------------
# Create sorting grid
setup_grid <- expand_grid(sorting_variable = "sv_ag",
                          drop_smallNYSE_at = c(0, 0.05, 0.2),
                          include_financials = c(TRUE, FALSE),
                          include_utilities = c(TRUE, FALSE),
                          drop_bookequity = c(TRUE, FALSE),
                          drop_earnings = c(TRUE, FALSE),
                          drop_stock_age_at = c(0, 2),
                          drop_price_at = c(0, 1, 5),
                          sv_lag = c("3m", "6m", "FF"),
                          formation_time = c("monthly", "FF"),
                          n_portfolios_main = c(5, 10),
                          sorting_method = c("single", "dbl_ind", "dbl_dep"),
                          n_portfolios_secondary = c(2, 5),
                          exchanges = c("NYSE", "NYSE|NASDAQ|AMEX"),
                          value_weighted = c(TRUE, FALSE))

# Remove information on double sorting for univariate sorts
setup_grid <- setup_grid |> 
  filter(!(sorting_method == "single" & n_portfolios_secondary > 2)) |> 
  mutate(n_portfolios_secondary = case_when(sorting_method == "single" ~ NA_real_, 
                                            TRUE ~ n_portfolios_secondary))


# Data: FF-merging ------------------------------------------------------
data_FF <- crsp_monthly |> 
  mutate(sorting_date = month) |> 
  left_join(compustat |> 
              mutate(sorting_date = floor_date(datadate, "year") %m+% months(18),
                     sv_date_COMP_y = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date"))

# Fill variables and ensure timeliness of data
data_FF <- data_FF |> 
  arrange(permno, month) |> 
  group_by(permno, gvkey) |> 
  fill(starts_with("sv_"), starts_with("filter_")) |> 
  ungroup() |> 
  filter(sv_date_COMP_y > month %m-% months(12)) |>
  select(-sv_date_COMP_y, -sorting_date, -gvkey)


# Data: 3m-merging ------------------------------------------------------
# 3 months
## Merge data
data_3m <- crsp_monthly |> 
  mutate(sorting_date = month) |> 
  left_join(compustat |> 
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(3),
                     sv_date_COMP_y = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date"))

## Fill variables and ensure timeliness of data
data_3m <- data_3m |> 
  arrange(permno, month) |> 
  group_by(permno, gvkey) |>
  fill(starts_with("sv_"), starts_with("filter_")) |> 
  ungroup() |> 
  filter(sv_date_COMP_y > month %m-% months(12)) |>
  select(-sv_date_COMP_y, -sorting_date, -gvkey) 


# Data: 6m-merging ------------------------------------------------------
## Merge data
data_6m <- crsp_monthly |> 
  mutate(sorting_date = month) |> 
  left_join(compustat |> 
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(6),
                     sv_date_COMP_y = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date"))

## Fill variables and ensure timeliness of data
data_6m <- data_6m |> 
  arrange(permno, month) |> 
  group_by(permno, gvkey) |>
  fill(starts_with("sv_"), starts_with("filter_")) |> 
  ungroup() |> 
  filter(sv_date_COMP_y > month %m-% months(12)) |>
  select(-sv_date_COMP_y, -sorting_date, -gvkey) 


# Function: Data selection ----------------------------------------------
handle_data <- function(include_financials, include_utilities, 
                        drop_smallNYSE_at, drop_price_at, drop_stock_age_at, 
                        drop_earnings, drop_bookequity, 
                        sv_lag) {
  
  # Select dataset
  if(sv_lag == "FF") data_all <- data_FF
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
  
  # Book equity filter
  if(drop_bookequity) {
    data_all <- data_all |> 
      filter(filter_be > 0)
  }
  
  # Earnings filter
  if(drop_earnings) {
    data_all <- data_all |> 
      filter(filter_earnings > 0)
  }
  
  # Stock age filter
  if(drop_stock_age_at > 0) {
    data_all <- data_all |> 
      filter(filter_stock_age >= drop_stock_age_at)
  }
  
  # Price filter
  if(drop_price_at > 0) {
    data_all <- data_all |> 
      filter(filter_price >= drop_price_at)
  }
  
  # Define ME
  data_all <- data_all |> 
    mutate(me = mktcap_lag) |> 
    drop_na(me) |> 
    select(-starts_with("filter_"), -industry)
  
  # Return
  return(data_all)
}


# Function: Portfolio assignment ----------------------------------------
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
    print(breakpoints)
    cat(paste0("\n Breakpoint issue! Month ", as.Date(as.numeric(cur_group())), "\n"))
    stop()
  }
}


# Function: Single, double sorts ----------------------------------------
# Single sorts
sort_single <- function(data, sorting_variable, exchanges, n_portfolios_main) {
  data |> 
    group_by(month) |> 
    mutate(portfolio = assign_portfolio(data = pick(all_of(sorting_variable), exchange),
                                        sorting_variable = sorting_variable,
                                        n_portfolios = n_portfolios_main,
                                        exchanges = exchanges)) |> 
    drop_na(portfolio) |> 
    ungroup()
}

# Independent double sorts 
sort_double_ind <- function(data, sorting_variable, exchanges, n_portfolios_main, n_portfolios_secondary) {
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
    ungroup()
}

# Dependent double sorts 
sort_double_dep <- function(data, sorting_variable, exchanges, n_portfolios_main, n_portfolios_secondary) {
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
    ungroup()
}


# Function: Monthly and annual rebalancing ------------------------------
# Monthly rebalancing
rebalance_mon <- function(data, sorting_variable, sorting_method, 
                          n_portfolios_main, n_portfolios_secondary, 
                          exchanges, value_weighted) {
  # Single sort
  if(sorting_method == "single") {
    data_rets <- data |> 
      sort_single(sorting_variable = sorting_variable, 
                  exchanges = exchanges, 
                  n_portfolios_main = n_portfolios_main) |> 
      group_by(month, portfolio) |> 
      summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
                .groups = "drop")
  }
  
  # Double independent sort
  if(sorting_method == "dbl_ind") {
    data_rets <- data |> 
      sort_double_ind(sorting_variable = sorting_variable, 
                      exchanges = exchanges, 
                      n_portfolios_main = n_portfolios_main,
                      n_portfolios_secondary = n_portfolios_secondary) |> 
      group_by(month, portfolio) |>
      summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
                portfolio_main = unique(portfolio_main),
                .groups = "drop") |>
      group_by(month, portfolio_main) |>
      summarize(ret = mean(ret),
                .groups = "drop") |> 
      rename(portfolio = portfolio_main)
  }
  
  # Double dependent sort
  if(sorting_method == "dbl_dep") {
    data_rets <- data |> 
      sort_double_dep(sorting_variable = sorting_variable, 
                      exchanges = exchanges, 
                      n_portfolios_main = n_portfolios_main,
                      n_portfolios_secondary = n_portfolios_secondary) |> 
      group_by(month, portfolio) |>
      summarize(ret = if_else(value_weighted, weighted.mean(ret_excess, mktcap_lag), mean(ret_excess)),
                portfolio_main = unique(portfolio_main),
                .groups = "drop") |>
      group_by(month, portfolio_main) |>
      summarize(ret = mean(ret),
                .groups = "drop") |> 
      rename(portfolio = portfolio_main)
  }
  
  return(data_rets)
}

# Annual rebalancing
rebalance_ann <- function(data, sorting_variable, sorting_method, 
                          n_portfolios_main, n_portfolios_secondary, 
                          exchanges, value_weighted) {
  data_sorting <- data |> 
    filter(month(month) == 7) |>
    group_by(month)
  
  # Single sort
  if(sorting_method == "single") {
    # Assign portfolios 
    data_sorting <- data_sorting |> 
      sort_single(sorting_variable = sorting_variable, 
                  exchanges = exchanges, 
                  n_portfolios_main = n_portfolios_main) |> 
      select(permno, month, portfolio) |> 
      mutate(sorting_month = month)
  }
  
  # Double independent sort
  if(sorting_method == "dbl_ind") {
    # Assign portfolios
    data_sorting <- data_sorting |> 
      sort_double_ind(sorting_variable = sorting_variable, 
                      exchanges = exchanges, 
                      n_portfolios_main = n_portfolios_main,
                      n_portfolios_secondary = n_portfolios_secondary) |> 
      select(permno, month, portfolio, portfolio_main, portfolio_secondary) |> 
      mutate(sorting_month = month)
  }
  
  # Double dependent sort
  if(sorting_method == "dbl_dep") {
    # Assign portfolios
    data_sorting <- data_sorting |> 
      sort_double_dep(sorting_variable = sorting_variable, 
                      exchanges = exchanges, 
                      n_portfolios_main = n_portfolios_main,
                      n_portfolios_secondary = n_portfolios_secondary)  |> 
      select(permno, month, portfolio, portfolio_main, portfolio_secondary) |> 
      mutate(sorting_month = month)
  }
  
  # Compute portfolio return
  if(sorting_method == "single") {
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
  } else {
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
}


# Function: Execution function ------------------------------------------
execute_sorts <- function(sorting_variable, drop_smallNYSE_at, include_financials, include_utilities, drop_bookequity,
                          drop_earnings, drop_stock_age_at, drop_price_at, sv_lag, formation_time, n_portfolios_main,
                          sorting_method, n_portfolios_secondary, exchanges, value_weighted) {
  # Select data
  data_sorts <- handle_data(include_financials = include_financials, 
                            include_utilities = include_utilities, 
                            drop_smallNYSE_at = drop_smallNYSE_at,  
                            drop_price_at = drop_price_at,
                            drop_stock_age_at = drop_stock_age_at,
                            drop_earnings = drop_earnings, 
                            drop_bookequity = drop_bookequity,
                            sv_lag = sv_lag)
  
  # Rebalancing
  ## Monthly
  if(formation_time == "monthly") {
    data_return <- rebalance_mon(data = data_sorts, 
                                 sorting_variable = sorting_variable, 
                                 sorting_method = sorting_method, 
                                 n_portfolios_main = n_portfolios_main, 
                                 n_portfolios_secondary = n_portfolios_secondary, 
                                 exchanges = exchanges, 
                                 value_weighted = value_weighted)
  }
  
  ## Monthly
  if(formation_time == "FF") {
    data_return <- rebalance_ann(data = data_sorts, 
                                 sorting_variable = sorting_variable, 
                                 sorting_method = sorting_method, 
                                 n_portfolios_main = n_portfolios_main, 
                                 n_portfolios_secondary = n_portfolios_secondary, 
                                 exchanges = exchanges, 
                                 value_weighted = value_weighted)
  }
  
  # Compute return differential
  data_return |> 
    group_by(month) |> 
    summarize(premium = ret[portfolio == max(portfolio)] - ret[portfolio == min(portfolio)],
              .groups = "drop") |> 
    pull(premium) |> 
    mean() * -100
}


# Implementation --------------------------------------------------------
# Parallel environment
plan(multisession, workers = availableCores())

# Implementation of sorts
data_premia <- setup_grid |>
  mutate(premium_estimate = future_pmap(
    .l = list(
      sorting_variable, drop_smallNYSE_at, include_financials, include_utilities, drop_bookequity,
      drop_earnings, drop_stock_age_at, drop_price_at, sv_lag, formation_time, n_portfolios_main,
      sorting_method, n_portfolios_secondary, exchanges, value_weighted
    ),
    .f = ~ execute_sorts(
      sorting_variable = ..1,
      drop_smallNYSE_at = ..2,
      include_financials = ..3,
      include_utilities = ..4,
      drop_bookequity = ..5,
      drop_earnings = ..6,
      drop_stock_age_at = ..7,
      drop_price_at = ..8,
      sv_lag = ..9,
      formation_time = ..10,
      n_portfolios_main = ..11,
      sorting_method = ..12,
      n_portfolios_secondary = ..13,
      exchanges = ..14,
      value_weighted = ..15
    )
  ))


# Plot results ----------------------------------------------------------
data_premia |>
  unnest(premium_estimate) |> 
  ggplot() +
  geom_density(aes(x  = premium_estimate), 
               alpha = 0.25, linewidth = 1.2) + 
  labs(x = "Premium (in \\%, p.m.)",
       y = NULL)