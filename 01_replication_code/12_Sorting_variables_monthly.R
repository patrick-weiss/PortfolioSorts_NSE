# Construction of monthly sorting variables 
  
# Packages and Setup -----------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)
library(slider)

# Access Database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

# Load data --------------------------------------------------------
# CRSP
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")

# FF5
factors_ff_monthly <- dbReadTable(data_nse, "factors_ff_monthly")

# Merge FF5 factors
crsp_monthly <- crsp_monthly |>
  left_join(factors_ff_monthly |> 
              select(month, rf, mkt_excess, smb, hml), by = "month")


# Lag variables ----------------------------------------------------
# Lag by one month
crsp_monthly_lag_1 <- crsp_monthly |>
  select(permno, month, altprc) |>
  mutate(month = month %m+% months(1)) |>
  rename_with(.cols = altprc, ~ paste0(.x, "_lag1")) |>
  select(permno, month, ends_with("_lag1"))

# Lag by five years
crsp_monthly_lag_5 <- crsp_monthly |>
  select(permno, month, mktcap) |>
  mutate(month = month %m+% years(5)) |>
  rename_with(.cols = mktcap, ~ paste0(.x, "_lag5")) |>
  select(permno, month, ends_with("_lag5"))

# Remerge
crsp_monthly <- crsp_monthly |>
  left_join(crsp_monthly_lag_1, by = c("permno", "month")) |>
  left_join(crsp_monthly_lag_5, by = c("permno", "month"))

# Free memory
rm(crsp_monthly_lag_1, crsp_monthly_lag_5)


# Residual Momentum ------------------------------------------------
# Functions
## Compute residuals 
compute_residuals <- function(data) {
  lm(ret_excess ~ mkt_excess + smb + hml, data = data)$residuals |> 
    tail(n = 1)
}

## Rolling residual estimation
roll_residual <- function(data, months) {
  data <- bind_rows(data)
  
  residuals <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months, 
                   NA_real_, 
                   compute_residuals(.x)),
    .before = months - 1,
    .complete = FALSE
  )
  
  tibble(
    month = unique(data$month),
    residual = residuals
  )
}

## Rolling means
roll_mean_over_sd <- function(data, months) {
  data <- bind_rows(data)
  
  means <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months, 
                   NA_real_, 
                   mean(.x$residual) / sd(.x$residual)),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(means)
}

# Compute residuals
## Nest data
residuals_nested <- crsp_monthly |>
  select(permno, month, ret_excess, mkt_excess, smb, hml) |>
  arrange(month) |> 
  group_by(permno) |> 
  nest()

## Compute residuals
residuals_nested <- residuals_nested |> 
  mutate(residual = map(data, ~ roll_residual(., months = 36))) |> 
  unnest(c(residual)) |> 
  select(permno, month, residual) |> 
  drop_na()

# Calculate mean and volatility of residuals 
crsp_residuals <- residuals_nested |>
  arrange(month) |>
  group_by(permno) |>
  mutate(sv_rmom = roll_mean_over_sd(pick(month, residual), 
                                     months = 11)) |>
  ungroup() |> 
  select(permno, month, sv_rmom) 

# Skip first month for momentum - Typically the first month in the past for the signal is skipped: short term reversal effect 
crsp_residuals <- crsp_residuals |>
  mutate(month = month %m+% months(1))

# Remerge
crsp_monthly <- crsp_monthly |>
  left_join(crsp_residuals, by = c("permno", "month"))

# Free memory
rm(crsp_residuals, residuals_nested)


# Market beta ------------------------------------------------------
# Functions
## Calculate market beta over the last 60 months
compute_beta <- function(data) {
  coefficients(lm(ret_excess ~ mkt_excess, data = data))[2]
}

## Rolling beta estimation
roll_beta <- function(data, months, min_obs) {
  data <- bind_rows(data)
  
  residuals <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < min_obs, 
                   NA_real_, 
                   compute_beta(.x)),
    .before = months - 1,
    .complete = FALSE
  )
}

# Set data
beta_data <- crsp_monthly |>
  select(permno, month, ret, rf, mkt_excess) |>
  arrange(permno, month) |>
  mutate(ret_excess = ret - rf) |>
  drop_na() |> 
  select(permno, month, ret_excess, mkt_excess) 

# Compute rolling betas
beta_data <- beta_data |>
  arrange(permno, month) |>
  group_by(permno) |>
  mutate(sv_beta = roll_beta(data = pick(month, ret_excess, mkt_excess),
                             months = 60,
                             min_obs = 24)) |> # At least 24 months of observations
  ungroup() |> 
  select(permno, month, sv_beta) |>
  drop_na()

# Remerge
crsp_monthly <- crsp_monthly |>
  left_join(beta_data, by = c("permno", "month"))

# Free memory
rm(beta_data)


# Other sorting variables ------------------------------------------
# Function for rolling window estimates
roll_returns <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months, 
                   NA_real_, 
                   sum(log(1 + .x$ret))),             
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

sorting_variables_CRSP_monthly <- crsp_monthly |>
  group_by(permno) |>
  arrange(month) |>
  mutate(sv_me = log(shrout * 10^3 * abs(altprc)), # Size 
         sv_srev = if_else(is.na(altprc_lag1), NA_real_, ret), # Short-term reversal
         sv_mom = exp(roll_returns(pick(month, ret), months = 11)) - 1, # Momentum
         sv_csi = log(mktcap / mktcap_lag5) - roll_returns(pick(month, ret), months = 60), # Composite share issuance
         sv_rev = exp(roll_returns(pick(month, ret), months = 48)) - 1, # Long-term reversal 
         filter_stock_age = as.numeric(difftime(month, min(month), units = "days"))/365) |> # Stock age filter
  ungroup() |> 
  select(permno, month, starts_with("sv_"), starts_with("filter_"))

# Skip first month for momentum
# Needed for the signal construction and has nothing to do with aligning with CRSP (standard lagging of one month) - Compare short reversal effects 
sorting_variables_mom <- sorting_variables_CRSP_monthly |>
   select(permno, month, sv_mom) |>
   mutate(month = month %m+% months(1))

# Skip first 13 months for long-term reversal                                   
# Needed for the signal construction and has nothing to do with aligning with CRSP (standard lagging of one month) - Compare momentum effect 
sorting_variables_rev <- sorting_variables_CRSP_monthly |>
  select(permno, month, sv_rev) |>
  mutate(month = month %m+% months(13))

# Remerge
sorting_variables_CRSP_monthly <- sorting_variables_CRSP_monthly |>
  select(-sv_rev, -sv_mom) |>
  left_join(sorting_variables_mom, by = c("permno", "month")) |>
  left_join(sorting_variables_rev, by = c("permno", "month"))


# Filters ----------------------------------------------------------
# Remove Inf and NaN
sorting_variables_CRSP_monthly <- sorting_variables_CRSP_monthly |>
  mutate(across(starts_with("sv_"), ~ na_if(., Inf)),
         across(starts_with("sv_"), ~ na_if(., -Inf)),
         across(starts_with("sv_"), ~ na_if(., NaN)))


# Store variables --------------------------------------------------
# Load daily SVs
sorting_variables_CRSP_daily <- dbReadTable(data_nse, "sorting_variables_CRSP_daily")

# Merge
sorting_variables_CRSP <- sorting_variables_CRSP_monthly |> 
  full_join(sorting_variables_CRSP_daily, by = c("permno", "month"))

# Store
sorting_variables_CRSP |> 
  dbWriteTable(conn = data_nse, 
               name = "sorting_variables_CRSP",
               value = _, 
               overwrite = TRUE)