# Construction of daily sorting variables 

# Packages and Setup -----------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)
library(slider)
library(furrr)
library(moments) 

# Access Database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Data -------------------------------------------------------------
# Load CRSP Daily Data
crsp_daily <- dbReadTable(data_nse, "crsp_daily") 

# Load FF daily
factors_ff_daily <- dbReadTable(data_nse, "factors_ff_daily") 


# Dollar trading volume --------------------------------------------
# Adjust dollar trading volume
crsp_dollar_data <- crsp_daily |>
  mutate(month = floor_date(date, "month"),
         dollar_vol = abs(prc) * vol_adj / 1000) |> 
  group_by(permno, month) |>
  summarize(dollar_vol_m = sum(dollar_vol, na.rm = TRUE),
            obs_m = n(),
            .groups = 'drop') |> 
  drop_na()

# Roll over monthly data
## Function for rolling sum of trading volume
roll_sum_dollar <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months | sum(.x$obs_m) < 50, 
                  NA_real_, # if less than 6 months or less than 50 trading days
                  sum(.x$dollar_vol_m) / sum(.x$obs_m)),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

## Apply functions
crsp_dollar_data <- crsp_dollar_data |>  
  arrange(permno, month) |> 
  group_by(permno) |>
  mutate(sv_dtv = roll_sum_dollar(pick(month, dollar_vol_m, obs_m), 6)) |>
  ungroup() |> 
  select(permno, month, sv_dtv) |> 
  drop_na()

# Create monthly sorting variables
sorting_variables_CRSP_daily <- crsp_dollar_data

# Free memory
rm(crsp_dollar_data)


# 52 week high -----------------------------------------------------
# Reload data
crsp_52_data <- crsp_daily |>  
  mutate(month = floor_date(date, "month")) |> 
  select(permno, month, date, prc_adj)

# Monthly max price
crsp_52_data <- crsp_52_data |>  
  group_by(permno, month) |>
  filter(n() > 3) |> 
  mutate(max_price_month = max(prc_adj, na.rm = TRUE)) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  select(permno, month, prc_adj, max_price_month)
  

# Highest daily price over the last 12 months
## Function for rolling maximum over the last 12 months 
roll_max <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months, 
                   NA_real_, 
                   max(.x$max_price_month)),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

## Apply function
crsp_52_data <- crsp_52_data |> 
  arrange(permno, month) |> 
  group_by(permno) |> 
  mutate(max_prc_52_week = roll_max(pick(month, max_price_month), 12), 
         sv_52w = prc_adj / max_prc_52_week ) |> 
  ungroup() |> 
  select(permno, month, sv_52w) |> 
  drop_na()

# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |> 
  full_join(crsp_52_data, by = c("permno", "month"))

# Free memory
rm(crsp_52_data)


# Maximum daily return  --------------------------------------------
# Reload data
crsp_mdr_data <- crsp_daily |> 
  mutate(month = floor_date(date, "month")) |> 
  select(permno, month, date, ret_excess) |> 
  drop_na()

# Max daily return
crsp_mdr_data <- crsp_mdr_data |>   
  group_by(permno, month) |>
  filter(n() >= 15) |> 
  summarize(sv_mdr = max(ret_excess, na.rm = TRUE), # Maximum daily return
            .groups = 'drop') |> 
  select(permno, month, sv_mdr) 

# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |> 
  full_join(crsp_mdr_data, by = c("permno", "month"))

# Free memory
rm(crsp_mdr_data)


# Share turnover ---------------------------------------------------
# Reload data
crsp_tur_data <- crsp_daily |> 
  mutate(month = floor_date(date, "month")) |> 
  select(permno, month, vol_adj, shrout)                                              

# Generate share turnover
crsp_tur_data <- crsp_tur_data |>   
  mutate(turnover = vol_adj / (shrout * 1000)) |> 
  group_by(permno, month) |> 
  summarize(turnover_m = sum(turnover, na.rm = TRUE), 
            obs_m = n(),
            .groups = 'drop') |> 
  select(permno, month, turnover_m, obs_m)

# Roll over monthly data:
## Function for rolling sum over the last 6 months 
roll_sum_turn <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months | sum(.x$obs_m) < 50, 
                   NA_real_,  
                   sum(.x$turnover_m) / sum(.x$obs_m)),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

## Apply function
crsp_tur_data <- crsp_tur_data |>   
  arrange(permno, month) |>
  group_by(permno) |>
  mutate(sv_tur = roll_sum_turn(pick(month, obs_m, turnover_m), 6)) |>
  ungroup() |> 
  select(permno, month, sv_tur) |> 
  drop_na()

# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |>
  full_join(crsp_tur_data, by = c("permno", "month"))

# Free memory
rm(crsp_tur_data)


# Amihud illiquidity ratio -----------------------------------------
# Reload data
crsp_ami_data <- crsp_daily |>
  mutate(month = floor_date(date, "month")) |>
  select(permno, month, vol_adj, prc, ret_excess)                                              

# Amihud measure
crsp_ami_data <- crsp_ami_data |> 
  mutate(dollar_vol = abs(prc) * vol_adj,
         amihud = abs(ret_excess) / dollar_vol,
         amihud = if_else(is.nan(amihud), NA_real_, amihud),
         amihud = if_else(is.infinite(amihud), NA_real_, amihud)) |> 
  group_by(permno, month) |> 
  summarize(amihud_m = sum(amihud, na.rm = TRUE),
            obs_m = n(),
            .groups = 'drop') |> 
  select(permno, month, amihud_m, obs_m)
  
# Roll over monthly data
## Function for rolling sum over the last 6 months 
roll_sum_ami <- function(data, months) {
    data <- bind_rows(data)
    
    returns <- slide_period_vec(
      .x = data,
      .i = data$month,
      .period = "month",
      .f = ~ if_else(nrow(.x) < months | sum(.x$obs_m) < 50, 
                    NA_real_, 
                    sum(.x$amihud_m) / sum(.x$obs_m)),      
      .before = months - 1,
      .complete = FALSE
    )
    
    return(returns)
  }
  
## Apply function  
crsp_ami_data <- crsp_ami_data |>   
  arrange(permno, month) |>  
  group_by(permno) |> 
  mutate(sv_ami = roll_sum_ami(pick(month, obs_m, amihud_m), 6)) |> 
  ungroup() |> 
  select(permno, month, sv_ami) |> 
  drop_na()
  
# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |> 
  full_join(crsp_ami_data, by = c("permno", "month"))
  
# Free memory
rm(crsp_ami_data)


# Ivol & iskew -----------------------------------------------------
# Reload data
crsp_ivol_data <- crsp_daily |> 
  mutate(month = floor_date(date, "month")) |> 
  select(permno, date, month, ret_excess) |> 
  group_by(permno, month) |> 
  filter(n() >= 15) |> 
  ungroup() 

# Function
model_ff <- function(data) {
  data |>
    left_join(factors_ff_daily, by = c("date")) |>
    group_by(month) |>
    mutate(resid_ff = lm(ret_excess ~ mkt_excess + smb + hml)$residuals) |>
    summarize(sv_ivol = sd(resid_ff, na.rm = TRUE),
              sv_iskew = skewness(resid_ff, na.rm = TRUE),
              .groups = 'drop') |> 
    mutate(across(starts_with("sv_"), ~ if_else(.x == 0, NA_real_, .x)),
           across(starts_with("sv_"), ~ if_else(is.nan(.x), NA_real_, .x)),
           across(starts_with("sv_"), ~ if_else(abs(.x) < 10^-8, NA_real_, .x)))
}

# Nest
crsp_ivol_data <- crsp_ivol_data |>
  group_by(permno) |>
  nest()

# Subsets
spacing <- 8 * 25
subsets_data <- nrow(crsp_ivol_data) %/% spacing + 1

# loop
for(set in 1:subsets_data) {
  cat(as.character(Sys.time()))
  
  # Ivol subset
  ivol_data_subset <- crsp_ivol_data |>
    ungroup() |>
    slice(((set-1)*spacing+1):min(nrow(crsp_ivol_data), (set*spacing)))
  
  # Parallel environment
  plan(multisession, workers = 8)
  ivol_data_subset <- ivol_data_subset |>
    mutate(svs = future_map(data, ~ model_ff(.))) |>
    unnest(svs) |>
    select(permno, month, starts_with("sv_"))
  
  # End plan
  plan(sequential)
  
  # Add all together
  if(set == 1) {
    ivol_data_all <- ivol_data_subset
  } else  {
    ivol_data_all <- ivol_data_all |> 
      bind_rows(ivol_data_subset)
  }
  
  # Final report
  rm(ivol_data_subset)
  cat(paste(" - Done", set, "\n"))
}

# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |> 
  full_join(ivol_data_all, by = c("permno", "month"))

# Free memory
rm(ivol_data_all, crsp_ivol_data)


# Cumulative abnormal returns around earnings' announcements -------
# Bring in the earnings announcement dates from Compustat quarterly data
rdq_data <- dbReadTable(data_nse, "compustat_quarterly")  |> 
  select(gvkey, datadate, rdq) |>
  rename(date = rdq) |>
  filter(!is.na(date), date >= datadate) |>                                                         
  distinct(gvkey, date) |> 
  filter(date > as.Date("1969-12-01"))
  
# Reload data and bring in the market return and earnings announcements
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly") |>
  select(permno, gvkey, month)

crsp_car_data <- crsp_daily |> 
  mutate(month = floor_date(date, "month")) |> 
  left_join(factors_ff_daily, by = c("date")) |>
  left_join(crsp_monthly, 
            by = c("permno", "month")) |>
  select(permno, gvkey, date, month, ret_excess, mkt_excess) |> 
  arrange(permno, date) |> 
  group_by(permno) |>
  mutate(ret_minus_mkt = (ret_excess) - (mkt_excess),
         ret_minus_mkt_lag1 = lag(ret_minus_mkt, n = 1),
         ret_minus_mkt_lag2 = lag(ret_minus_mkt, n = 2),
         ret_minus_mkt_for1 = lead(ret_minus_mkt, n = 1)) |>
  ungroup() |> 
  select(permno, gvkey, date, month, starts_with("ret_minus_mkt"))|>
  inner_join(rdq_data, by = c("gvkey", "date")) |>
  select(permno, date, month, starts_with("ret_minus_mkt")) 

# Free memory 
rm(crsp_monthly, rdq_data)

# Calculate CAR
crsp_car_data <- crsp_car_data |>
  mutate(sv_abr = ret_minus_mkt_lag1 + ret_minus_mkt_lag2 + ret_minus_mkt + ret_minus_mkt_for1, 
         sv_abr = if_else(month >= as.Date("1972-01-01"), sv_abr, NA_real_)) |>
  select(permno, month, date, sv_abr)

# Keep only the latest CAR if there is more than one announcement in a month
crsp_car_data <- crsp_car_data |>
  arrange(permno, date) |>
  group_by(permno, month) |>
  slice_tail(n = 1) |>
  ungroup() |> 
  select(permno, month, sv_abr) |>
  drop_na()

# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |> 
  full_join(crsp_car_data, by = c("permno", "month"))

# Free memory
rm(crsp_car_data)


# Frazzini and Pedersen Beta ---------------------------------------
# Functions 
## SD computation
sd_beta <- function(data, variable, minimum = 120, observations = 750) {
  data <- data |> 
    drop_na(all_of(variable))
  
  if(nrow(data) < minimum) return(NA_real_)
  
  data |> 
    slice_tail(n = observations) |> 
    pull(all_of(variable)) |> 
    sd()
}

## Correlation computation
sd_corr <- function(data, minimum = 120, observations = 750) {
  if(nrow(data) < minimum) return(NA_real_) 
  if(length(unique(data$ret_3)) < 10) return(NA_real_) # Some stocks have constant prices
  
  data <- data |> 
    slice_tail(n = observations) 
  
  cor(data$ret_3, 
      data$mkt_3, 
      use = 'pairwise.complete.obs')
}

## Rolling standard deviation
roll_sd_beta <- function(data, variable, minimum = 120, observations = 750) {
  sds <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ sd_beta(data = .,
                   variable = variable,
                   minimum = minimum,
                   observations = observations),
    .before = 36 - 1,
    .complete = FALSE
  )
  
  results <- tibble(month = unique(data$month),
                    sd = sds)
  
  return(results)
}

## Rolling correlation
roll_cor_beta <- function(data, minimum = 120, observations = 750) {
  corrs <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ sd_corr(data = .,
                   minimum = minimum,
                   observations = observations),
    .before = 36 - 1,
    .complete = FALSE
  )
  
  results <- tibble(month_cor = unique(data$month),
                    corr = corrs)
  
  return(results)
}

# Calculate rolling standard deviations for the market index 
market_factors <- factors_ff_daily |>
  arrange(date) |> 
  mutate(mkt = log(1 + (mkt_excess + rf)),
         mkt_3 = mkt + lead(mkt, n = 1) + lead(mkt, n = 2)) |>
  select(date, mkt_3, rf)

# Rolling market SD
market_factors_sds <- factors_ff_daily |>
  select(date, mkt_excess, rf) |> 
  arrange(date) |>
  mutate(month = floor_date(date, "month"),
         mkt = log(1 + (mkt_excess + rf))) |> 
  roll_sd_beta(variable = "mkt",
               minimum = 120,
               observations = 750) |>
  select(month, sd_mkt = sd)

# Data for rolling beta
crsp_beta_data <- crsp_daily |> 
  mutate(month = floor_date(date, "month")) |> 
  inner_join(market_factors, by = c("date")) |>
  arrange(date) |> 
  mutate(ret = log(1 + (ret_excess + rf))) |> 
  group_by(permno) |> 
  mutate(ret_3 = ret + lead(ret, n = 1) + lead(ret, n = 2)) |>
  ungroup() |> 
  select(permno, date, month, ret, ret_3, mkt_3)

# Calculate rolling standard deviation for stocks 
# Nest
crsp_data_nest <- crsp_beta_data |>
  drop_na() |> 
  arrange(permno, date) |> 
  group_by(permno) |>
  nest()

# Subsets
spacing <- 8 * 25
subsets_data <- nrow(crsp_data_nest) %/% spacing + 1

# loop
for(set in 1:subsets_data) {
  cat(as.character(Sys.time()))
  
  # Beta subset
  beta_subset <- crsp_data_nest |>
    ungroup() |>
    slice(((set-1)*spacing+1):min(nrow(crsp_data_nest), (set*spacing)))
  
  # Parallel environment
  plan(multisession, workers = 8)
  beta_subset <- beta_subset |>
    mutate(ret_sds = future_map(data, ~ roll_sd_beta(data = .,
                                                    variable = "ret",
                                                    minimum = 120,
                                                    observations = 750)),
           ret_cor = future_map(data, ~ roll_cor_beta(data = .,
                                                      minimum = 120,
                                                      observations = 750))) |>
    unnest(c(ret_sds, ret_cor)) |>
    select(permno, month, month_cor, sd_ret = sd, corr)
  
  # Break (just a safety feature)
  if(!all(beta_subset$month == beta_subset$month_cor)) stop("Time error")
  beta_subset <- beta_subset |> 
    select(-month_cor)
  
  # End plan
  plan(sequential)
  
  # Add all together
  if(set == 1) {
    beta_all <- beta_subset
  } else  {
    beta_all <- beta_all |> 
      bind_rows(beta_subset)
  }
  
  # Final report
  rm(beta_subset)
  cat(paste(" - Done", set, "\n"))
}

# Monthly market factors
market_factors <- market_factors |> 
  arrange(date) |> 
  mutate(month = floor_date(date, "month")) |> 
  group_by(month) |> 
  slice_tail(n = 1) |> 
  ungroup()

# Combine and calculate  measure
bfp_final <- beta_all |> 
  left_join(market_factors_sds, by = "month") |> 
  mutate(sv_bfp = (corr * sd_ret) / sd_mkt) |> 
  select(permno, month, sv_bfp) |> 
  drop_na()

# Merge into our monthly SV Dataset 
sorting_variables_CRSP_daily <- sorting_variables_CRSP_daily |> 
    full_join(bfp_final, by = c("permno", "month"))
  
# Free memory
rm(beta_all, market_factors_sds, market_factors, crsp_data_nest)


# Store variables --------------------------------------------------
sorting_variables_CRSP_daily |> 
  dbWriteTable(conn = data_nse, 
               name = "sorting_variables_CRSP_daily",
               value = _, 
               overwrite = TRUE)
