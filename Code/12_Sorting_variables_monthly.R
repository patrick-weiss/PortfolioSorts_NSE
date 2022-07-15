

# Construction of monthly sorting variables 


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Packages and Setup ------------------------------------------------------

# Packages 
library(tidyverse)
library(RSQLite)
library(DBI)
library(lubridate)
library(zoo)
library(slider)
library(rollRegres)
library(matrixStats)
library(JWileymisc)
library(modelr)
library(broom)
library(moments)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Monthly Variables -------------------------------------------------------

# Load CRSP

crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")
factors_ff_monthly <- dbReadTable(data_nse, "factors_ff_monthly")

# Check for duplicates: (gvkey and month are not unique!!)

crsp_monthly %>%
  distinct(gvkey, month) %>% 
  nrow() == nrow(crsp_monthly %>% drop_na(gvkey)) # Check if distinct observations = starting number

crsp_monthly %>%
  distinct(permno, month) %>% 
  nrow() == nrow(crsp_monthly) # Check if distinct observations = starting number

crsp_monthly <- crsp_monthly %>%
  left_join(factors_ff_monthly %>% select(month, rf, mkt_excess, smb, hml), by = "month")

# Lag variables

crsp_monthly_lag <- crsp_monthly %>%
  select(permno, month, mktcap) %>%
  mutate(month = month %m+% years(5)) %>%
  rename_with(.cols = mktcap, ~ paste0(.x, "_lag5")) %>%
  select(permno, month, ends_with("_lag5"))

# Remerge

crsp_monthly <- crsp_monthly %>%
  left_join(crsp_monthly_lag, by = c("permno", "month"))

rm(crsp_monthly_lag)

# 1. Residual Momentum 

crsp_monthly2 <- crsp_monthly %>%
  select(permno, month, ret_excess, mkt_excess, smb, hml) %>%
  arrange(permno, month)

# Get residuals 

Coef <- . %>% as.data.frame %>% lm %>% coef

output <- crsp_monthly2 %>% 
  group_by(permno) %>% 
  do(cbind(reg_col = select(., ret_excess, mkt_excess, smb, hml) %>% rollapplyr(36, Coef, by.column = FALSE, fill = NA),date_col = select(., month))) %>%
  ungroup %>%
  rename(., cons = "reg_col.(Intercept)",
         b_mkt = "reg_col.mkt_excess",
         b_smb = "reg_col.smb", 
         b_hml = "reg_col.hml")

crsp_monthly2 <- crsp_monthly2  %>%
  left_join(output, by = c("permno", "month")) %>%
  mutate(residual = ret_excess - (cons + (b_mkt * mkt_excess) + (b_smb * smb) + (b_hml * hml))) %>% 
  select(permno, month, residual)

# Calculate mean and volatility of residuals 

# Function for rolling mean estimates

roll_mean <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ ifelse(nrow(.x) < months, NA, mean(.x$residual)),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

roll_sd <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ ifelse(nrow(.x) < months, NA, sd(.x$residual)),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

crsp_monthly2 <- crsp_monthly2 %>%
  arrange(permno, month) %>%
  select(permno, month, residual) %>%
  group_by(permno) %>%
  mutate(e_6_mean = roll_mean(cur_data(), 6), 
         e_6_sd = roll_sd(cur_data(), 6),
         e_11_mean = roll_mean(cur_data(), 11), 
         e_11_sd = roll_sd(cur_data(), 11), 
         sv_e_6 = e_6_mean / e_6_sd, 
         sv_e_11 = e_11_mean / e_11_sd) %>%
  select(permno, month, sv_e_6, sv_e_11)

# Skip first month for momentum

crsp_monthly2 <- crsp_monthly2 %>%
  mutate(month = month %m+% months(1)) 

# Remerge
crsp_monthly <- crsp_monthly %>%
  left_join(crsp_monthly2, by = c("permno", "month"))

rm(crsp_monthly2)

# 2. Compute other sorting variables 

# Function for rolling window estimates
roll_returns <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ ifelse(nrow(.x) < months, NA, sum(log(1 + .x$ret))),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

sorting_variables_crsp <- crsp_monthly %>%
  group_by(permno) %>%
  arrange(month) %>%
  mutate(sv_size = log(mktcap),                                                                  # Size 
         sv_mom = exp(roll_returns(cur_data(), 11)) - 1,                                         # Momentum
         sv_mom_6 = exp(roll_returns(cur_data(), 6)) - 1,                                        # Momentum
         sv_csi = log(mktcap / mktcap_lag5) - roll_returns(cur_data(), 60),                      # Composite share issuance
         filter_stock_age = as.numeric(difftime(month, min(month), units = "days"))/365) %>%     # Stock age filter
  select(permno, month, starts_with("sv_"), starts_with("filter_"))

# TODO: Check if the version above is the same, slide_period is more robust for monthly sliders
# sorting_variables_crsp <- crsp_monthly %>%
#   group_by(permno) %>%
#   arrange(month) %>%
#   mutate(sv_size = log(mktcap)   ,                                                                                                           # Size 
#          sv_mom = exp(rollapply(log(1+ret), 11, sum, partial = TRUE, align = "right" )) -1,                                             # Momentum
#          sv_mom = ifelse(index >= 11, sv_mom, as.numeric(NA)),
#          sv_csi = log(mktcap / mktcap_lag5) - rollapply(log(1+ret), 60, sum, partial = TRUE, align = "right" ),                          # Composite share issuance
#          sv_csi = ifelse(index >= 60, sv_csi, as.numeric(NA)) 
#   ) %>%
#   select(gvkey, permno, month, starts_with("sv_"))

# Skip first month for momentum
sorting_variables_mom <- sorting_variables_crsp %>%
  select(permno, month, sv_mom, sv_mom_6) %>%
  mutate(month = month %m+% months(1)) %>%
  select(permno, month, sv_mom, sv_mom_6)

# Remerge
sorting_variables_CRSP <- sorting_variables_crsp %>%
  select(-sv_mom, -sv_mom_6) %>%
  left_join(sorting_variables_mom, by = c("permno", "month")) %>%
  select(permno, month, starts_with("sv_"), starts_with("filter_"))

rm(sorting_variables_crsp, sorting_variables_mom)


# Filters -----------------------------------------------------------------

# Remove Inf and NaN
sorting_variables_CRSP <- sorting_variables_CRSP %>%
  mutate(across(starts_with("sv_"), ~ na_if(., Inf)),
         across(starts_with("sv_"), ~ na_if(., -Inf)),
         across(starts_with("sv_"), ~ na_if(., NaN)))

# TODO: should we also trim stupidely large/small values? (note, winsorization would not change anything)


# Store variables ---------------------------------------------------------

# Store
sorting_variables_CRSP %>%
  dbWriteTable(data_nse, "sorting_variables_CRSP", ., overwrite = TRUE)



