

# Construction of sorting variables 


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
library(furrr)
library(moments)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Daily Variables ---------------------------------------------------------



# Data --------------------------------------------------------------------


# Load CRSP Daily Data
crsp_daily <- dbReadTable(data_nse, "crsp_daily") %>%
  select(permno, month, ret_excess, prc, vol)

# Load sorting variables
sorting_variables_CRSP <- dbReadTable(data_nse, "sorting_variables_CRSP")

# Load FF daily
factors_ff_daily <- dbReadTable(data_nse, "factors_ff_daily") %>%
  select(-rf)

# Check for duplicates: (gvkey and month are not unique!!)

crsp_daily %>%
  distinct(permno, month) %>% 
  nrow() == nrow(crsp_daily %>% drop_na(permno)) # Check if distinct observations = starting number

crsp_daily %>%
  distinct(permno, month) %>% 
  nrow() == nrow(crsp_daily) # Check if distinct observations = starting number


# Dollar trading volume ---------------------------------------------------

# Dates
date_1 <- as.Date("2001-02-01")
date_2 <- as.Date("2001-12-31")
date_3 <- as.Date("2002-01-01")
date_4 <- as.Date("2004-01-01")

# Adjust dollar trading volume according to Gao and Ritter (2010)
dollar_data <- crsp_daily %>%
  select(permno, month, vol, prc, exchcd) %>%
  arrange(permno, month) %>%
  mutate(vol = ifelse(vol == -99, as.numeric(NA), vol) ,
         vol_adj = ifelse(exchcd ==3 & month < date_1, vol /2 , vol) ,
         vol_adj = ifelse(exchcd ==3 & month >= date_1 & month <= date_2, vol /1.8 , vol_adj) ,
         vol_adj = ifelse(exchcd ==3 & month >= date_3 & month < date_4, vol /1.6 , vol_adj) ,
         vol_adj = ifelse(exchcd ==3 & month >= date_4 , vol /1.0 , vol_adj) ,
         dollar_vol = abs(prc) * vol_adj, 
         mon = floor_date(month, "month")) %>%
  select(permno, month, mon, dollar_vol)

# Monthly mean dollar trading volume
dollar_data <- dollar_data %>% 
  group_by(permno, mon) %>%
  summarise(dollar_vol_m_av = mean(dollar_vol, na.rm =TRUE),
            max_index = n()) %>%
  select(permno, month = mon, dollar_vol_m_av, max_index) 

# 6 month moving average of dollar trading volume

# Function for rolling window estimates
roll_trading_vol <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ ifelse(nrow(.x) < months, NA, ifelse(sum(.x$max_index, na.rm = T) < 50, NA, mean(.x$dollar_vol_m_av))),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

# Function call
dollar_data <- dollar_data %>% 
  arrange(permno, month) %>% 
  group_by(permno) %>% 
  mutate(sv_dtv = roll_trading_vol(cur_data(), 6))%>% # TODO: Check
  select(permno, month, sv_dtv)

# Merge into our monthly SV dataset 
sorting_variables_CRSP <- sorting_variables_CRSP %>%
  left_join(dollar_data, by = c("permno", "month"))

rm(dollar_data)


# Ivol & iskew ------------------------------------------------------------

# Data
ivol_data <- crsp_daily %>%
  mutate(day = month,
         month = floor_date(month, "month")) %>%
  select(permno, day, month, ret_excess) %>%
  group_by(permno, month) %>%
  mutate(max_index = n()) %>%
  ungroup() %>%
  filter(max_index >= 15) %>%
  select(-max_index)

# Remove main set to save space
rm(crsp_daily)
gc()

# Function
model_capm_ff <- function(data) {
  data %>%
    left_join(factors_ff_daily, by = c("day" = "month")) %>%
    group_by(month) %>%
    mutate(resid_c = lm(ret_excess ~ mkt_excess, data = cur_data())$residuals,
           resid_ff = lm(ret_excess ~ mkt_excess + smb + hml, data = cur_data())$residuals) %>%
    summarise(sv_ivolc = sd(resid_c, na.rm = TRUE),
              sv_iscc = skewness(resid_c, na.rm = TRUE),
              sv_ivolff = sd(resid_ff, na.rm = TRUE),
              sv_iscff = skewness(resid_ff, na.rm = TRUE),
              .groups = 'drop')
}

# Nest
ivol_data <- ivol_data %>%
  group_by(permno) %>%
  nest()
gc()

# Subsets
spacing <- 8 * 25
subsets_data <- nrow(ivol_data) %/% spacing + 1

# loop
for(set in 1:subsets_data) {
  cat(as.character(Sys.time()))
  
  # Ivol subset
  ivol_data_subset <- ivol_data %>%
    ungroup() %>%
    slice(((set-1)*spacing+1):min(nrow(ivol_data), (set*spacing)))
  
  # Parallel environment
  plan(multisession, workers = 8)
  ivol_data_subset <- ivol_data_subset %>%
    mutate(svs = future_map(data, ~ model_capm_ff(.))) %>%
    unnest(svs) %>%
    select(permno, month, starts_with("sv_"))
  
  # End plan
  plan(sequential)
  
  # Add all together
  if(set == 1) {
    ivol_data_all <- ivol_data_subset
  } else  {
    ivol_data_all <- ivol_data_all %>% 
      bind_rows(ivol_data_subset)
  }
  
  # Final report
  rm(ivol_data_subset)
  cat(paste(" - Done", set, "\n"))
}

# Merge into our monthly SV Dataset 
sorting_variables_CRSP <- sorting_variables_CRSP %>%
  left_join(ivol_data_all, by = c("permno", "month"))

# Remove errors
removals <- function(data) {
  data[which(data == 0)] <- NA
  data[which(is.nan(data))] <- NA
  data[which(abs(data) < 10^-8)] <- NA

  return(data)
}

sorting_variables_CRSP <- sorting_variables_CRSP %>%
  mutate(across(starts_with("sv_i"), ~ removals(.x)))

# Store variables ---------------------------------------------------------

# Store
sorting_variables_CRSP %>%
  dbWriteTable(data_nse, "sorting_variables_CRSP", ., overwrite = TRUE)
