# Construction of quarterly sorting variables 

# Packages and Setup -----------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)
library(slider)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

# Quarterly Variables ----------------------------------------------
# Load CRSP
compustat_quarterly <- dbReadTable(data_nse, "compustat_quarterly")  

# Compute sub-variables 
compustat_quarterly <- compustat_quarterly |> 
 mutate(noaq = (atq - cheq - replace_na(ivaoq, 0)) - 
          (atq - replace_na(dlcq, 0) - replace_na(dlttq, 0) - replace_na(mibq, 0) - replace_na(pstkq, 0) - ceqq), 
        beq_part1 = coalesce(seqq, ceqq + pstkq, atq - ltq),
        beq_part2 = coalesce(txditcq, txdbq, 0),
        beq_part3 = coalesce(pstkrq, pstkq, 0),
        beq = beq_part1 + beq_part2 - beq_part3) |>
 select(-starts_with("beq_part"))
 
# Lag variables one quarter 
compustat_quarterly_lag <- compustat_quarterly |> 
 select(gvkey, month, atq, beq, wcapq, noaq) |> 
 mutate(month = month %m+% months(3)) |> 
 rename_with(.cols = atq:noaq, ~ paste0(.x, "_lag"))
 
# Lag variables four quarters
compustat_quarterly_lag4 <- compustat_quarterly |> 
  select(gvkey, month, atq, beq, epspxq, ajexq, saleq, cshprq, txtq, ibq) |> 
  mutate(month = month %m+% months(12)) |> 
  rename_with(.cols = atq:ibq, ~ paste0(.x, "_lag4"))

# Lag variables five quarters
compustat_quarterly_lag5 <- compustat_quarterly |> 
  select(gvkey, month, atq, beq, ibq) |> 
  mutate(month = month %m+% months(15)) |> 
  rename_with(.cols = atq:ibq, ~ paste0(.x, "_lag5"))

# Remerge
compustat_quarterly <- compustat_quarterly |> 
  left_join(compustat_quarterly_lag, by = c("gvkey", "month")) |> 
  left_join(compustat_quarterly_lag4, by = c("gvkey", "month")) |> 
  left_join(compustat_quarterly_lag5, by = c("gvkey", "month"))

# Free space
rm(compustat_quarterly_lag, compustat_quarterly_lag4, compustat_quarterly_lag5)


# Compute cash flow volatility -------------------------------------
# Function for rolling standard deviation 
roll_sd <- function(data, quarters, min_obs) {
  data <- bind_rows(data)
  
  variable <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "quarter",
    .f = ~ if_else(sum(!is.na(.x$ocf)) < min_obs, 
                   NA_real_, 
                   sd(.x$ocf, na.rm = TRUE)),
    .before = quarters - 1,
    .complete = FALSE
  )
  
  return(variable)
}

# Estimate sorting variable
compustat_quarterly_cfv <- compustat_quarterly |> 
  mutate(ocf = (ibq + dpq + (wcapq - wcapq_lag)) / if_else(saleq < 0, NA_real_, saleq)) |> 
  select(gvkey, month, ocf) |> 
  group_by(gvkey) |> 
  arrange(month) |> 
  mutate(sv_cfv = roll_sd(pick(month, ocf), 
                          quarters = 16, 
                          min_obs = 8), 
         sv_cfv = if_else(month >= as.Date("1978-01-01"), sv_cfv, NA_real_)) |>  
  ungroup() |> 
  select(gvkey, month, sv_cfv) |> 
  drop_na()
  
# Remerge
compustat_quarterly <- compustat_quarterly |> 
  left_join(compustat_quarterly_cfv, by = c("gvkey", "month"))

# Free space
rm(compustat_quarterly_cfv)


# Standardized unexpected earnings ---------------------------------
# Function for rolling standard deviation 
roll_sd <- function(data, quarters, min_obs) {
  data <- bind_rows(data)
  
  variable <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "quarter",
    .f = ~ if_else(sum(!is.na(.x$ue)) < min_obs, 
                   NA_real_, 
                   sd(.x$ue, na.rm = TRUE)),
    .before = quarters - 1,
    .complete = FALSE
  )
  
  return(variable)
}

# Estimate sorting variable
compustat_quarterly_ue <- compustat_quarterly |> 
  mutate(ue = (epspxq / ajexq) - (epspxq_lag4 / ajexq_lag4),
         ue = if_else(is.nan(ue) | is.infinite(ue), NA_real_, ue)) |> 
  select(gvkey, month, ue) |> 
  group_by(gvkey) |> 
  arrange(month) |> 
  mutate(sd_ue = roll_sd(pick(month, ue), 
                         quarters = 8, 
                         min_obs = 6), 
         sv_sue = ue / sd_ue, 
         sv_sue = if_else(month >= as.Date("1972-01-01"), sv_sue, NA_real_)) |> 
  ungroup() |> 
  select(gvkey, month, sv_sue) |> 
  drop_na()

# Remerge
compustat_quarterly <- compustat_quarterly |> 
  left_join(compustat_quarterly_ue, by = c("gvkey", "month"))

# Free space
rm(compustat_quarterly_ue)


# Revenue surprises ------------------------------------------------
# Function for rolling standard deviation 
roll_sd <- function(data, quarters, min_obs) {
  data <- bind_rows(data)
  
  variable <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "quarter",
    .f = ~ if_else(sum(!is.na(.x$rs)) < min_obs, 
                   NA_real_, 
                   sd(.x$rs, na.rm = TRUE)),
    .before = quarters - 1,
    .complete = FALSE
  )
  
  return(variable)
}

# Estimate sorting variable
compustat_quarterly_rs <- compustat_quarterly |> 
  mutate(rs = (saleq / (ajexq * cshprq)) - (saleq_lag4 / (ajexq_lag4 * cshprq_lag4)),
         rs = if_else(is.nan(rs) | is.infinite(rs), NA_real_, rs))|> 
  select(gvkey, month, rs) |> 
  group_by(gvkey) |> 
  arrange(month) |> 
  mutate(sd_rs = roll_sd(pick(month, rs), 
                         quarters = 8, 
                         min_obs = 6), 
         sv_rs = rs / sd_rs, 
         sv_rs = if_else(month >= as.Date("1972-01-01"), sv_rs, NA_real_)) |> 
  ungroup() |> 
  select(gvkey, month, sv_rs) |> 
  drop_na()

# Remerge
compustat_quarterly <- compustat_quarterly |> 
  left_join(compustat_quarterly_rs, by = c("gvkey", "month"))

# Free space
rm(compustat_quarterly_rs)


# Tax expense suprises ---------------------------------------------
# Compute sorting variable
compustat_quarterly_tes <- compustat_quarterly |> 
  mutate(te = txtq / (cshprq * ajexq), 
         te_lag4 = txtq_lag4 / (cshprq_lag4 * ajexq_lag4),
         scale = atq_lag4 / (cshprq_lag4 * ajexq_lag4),
         sv_tes = (te - te_lag4) / scale, 
         sv_tes = if_else(txtq == 0, NA_real_, sv_tes), 
         sv_tes = if_else(month >= as.Date("1976-01-01"), sv_tes, NA_real_)) |> 
  select(gvkey, month, sv_tes) |>
  drop_na()

# Remerge
compustat_quarterly <- compustat_quarterly |> 
  left_join(compustat_quarterly_tes, by = c("gvkey", "month"))

# Free space
rm(compustat_quarterly_tes)


# Compute return on assets and return on equity --------------------
# Compute varibales
compustat_quarterly <- compustat_quarterly |> 
 mutate(sv_roa = ibq / atq_lag,
        sv_roa = if_else(month >= as.Date("1972-01-01"), sv_roa, NA_real_),
        sv_roe = ibq / beq_lag,
        sv_roe = if_else(month >= as.Date("1972-01-01"), sv_roe, NA_real_)) |> 
  select(gvkey, month, datadate, starts_with("filter_"), starts_with("sv_")) 


# Filters ----------------------------------------------------------
# Remove Inf and NaN
sorting_variables_comp_q <- compustat_quarterly |> 
  mutate(across(starts_with("sv_"), ~ na_if(., Inf)),
         across(starts_with("sv_"), ~ na_if(., -Inf)),
         across(starts_with("sv_"), ~ na_if(., NaN)))


# Store variables --------------------------------------------------
# Store
sorting_variables_comp_q  |> 
 dbWriteTable(conn = data_nse, 
              name = "sorting_variables_comp_q", 
              value = _, 
              overwrite = TRUE)