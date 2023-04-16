# Compute portfolio sorts
## The cluster prep avoids using an sqlite database on the cluster
## Instead, we save a Rdata files
## Functions and computations are in 21_portfolio_sorts_cluster.R

# Setup -------------------------------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Data --------------------------------------------------------------------
# CRSP
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")

# Fama french 5 factors
factors_ff_monthly <- dbReadTable(data_nse, "factors_ff_monthly")

# Q factors
factors_q_monthly <- dbReadTable(data_nse, "factors_q_monthly")

# Sorting variables compustat quarterly
sorting_variables_COMP_q <- dbReadTable(data_nse, "sorting_variables_comp_q")

# Sorting variables compustat annual
sorting_variables_COMP_y <- dbReadTable(data_nse, "sorting_variables_comp_y")

# Sorting variables CRSP
sorting_variables_CRSP <- dbReadTable(data_nse, "sorting_variables_CRSP")

# Drop unused columns from CRSP
crsp_monthly <- crsp_monthly |> 
  select(permno, gvkey, month, ret_excess, mktcap_lag, exchange, industry)

# Direction of the premium
direction_hml_portfolio <- dbReadTable(data_nse, "direction_hml_portfolio")


# Factor data -----------------------------------------------------------
# Merge factor data
factors_monthly <- factors_ff_monthly |> 
  left_join(factors_q_monthly, by = c("month"))

# Save for cluster
save(factors_monthly, file = "Data/data_factors.Rdata")


# Data with Fama & French  ----------------------------------------------
# Merge data
data_FF <- crsp_monthly |> 
  mutate(sorting_date = month) |> 
  left_join(sorting_variables_COMP_y |> 
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
  filter(sv_date_COMP_y > month %m-% months(12)) |> # filter to ensure data is not too old
  select(-sv_date_COMP_y, -sorting_date, -gvkey)

# Add CRSP filter
data_FF <- data_FF |> 
  left_join(sorting_variables_CRSP |>               
              mutate(month = month %m+% months(1)) |> 
              select(permno, month, starts_with("filter_")),
            by = c("permno", "month"))

# Check
if(data_FF |> distinct() |>  nrow() != nrow(data_FF)) stop("Data error!")


# Data with 1m lag ------------------------------------------------------
# Merge data
data_1m <- crsp_monthly |> 
  mutate(sorting_date = month) |> 
  left_join(sorting_variables_COMP_y |> 
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(3),
                     sv_date_COMP_y = sorting_date) |> 
              select(gvkey, sorting_date, sv_date_COMP_y, starts_with("filter_")), 
            by = c("gvkey", "sorting_date")) 

# Fill accounting variables and ensure timeliness of data
data_1m <- data_1m |> 
  arrange(permno, month) |> 
  group_by(permno, gvkey) |> 
  fill(starts_with("sv_"), starts_with("filter_")) |> 
  ungroup() |> 
  filter(sv_date_COMP_y > month %m-% months(12)) |> # filter to ensure data is not too old
  select(-sv_date_COMP_y, -sorting_date, -gvkey)

# Add CRSP variables and filter
data_1m <- data_1m |> 
  left_join(sorting_variables_CRSP |> 
              mutate(month = month %m+% months(1)) |> 
              select(permno, month, starts_with("sv_"), starts_with("filter_")),
            by = c("permno", "month"))

# Check
if(data_1m |> distinct() |> nrow() != nrow(data_1m)) stop("Data error!")


# Data with 3m lag ------------------------------------------------------
# Merge data
data_3m <- crsp_monthly |> 
  mutate(sorting_date = month) |> 
  left_join(sorting_variables_COMP_y |> 
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(3),
                     sv_date_COMP_y = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date")) |> 
  left_join(sorting_variables_COMP_q |> 
              mutate(sorting_date = month %m+% months(3),
                     sv_date_COMP_q = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date")) |>  
  left_join(sorting_variables_CRSP |> 
              mutate(sorting_date = month %m+% months(3),
                     sv_date_CRSP = sorting_date) |> 
              select(-month, -starts_with("filter_")), 
            by = c("permno", "sorting_date")) 


# Fill variables and ensure timeliness of data
data_3m <- data_3m |> 
  arrange(permno, month) |> 
  group_by(permno, gvkey) |>
  fill(starts_with("sv_"), starts_with("filter_")) |> 
  ungroup() |> 
  filter(sv_date_COMP_y > month %m-% months(12),
         sv_date_COMP_q > month %m-% months(12),
         sv_date_CRSP > month %m-% months(12)) |>  # filter to ensure data is not too old
  select(-sv_date_COMP_y, -sv_date_COMP_q, -sv_date_CRSP, -sorting_date, -gvkey) 

# Add CRSP filter
data_3m <- data_3m |>  
  left_join(sorting_variables_CRSP |> 
              mutate(month = month %m+% months(1)) |> 
              select(permno, month, starts_with("filter_")),
            by = c("permno", "month"))


# Check
if(data_3m |> distinct() |> nrow() != nrow(data_3m)) stop("Data error!")


# Data with 6m lag ------------------------------------------------------
## 6m lag
data_6m <- crsp_monthly|> 
  mutate(sorting_date = month) |> 
  left_join(sorting_variables_COMP_y |> 
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(6),
                     sv_date_COMP_y = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date")) |> 
  left_join(sorting_variables_COMP_q |> 
              mutate(sorting_date = month %m+% months(6),
                     sv_date_COMP_q = sorting_date) |> 
              select(-month, -datadate), 
            by = c("gvkey", "sorting_date")) |>  
  left_join(sorting_variables_CRSP |> 
              mutate(sorting_date = month %m+% months(6),
                     sv_date_CRSP = sorting_date) |> 
              select(-month, -starts_with("filter_")), 
            by = c("permno", "sorting_date")) 

# Fill variables and ensure timeliness of data
data_6m <- data_6m |> 
  arrange(permno, month) |> 
  group_by(permno, gvkey) |>
  fill(starts_with("sv_"), starts_with("filter_")) |> 
  ungroup() |> 
  filter(sv_date_COMP_y > month %m-% months(12),
         sv_date_COMP_q > month %m-% months(12),
         sv_date_CRSP > month %m-% months(12)) |>  # filter to ensure data is not too old
  select(-sv_date_COMP_y, -sv_date_COMP_q, -sv_date_CRSP, -sorting_date, -gvkey) 

# Add CRSP filter
data_6m <- data_6m |>  
  left_join(sorting_variables_CRSP |> 
              mutate(month = month %m+% months(1)) |> 
              select(permno, month, starts_with("filter_")),
            by = c("permno", "month"))

# Check
if(data_6m |> distinct() |> nrow() != nrow(data_6m)) stop("Data error!")


# Save final data -------------------------------------------------------
# Save data
save(list = c("data_FF", 
              "data_1m", 
              "data_3m", 
              "data_6m", 
              "factors_monthly",
              "direction_hml_portfolio"), 
     file = "Data/data_sorts.RData")