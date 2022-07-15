# Compute portfolio sorts
## The cluster prep avoids using an sqlite database on the cluster
## Instead, we save a Rdata file
## Functions and computations are in 21_portfolio_sorts_cluster.R

# Setup -------------------------------------------------------------------

# Packages 
library(tidyverse)
library(RSQLite)
library(DBI)
library(lubridate)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Data --------------------------------------------------------------------
# CRSP
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")

# Fama-French 3
factors_ff_monthly <- dbReadTable(data_nse, "factors_ff_monthly")

# Sorting variables compustat annual
sorting_variables_COMP <- dbReadTable(data_nse, "sorting_variables_comp_y")

# Sorting variables CRSP
sorting_variables_CRSP <- dbReadTable(data_nse, "sorting_variables_CRSP")

# Drop unused columns
crsp_monthly <- crsp_monthly %>%
  select(-ret, -shrout, -altprc, -exchcd, -siccd)

# Match datasets
## Fama & French
data_FF <- crsp_monthly %>% 
  mutate(sorting_date = month) %>%
  left_join(sorting_variables_COMP %>%
              mutate(sorting_date = floor_date(datadate, "year") %m+% months(18),
                     sv_date_COMP = sorting_date) %>%
              select(-month), 
            by = c("gvkey", "sorting_date")) %>%
  arrange(permno, month) %>%
  group_by(permno, gvkey) %>%
  fill(starts_with("sv_"), starts_with("filter_")) %>%
  ungroup() %>%
  filter(sv_date_COMP > month %m-% months(12)) %>% # filter to ensure data is not too old
  select(-sv_date_COMP, -sorting_date, -datadate, -gvkey) %>%
  left_join(sorting_variables_CRSP %>%
              select(permno, month, starts_with("filter_")),
            by = c("permno", "month"))

## Check
if(data_FF %>% distinct() %>% nrow() != nrow(data_FF)) stop("Data error!")


## 1m lag for CRSP variables
data_1m <- crsp_monthly %>% 
  mutate(sorting_date = month) %>%
  left_join(sorting_variables_COMP %>%
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(3), # TODO for filters 3m lag?!
                     sv_date_COMP = sorting_date) %>%
              select(gvkey, sorting_date, sv_date_COMP, starts_with("filter_")), 
            by = c("gvkey", "sorting_date")) %>%
  left_join(sorting_variables_CRSP %>%
              mutate(sorting_date = month %m+% months(1),
                     sv_date_CRSP = sorting_date) %>%
              select(-month), 
            by = c("permno", "sorting_date")) %>%
  arrange(permno, month) %>%
  group_by(permno, gvkey) %>%
  fill(starts_with("sv_"), starts_with("filter_")) %>%
  ungroup() %>%
  filter(sv_date_COMP > month %m-% months(12),
         sv_date_CRSP > month %m-% months(12)) %>% # filter to ensure data is not too old
  select(-sv_date_COMP, -sv_date_CRSP, -sorting_date, -gvkey)

## Check
if(data_1m %>% distinct() %>% nrow() != nrow(data_1m)) stop("Data error!")


## 3m lag
data_3m <- crsp_monthly %>% 
  mutate(sorting_date = month) %>%
  left_join(sorting_variables_COMP %>%
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(3),
                     sv_date_COMP = sorting_date) %>%
              select(-month), 
            by = c("gvkey", "sorting_date")) %>%
  arrange(permno, month) %>%
  group_by(permno, gvkey) %>%
  fill(starts_with("sv_"), starts_with("filter_")) %>%
  ungroup() %>%
  filter(sv_date_COMP > month %m-% months(12)) %>% # filter to ensure data is not too old
  select(-sv_date_COMP, -sorting_date, -datadate, -gvkey) %>%
  left_join(sorting_variables_CRSP %>%
              select(permno, month, starts_with("filter_")),
            by = c("permno", "month"))


## Check
if(data_3m %>% distinct() %>% nrow() != nrow(data_3m)) stop("Data error!")


## 6m lag
data_6m <-crsp_monthly %>% 
  mutate(sorting_date = month) %>%
  left_join(sorting_variables_COMP %>%
              mutate(sorting_date = floor_date(datadate, "month") %m+% months(3),
                     sv_date_COMP = sorting_date) %>%
              select(-month), 
            by = c("gvkey", "sorting_date")) %>%
  arrange(permno, month) %>%
  group_by(permno, gvkey) %>%
  fill(starts_with("sv_"), starts_with("filter_")) %>%
  ungroup() %>%
  filter(sv_date_COMP > month %m-% months(12)) %>% # filter to ensure data is not too old
  select(-sv_date_COMP, -sorting_date, -datadate, -gvkey) %>%
  left_join(sorting_variables_CRSP %>%
              select(permno, month, starts_with("filter_")),
            by = c("permno", "month"))


## Check
if(data_6m %>% distinct() %>% nrow() != nrow(data_6m)) stop("Data error!")

# Save data
save(list = c("data_FF", "data_1m", "data_3m", "data_6m", "factors_ff_monthly"), file = "Data/data_sorts.RData")
