# Decision nodes
# We specify all decision nodes and their respective levels here
# Additionally, we map decision nodes to names appearing in the paper

# Packages -------------------------------------------------------------------
library(tidyverse)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Data on SVs ----------------------------------------------------------------
# Sorting variables compustat 
## Quarterly 
sv_comp_q <- dbReadTable(data_nse, "sorting_variables_comp_q") |> 
  select(starts_with("sv_")) |> 
  colnames()

## Yearly
sv_comp_y <- dbReadTable(data_nse, "sorting_variables_comp_y") |> 
  select(starts_with("sv_")) |> 
  colnames()

# Sorting variables CRSP
sv_CRSP <- dbReadTable(data_nse, "sorting_variables_CRSP") |> 
  select(starts_with("sv_")) |> 
  colnames()

# Combined
available_sorting_variables <- c(sv_CRSP, sv_comp_q, sv_comp_y)


# Specification grid ---------------------------------------------------------
# Create grid
setup_grid <- expand_grid(n_portfolios_main = c(5, 10),
                          n_portfolios_secondary = c(2, 5),
                          exchanges = c("NYSE", "NYSE|NASDAQ|AMEX"),
                          value_weighted = c(TRUE, FALSE),
                          sorting_method = c("single", "dbl_ind", "dbl_dep"),
                          formation_time = c("monthly", "FF"),
                          include_financials = c(TRUE, FALSE),
                          include_utilities = c(TRUE, FALSE),
                          drop_smallNYSE_at = c(0, 0.05, 0.2),
                          drop_price_at = c(0, 1, 5),
                          drop_stock_age_at = c(0, 2),
                          drop_earnings = c(TRUE, FALSE),
                          drop_bookequity = c(TRUE, FALSE),
                          sv_lag = c("1m", "3m", "6m", "FF"),
                          sorting_variable = available_sorting_variables)

## Remove information on double sorting for univariate sorts
setup_grid <- setup_grid |> 
  filter(!(sorting_method == "single" & n_portfolios_secondary > 2)) |> 
  mutate(n_portfolios_secondary = case_when(sorting_method == "single" ~ NA_real_, 
                                            TRUE ~ n_portfolios_secondary))

## Remove formation time for monthly and quarterly sorting variables
setup_grid <- setup_grid |> 
  mutate(sv_accounting = sorting_variable %in% sv_comp_y,
         formation_time = case_when(sv_accounting ~ formation_time,
                                    !sv_accounting ~ "monthly")) |> 
  select(-sv_accounting) |> 
  distinct()

## Remove unused lags
setup_grid <- setup_grid |>
  filter(!(sorting_variable %in% sv_CRSP & sv_lag == "FF")) |> # CRSP variables only get lags 1m, 3m, or 6m
  filter(!(sorting_variable %in% sv_comp_q & sv_lag %in% c("1m", "FF"))) |> # Quarterly data only gets lags 3m or 6m
  filter(!(sorting_variable %in% sv_comp_y & sv_lag == "1m")) # Annual variables only get 3m, 6m, or FF
  
## Resort and add index
setup_grid <- setup_grid |> 
  arrange(sv_lag, sorting_variable) |> 
  rowid_to_column(var = "ID")

# Save grid
save(list = c("setup_grid"), file = "Data/data_grid.RData")


# Decision nodes mapping -----------------------------------------------------
# Create nodes and names
mapping_nodes <- tibble(node = c("n_portfolios_main",
                              "value_weighted",
                              "sorting_method",
                              "formation_time",
                              "include_financials",
                              "include_utilities",
                              "drop_smallNYSE_at",
                              "drop_price_at",
                              "drop_stock_age_at",
                              "n_portfolios_secondary",
                              "drop_earnings",
                              "drop_bookequity",
                              "sv_lag",
                              "exchanges"),
                        node_name = c("BP: Quantiles (main)",
                                      "Weighting scheme",
                                      "Double sort",
                                      "Rebalancing",
                                      "Financials",
                                      "Utilities",
                                      "Size restriction",
                                      "Price restriction",
                                      "Stock-age restriction",
                                      "BP: Quantiles (second)",
                                      "Positive earnings",
                                      "Positive book equity",
                                      "Sorting variable lag",
                                      "BP: Exchanges"))



# Save results ---------------------------------------------------------------
mapping_nodes |> 
  dbWriteTable(conn = data_nse, 
               name = "mapping_nodes", 
               value = _, 
               overwrite = TRUE)
