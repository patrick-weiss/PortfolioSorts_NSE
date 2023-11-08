# Decision nodes
# We specify all decision nodes and their respective levels here
# Additionally, we map decision nodes to names appearing in the paper

# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Data on SVs ------------------------------------------------------
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


# Specification grid -----------------------------------------------
# Create grid
setup_grid <- expand_grid(n_portfolios_main = c(5, 10),
                          n_portfolios_secondary = c(2, 5),
                          exchanges = c("NYSE", "NYSE|NASDAQ|AMEX"),
                          value_weighted = c(TRUE, FALSE),
                          sorting_method = c("single", "dbl_ind", "dbl_dep"),
                          formation_time = c("monthly", "FF"),
                          include_financials = c(TRUE, FALSE),
                          include_utilities = c(TRUE, FALSE),
                          drop_smallNYSE_at = c(0, 0.1, 0.2),
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


# MHT grid ---------------------------------------------------------
# Load grid
load("Data/data_grid.RData")

# Concentrate choices
setup_grid_MHT <- setup_grid |> 
  filter(drop_stock_age_at == 2) |> # Due to some SV being naturally restricted
  filter(drop_price_at == 0) |> # Most common choice
  filter(drop_bookequity == FALSE) # Most common choice

# Save MHT grid
save(list = c("setup_grid_MHT"), file = "Data/data_grid_MHT.RData")


# Decision nodes mapping -------------------------------------------
# Create nodes and names (the node order based on mads is now hard-coded for flexibility)
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
                                      "Size exclusion",
                                      "Price exclusion",
                                      "Stock-age exclusion",
                                      "BP: Quantiles (second)",
                                      "Negative earnings",
                                      "Negative book equity",
                                      "Sorting variable lag",
                                      "BP: Exchanges"),
                        node_order = c(10,
                                      14,
                                      11,
                                      9,
                                      2,
                                      3,
                                      1,
                                      7,
                                      6,
                                      12,
                                      5,
                                      4,
                                      8,
                                      13),
                        node_order_mad = c(1,
                                       2,
                                       8,
                                       10,
                                       7,
                                       11,
                                       4,
                                       13,
                                       12,
                                       9,
                                       3,
                                       14,
                                       5,
                                       6)) |> 
  arrange(node_order_mad)


# Save results -----------------------------------------------------
mapping_nodes |> 
  dbWriteTable(conn = data_nse, 
               name = "mapping_nodes", 
               value = _, 
               overwrite = TRUE)


# Path probabilities -----------------------------------------------
# Define general 56 papers
probabilities_56 <- tibble(node = "n_portfolios_main", choice = "5", probability = 0.485) |> 
  add_row(node = "n_portfolios_main", choice = "10", probability = 0.515) |> 
  add_row(node = "n_portfolios_secondary", choice = "2", probability = 0.556) |> 
  add_row(node = "n_portfolios_secondary", choice = "5", probability = 0.444) |> 
  add_row(node = "n_portfolios_secondary", choice = "NA", probability = 1) |> # for single sorts
  add_row(node = "exchanges", choice = "NYSE", probability = 0.307) |> 
  add_row(node = "exchanges", choice = "NYSE|NASDAQ|AMEX", probability = 0.693) |> 
  add_row(node = "value_weighted", choice = "TRUE", probability = 0.434) |> 
  add_row(node = "value_weighted", choice = "FALSE", probability = 0.566) |> 
  add_row(node = "sorting_method", choice = "single", probability = 0.58) |> 
  add_row(node = "sorting_method", choice = "dbl_ind", probability = 0.258) |> 
  add_row(node = "sorting_method", choice = "dbl_dep", probability = 0.162) |> 
  add_row(node = "formation_time", choice = "monthly", probability = 0.209) |> 
  add_row(node = "formation_time", choice = "FF", probability = 0.791) |> # does not exist for some SVs, but gets standardized away below
  add_row(node = "include_financials", choice = "TRUE", probability = 0.552) |> 
  add_row(node = "include_financials", choice = "FALSE", probability = 0.448) |> 
  add_row(node = "include_utilities", choice = "TRUE", probability = 0.833) |> 
  add_row(node = "include_utilities", choice = "FALSE", probability = 0.167) |> 
  add_row(node = "drop_smallNYSE_at", choice = "0", probability = 0.662) |> 
  add_row(node = "drop_smallNYSE_at", choice = "0.1", probability = 0.014) |> 
  add_row(node = "drop_smallNYSE_at", choice = "0.2", probability = 0.324) |> 
  add_row(node = "drop_price_at", choice = "0", probability = 0.862) |> 
  add_row(node = "drop_price_at", choice = "1", probability = 0.05) |> 
  add_row(node = "drop_price_at", choice = "5", probability = 0.088) |> 
  add_row(node = "drop_stock_age_at", choice = "0", probability = 0.328) |> 
  add_row(node = "drop_stock_age_at", choice = "2", probability = 0.672) |> 
  add_row(node = "drop_earnings", choice = "TRUE", probability = 0.167) |> 
  add_row(node = "drop_earnings", choice = "FALSE", probability = 0.833) |> 
  add_row(node = "drop_bookequity", choice = "TRUE", probability = 0.276) |> 
  add_row(node = "drop_bookequity", choice = "FALSE", probability = 0.724)

# Define general 109 papers
probabilities_109 <- tibble(node = "n_portfolios_main", choice = "5", probability = 0.511) |> 
  add_row(node = "n_portfolios_main", choice = "10", probability = 0.489) |> 
  add_row(node = "n_portfolios_secondary", choice = "2", probability = 0.526) |> 
  add_row(node = "n_portfolios_secondary", choice = "5", probability = 0.474) |> 
  add_row(node = "n_portfolios_secondary", choice = "NA", probability = 1) |> # for single sorts
  add_row(node = "exchanges", choice = "NYSE", probability = 0.266) |> 
  add_row(node = "exchanges", choice = "NYSE|NASDAQ|AMEX", probability = 0.734) |> 
  add_row(node = "value_weighted", choice = "TRUE", probability = 0.455) |> 
  add_row(node = "value_weighted", choice = "FALSE", probability = 0.545) |> 
  add_row(node = "sorting_method", choice = "single", probability = 0.576) |> 
  add_row(node = "sorting_method", choice = "dbl_ind", probability = 0.246) |> 
  add_row(node = "sorting_method", choice = "dbl_dep", probability = 0.178) |> 
  add_row(node = "formation_time", choice = "monthly", probability = 0.219) |> 
  add_row(node = "formation_time", choice = "FF", probability = 0.781) |> # does not exist for some SVs, but gets standardized away below
  add_row(node = "include_financials", choice = "TRUE", probability = 0.632) |> 
  add_row(node = "include_financials", choice = "FALSE", probability = 0.368) |> 
  add_row(node = "include_utilities", choice = "TRUE", probability = 0.815) |> 
  add_row(node = "include_utilities", choice = "FALSE", probability = 0.185) |> 
  add_row(node = "drop_smallNYSE_at", choice = "0", probability = 0.615) |> 
  add_row(node = "drop_smallNYSE_at", choice = "0.1", probability = 0.036) |> 
  add_row(node = "drop_smallNYSE_at", choice = "0.2", probability = 0.349) |> 
  add_row(node = "drop_price_at", choice = "0", probability = 0.793) |> 
  add_row(node = "drop_price_at", choice = "1", probability = 0.061) |> 
  add_row(node = "drop_price_at", choice = "5", probability = 0.146) |> 
  add_row(node = "drop_stock_age_at", choice = "0", probability = 0.43) |> 
  add_row(node = "drop_stock_age_at", choice = "2", probability = 0.57) |> 
  add_row(node = "drop_earnings", choice = "TRUE", probability = 0.182) |> 
  add_row(node = "drop_earnings", choice = "FALSE", probability = 0.818) |> 
  add_row(node = "drop_bookequity", choice = "TRUE", probability = 0.417) |> 
  add_row(node = "drop_bookequity", choice = "FALSE", probability = 0.583)

# Conditional probabilities
## sv_lag 56 papers
conditional_probabilities_sv_lag_56 <- function(data) {
  # Which lags are available
  lags <- data |> 
    pull(sv_lag) |> 
    unique() |> 
    sort()
  
  # Quarterly
  if(length(lags) == 2) {
    # 3m and 6m
    if(all(lags == c("3m", "6m"))) {
      conditional_probabilities <- tibble(node = "sv_lag", choice = "3m", probability = 0.667) |> 
        add_row(node = "sv_lag", choice = "6m", probability = 0.333)
      
      return(conditional_probabilities)
    }
  }
  
  # Monthly
  if(length(lags) == 3) {
    # 1m, 3m, and 6m
    if(all(lags == c("1m", "3m", "6m"))) {
      conditional_probabilities <- tibble(node = "sv_lag", choice = "1m", probability = 0.75) |> 
        add_row(node = "sv_lag", choice = "3m", probability = 0.188) |> 
        add_row(node = "sv_lag", choice = "6m", probability = 0.062)
      
      return(conditional_probabilities)
    }
    
    # Yearly
    # 3m, 6m, and FF
    if(all(lags == c("3m", "6m", "FF"))) {
      conditional_probabilities <- tibble(node = "sv_lag", choice = "3m", probability = 0.487) |> 
        add_row(node = "sv_lag", choice = "6m", probability = 0.128) |> 
        add_row(node = "sv_lag", choice = "FF", probability = 0.385)
      
      return(conditional_probabilities)
    }
  }
}

## sv_lag 109 papers
conditional_probabilities_sv_lag_109 <- function(data) {
  # Which lags are available
  lags <- data |> 
    pull(sv_lag) |> 
    unique() |> 
    sort()
  
  # Quarterly
  if(length(lags) == 2) {
    # 3m and 6m
    if(all(lags == c("3m", "6m"))) {
      conditional_probabilities <- tibble(node = "sv_lag", choice = "3m", probability = 0.667) |> 
        add_row(node = "sv_lag", choice = "6m", probability = 0.333)
      
      return(conditional_probabilities)
    }
  }
  
  # Monthly
  if(length(lags) == 3) {
    # 1m, 3m, and 6m
    if(all(lags == c("1m", "3m", "6m"))) {
      conditional_probabilities <- tibble(node = "sv_lag", choice = "1m", probability = 0.666) |> 
        add_row(node = "sv_lag", choice = "3m", probability = 0.188) |> 
        add_row(node = "sv_lag", choice = "6m", probability = 0.146)
      
      return(conditional_probabilities)
    }
    
    # Yearly
    # 3m, 6m, and FF
    if(all(lags == c("3m", "6m", "FF"))) {
      conditional_probabilities <- tibble(node = "sv_lag", choice = "3m", probability = 0.456) |> 
        add_row(node = "sv_lag", choice = "6m", probability = 0.152) |> 
        add_row(node = "sv_lag", choice = "FF", probability = 0.392)
      
      return(conditional_probabilities)
    }
  }
}

# Path probabilities ----------------------------------------------------
## Function: Compute sorting-variable-specific path probabilities
probabilities_per_sorting_variable <- function(data) {
  # Define sv_lag probabilities and add baseline probabilities
  ## 56 papers
  sv_probabilities_56 <- conditional_probabilities_sv_lag_56(data = data) |> 
    add_row(probabilities_56) |> 
    rename(probability_56 = probability)

  ## 109 papers
  sv_probabilities_109 <- conditional_probabilities_sv_lag_109(data = data) |> 
    add_row(probabilities_109) |> 
    rename(probability_109 = probability)

  ## Combine
  sv_probabilities <- sv_probabilities_56 |> 
    full_join(sv_probabilities_109, by = c("node", "choice"))
  
  # Add probabilities to data
  data_with_probabilities <- data |>
    mutate(across(.cols = -ID, ~as.character(.))) |> 
    pivot_longer(cols = -ID, names_to = "node", values_to = "choice") |> 
    mutate(choice = if_else(is.na(choice), "NA", choice)) |> 
    left_join(sv_probabilities, by = join_by(node, choice))
  
  # Safety check
  stopifnot("Only non-NA probabilities supported" = !any(is.na(data_with_probabilities$probability_56)))
  stopifnot("Only non-NA probabilities supported" = !any(is.na(data_with_probabilities$probability_109)))
  
  # Drop stock_age_filter for variables requiring stock history
  filter_variables <- c("sv_rmom", "sv_rev", "sv_csi", "sv_cfv", 
                        "sv_eprd", "sv_beta", "sv_bfp")
  
  IDs_to_remove <- setup_grid |> 
    filter((sorting_variable %in% filter_variables & drop_stock_age_at == 2)) |> 
    pull(ID)
  
  data_with_probabilities <- data_with_probabilities |> 
    filter(!(ID %in% IDs_to_remove))
  
  # ID-wise probabilities
  data_with_probabilities |> 
    group_by(ID) |> 
    summarise(probability_56 = prod(probability_56),
              probability_109 = prod(probability_109),
              .groups = 'drop') |> 
    mutate(probability_56 = probability_56 / sum(probability_56),
           probability_109 = probability_109 / sum(probability_109))
}

## Probability grid
probability_grid <- setup_grid |> 
  nest(.by = sorting_variable) |> 
  mutate(probabilities = map(data, ~probabilities_per_sorting_variable(.))) |> 
  select(-data) |> 
  unnest(probabilities)


# Store -----------------------------------------------------------------
probability_grid |> 
  dbWriteTable(conn = data_nse, 
               name = "probability_grid", 
               value = _, 
               overwrite = TRUE)

dbDisconnect(data_nse)