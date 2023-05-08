# Final result prep
## Collect results, cleaning, merge variables, and scale variables


# Packages ----------------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)
library(kSamples)


# Data files --------------------------------------------------------------
# Access regular database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Access TS database
data_nse_TS <- dbConnect(SQLite(), 
                      "Data/data_nse_TS.sqlite", 
                      extended_types = TRUE)

# Data premium results 
load("Data/data_grid_results.Rdata")

# Data TS results 
data_TS_results <- dbReadTable(data_nse_TS, "data_TS_results")

# Data TS time series
data_TS_timeseries <- dbReadTable(data_nse_TS, "data_MAD_TS_results")

# Direction 
direction_hml_portfolio <- dbReadTable(data_nse, "direction_hml_portfolio") 

# SV Groups 
sv_groups <- dbReadTable(data_nse, "sv_groups")

# Node Text 
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")


# Functions -------------------------------------------------------------
# Compute AD test and output test statistic
ad_test <- function(data, node) {
  # Feedback
  cat(".")
  
  # Premiums
  ad_premiums <- data |>  
    pull(mean)
  
  # Levels
  ad_levels <- data |> 
    pull({{ node }}) |> 
    as_factor()
  
  # Escape low levels
  if(length(unique(ad_levels)) < 2 | sum(!is.na(ad_premiums)) < 100) {
    return(tibble(ad = NA_real_,
                  ad_t = NA_real_))
  }
  
  # Compute AD test
  ad_results <- ad.test(ad_premiums ~ ad_levels, 
                        method = "asymptotic", 
                        dist = FALSE)
  
  # Return tibble
  tibble(ad = ad_results$ad[1, 1],
         ad_t = ad_results$ad[1, 2])
}


# Cleaning ----------------------------------------------------------------
# Some variables require a history of stock returns, which mutes the drop_stock_age filter
filter_variables <- c("sv_rmom", "sv_rev", "sv_csi", "sv_cfv", 
                      "sv_eprd", "sv_beta", "sv_bfp")

# Premiums results
data_result <- data_result |> 
  filter(!(sorting_variable %in% filter_variables & drop_stock_age_at == 2))            

# TS results 
data_TS_results <- data_TS_results |> 
  filter(!(sorting_variable %in% filter_variables & node == "drop_stock_age_at"))

# TS timeseries 
data_TS_timeseries <- data_TS_timeseries |> 
  filter(!(sorting_variable %in% filter_variables & node == "drop_stock_age_at")) |> 
  filter(obs_R > 100)


# Merging -----------------------------------------------------------------
# Premiums results
data_result <- data_result |> 
  left_join(direction_hml_portfolio, by = join_by("sorting_variable" == "sv")) |> 
  left_join(sv_groups, by = join_by("sorting_variable" == "sv"))

# TS results
data_TS_results <- data_TS_results |> 
  left_join(sv_groups, by = join_by("sorting_variable" == "sv")) |> 
  left_join(mapping_nodes, by = "node")

# TS timeseries
data_TS_timeseries <- data_TS_timeseries |> 
  left_join(sv_groups, by = join_by("sorting_variable" == "sv")) |> 
  left_join(mapping_nodes, by = "node")


# Rescaling ---------------------------------------------------------------
# Premiums results
## Rescale
data_result <- data_result |> 
  mutate(mean = mean * direction * 100,
         skew = skew * direction,
         across(starts_with("se"), ~ .x * 100),
         across(starts_with("alpha"), ~ .x * 100 * direction),
         across(starts_with("t"), ~ .x * direction)) |> 
  select(-direction)

## Reorder
data_result <- data_result |> 
  arrange(group, sorting_variable) |> 
  mutate(SV = str_to_upper(substr(sorting_variable, 4, nchar(sorting_variable))),
         SV = str_replace(SV, "_", ""),
         SV_order = as.numeric(factor(SV, levels = unique(SV), ordered = TRUE)))

## Rename nodes
data_result <- data_result |> 
  mutate(value_weighted = case_when(value_weighted == FALSE ~ "EW",
                                    value_weighted == TRUE ~ "VW"),
         sorting_method = case_when(sorting_method == "single" ~ "Single",
                                    sorting_method == "dbl_ind" ~ "Independent",
                                    sorting_method == "dbl_dep" ~ "Dependent"),
         include_financials = case_when(include_financials == TRUE ~ "Included",
                                        include_financials == FALSE ~ "Excluded"),
         include_utilities = case_when(include_utilities == TRUE ~ "Included",
                                       include_utilities == FALSE ~ "Excluded"),
         drop_earnings = case_when(drop_earnings == TRUE ~ "Yes",
                                   drop_earnings == FALSE ~ "No"),
         drop_bookequity = case_when(drop_bookequity == TRUE ~ "Yes",
                                     drop_bookequity == FALSE ~ "No"),
         exchanges = case_when(exchanges == "NYSE" ~ "NYSE",
                               exchanges == "NYSE|NASDAQ|AMEX" ~ "All"))

# TS results
data_TS_results <- data_TS_results |> 
  mutate(across(starts_with("mad"), ~ .x * 100))

# TS timeseries
data_TS_timeseries <- data_TS_timeseries |> 
  mutate(across(starts_with("diff_avg"), ~ .x * 100))


# Computations ------------------------------------------------------------
# TS summary statistics
data_TS_results <- data_TS_results |> 
  group_by(sorting_variable, node) |> 
  summarise(group = unique(group),
            node_name = unique(node_name),
            across(starts_with("cor") | starts_with("mad"), ~ mean(.x, na.rm = TRUE)),
            .groups = 'drop')

# AD tests
## Initialize table
data_ad <- tibble(NULL)

## Loop through nodes
for(nodes in unique(data_TS_results$node)) {
  # Feedback
  cat(paste0("\n", nodes, "\n"))
  
  # Compute results
  data_ad_new <- data_result |> 
    group_by(sorting_variable) |> 
    summarise(node = nodes,
              ad_results = ad_test(data = pick("mean", all_of(nodes)), 
                                   node = nodes)) |> 
    unnest(ad_results)
  
  data_ad <- data_ad |> 
    bind_rows(data_ad_new)
  
  # Free memory
  rm(data_ad_new)
}

## Merge to data_TS_results
data_TS_results <- data_TS_results |> 
  left_join(data_ad, by = c("sorting_variable", "node"))

# Mad overall (for ordering nodes)
data_TS_mad_overall <- data_TS_results |> 
  group_by(node_name) |> 
  summarise(Overall = mean(mad, na.rm = T),
            node = unique(node),
            .groups = 'drop') |> 
  arrange(desc(Overall))

# Demean data
data_result_demeaned <- data_result |> 
  group_by(sorting_variable) |> 
  mutate(mean = mean - mean(mean, na.rm = T)) |> 
  ungroup()

# Aggregate TS time series
data_TS_timeseries <- data_TS_timeseries |> 
  group_by(month, node) |> 
  summarise(node_name = unique(node_name),
            mad_R = mean(diff_avg_R, na.rm = T),
            mad_C = mean(diff_avg_C, na.rm = T),
            mad_F = mean(diff_avg_F, na.rm = T),
            mad_Q = mean(diff_avg_Q, na.rm = T),
            .groups = 'drop')

# Reorder mapping
mapping_nodes <- mapping_nodes[match(data_TS_mad_overall$node_name, mapping_nodes$node_name),]


# Save results ------------------------------------------------------------
# Premium results
data_result |> 
  dbWriteTable(conn = data_nse,
               name = "data_premium_results", 
               value = _, 
               overwrite = TRUE)

# Premium results (demeaned)
data_result_demeaned |> 
  dbWriteTable(conn = data_nse,
               name = "data_premium_results_demeaned", 
               value = _, 
               overwrite = TRUE)

# TS results all
data_TS_results |> 
  dbWriteTable(conn = data_nse, 
               name = "data_TS_all", 
               value = _, 
               overwrite = TRUE)

# TS results all
data_TS_timeseries |> 
  dbWriteTable(conn = data_nse, 
               name = "data_TS_timeseries_all", 
               value = _, 
               overwrite = TRUE)

# Mapping nodes
mapping_nodes |> 
  dbWriteTable(conn = data_nse, 
               name = "mapping_nodes", 
               value = _, 
               overwrite = TRUE)

# Disconnect
dbDisconnect(data_nse)
dbDisconnect(data_nse_TS)