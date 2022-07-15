# Final result prep
## Collect results, cleaning, merge variables, and scale variables


# Packages ----------------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)


# Data files --------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Data premium results 
load("Data/data_grid_results.Rdata")

# Data TS results 
data_TS_results <- dbReadTable(data_nse, "data_TS_results")

# Direction 
direction_hml_portfolio <- dbReadTable(data_nse, "direction_hml_portfolio") 

# SV Groups 
sv_groups <- dbReadTable(data_nse, "sv_groups")

# Node Text 
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")


# Cleaning ----------------------------------------------------------------
# Premiums results
data_result <- data_result %>% 
  filter(sorting_variable != "sv_be") %>% # TODO Remove
  filter(!(sorting_variable == "sv_csi" & drop_stock_age_at == 2)) %>%          # CSI with stock-age filter is constant
  filter(!(sorting_variable == "sv_size" & sorting_method != "single")) %>%     # Size only single
  filter(!(sorting_variable == "sv_size" & !is.na(n_portfolios_secondary))) %>%
  filter(!(sorting_variable == "sv_e_11"))

# Direction
direction_hml_portfolio <- direction_hml_portfolio %>%
  select(-identifier_direction) %>%
  pivot_longer(everything(), names_to = "sv", values_to = "direction") %>%
  mutate(sv = substr(sv, 1, nchar(sv) - 2)) 

# TS results 
data_TS_results <- data_TS_results %>% 
  filter(sorting_variable != "sv_be") %>% # TODO Remove
  filter(!(sorting_variable == "sv_csi" & node == "drop_stock_age_at")) %>%
  filter(!(sorting_variable == "sv_size" & node == "sorting_method")) %>%
  filter(!(sorting_variable == "sv_size" & node == "n_portfolios_secondary")) %>%
  filter(!(sorting_variable == "sv_e_11"))


# Merging -----------------------------------------------------------------
# Premiums results
data_result <- data_result %>%
  left_join(direction_hml_portfolio, by = c("sorting_variable" = "sv")) %>%
  left_join(sv_groups, by = c("sorting_variable" = "sv"))

# TS results
data_TS_results <- data_TS_results %>%
  left_join(sv_groups, by = c("sorting_variable" = "sv")) %>%
  left_join(mapping_nodes, by = "node")


# Rescaling ---------------------------------------------------------------
# Premiums results
## Rescale
data_result <- data_result  %>%
  mutate(mean = mean * direction * 100,
         se = se * 100,
         t = t * direction,
         alpha_CAPM = alpha_CAPM * direction * 100,
         se_CAPM = se_CAPM * 100,
         t_CAPM = t_CAPM * direction,
         alpha_FF3 = alpha_FF3 * direction * 100,
         se_FF3 = se_FF3 * 100,
         t_FF3 = t_FF3* direction) %>%
  select(-direction) 

## Reorder
data_result <- data_result %>% 
  arrange(group, sorting_variable) %>%
  mutate(SV = str_to_upper(substr(sorting_variable, 4, nchar(sorting_variable))),
         SV = str_replace(SV, "_", ""),
         SV_order = as.numeric(factor(SV, levels = unique(SV), ordered = TRUE)))

## Rename nodes
data_result <- data_result %>%
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
data_TS_results <- data_TS_results %>%
  mutate(mad = mad * 100,
         mad_C = mad_C * 100,
         mad_F = mad_F * 100)


# Computations ------------------------------------------------------------
# TS summary statistics
data_TS_all <- data_TS_results %>%
  group_by(sorting_variable, node) %>%
  summarise(group = unique(group),
            node_name = unique(node_name),
            cor = mean(cor, na.rm = T),
            mad = mean(mad, na.rm = T),
            cor_C = mean(cor_C, na.rm = T),
            mad_C = mean(mad_C, na.rm = T),
            cor_F = mean(cor_F, na.rm = T),
            mad_F = mean(mad_F, na.rm = T),
            .groups = 'drop')

## Mad overall (for ordering nodes)
data_TS_mad_overall <- data_TS_all %>%
  group_by(node_name) %>%
  summarise(Overall = mean(mad, na.rm = T),
            node = unique(node),
            .groups = 'drop') %>%
  arrange(desc(Overall))

# Demean data
data_result_demeaned <- data_result %>%
  group_by(sorting_variable) %>%
  mutate(mean = mean - mean(mean, na.rm = T)) %>%
  ungroup()

# Reorder mapping
mapping_nodes <- mapping_nodes[match(data_TS_mad_overall$node_name, mapping_nodes$node_name),]

# Save results ------------------------------------------------------------
# Premium results
data_result %>%
  dbWriteTable(data_nse, "data_premium_results", ., overwrite = TRUE)

# Premium results (demeaned)
data_result_demeaned %>%
  dbWriteTable(data_nse, "data_premium_results_demeaned", ., overwrite = TRUE)

# TS results all
data_TS_all %>%
  dbWriteTable(data_nse, "data_TS_all", ., overwrite = TRUE)

# Mapping nodes
mapping_nodes %>%
  dbWriteTable(data_nse, "mapping_nodes", ., overwrite = TRUE)
