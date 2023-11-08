# Difference between factor exposures and alphas from FF5
## This file is meant to be run in parallel on a cluster


# Setup ------------------------------------------------------------
# ID 
cluster_id = as.integer(Sys.getenv("SGE_TASK_ID"))

# Check if completed
# Existing files
files_ran <- list.files("Project_NSE/Factor_diff")
files_ran <- substr(files_ran, 11, nchar(files_ran)-4)
sets <- 952 # See line 38ff

# Check if file ran w/o problems before
if(as.character(cluster_id) %in% files_ran & cluster_id != sets) {
  # File already ran w/o problems
  cat(paste("ID", cluster_id, "not needed"))
  stop("Already completed")
}

# Packages 
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# Starting
cat(paste("Starting Esimation", cluster_id, " - ", Sys.time()), "\n")

# Fama French factors
load("Project_NSE/Data/data_factors.Rdata")

# Additional factors
load("Project_NSE/Data/data_factors_add.Rdata")

# Sorting data
load("Project_NSE/Data/data_grid.RData")

# Remove certain paths
setup_grid <- setup_grid |> 
  filter(drop_smallNYSE_at != 0.1) |> 
  filter(drop_price_at != 1) |> 
  group_by(sorting_variable) |> 
  mutate(sv_lag_max = max(unique(sv_lag)),
         sv_lag_min = min(unique(sv_lag))) |> 
  ungroup() |> 
  filter(sv_lag == sv_lag_max | sv_lag == sv_lag_min) |> 
  select(-sv_lag_max, -sv_lag_min) |> 
  filter(sorting_method != "single")
  
# Decision nodes & sorting variables
input_decision_nodes <- 14
input_sorting_variables <- setup_grid |> pull(sorting_variable) |> unique() |> length()
# input_decision_nodes * input_sorting_variables # required tasks

set.seed("20211021")


# ID set -----------------------------------------------------------
node <- (cluster_id - 1) %/% input_sorting_variables + 1
sv <- cluster_id %% input_sorting_variables + 1

# Text
node_text <- colnames(setup_grid)[1+node]
sv_text <- unique(setup_grid$sorting_variable)[sv]

# Node levels
node_levels <- setup_grid |> 
  filter(sorting_variable == sv_text) |> 
  pull({{ node_text }}) |> 
  unique()
  
# Readjust node levels
if(node_text == "drop_smallNYSE_at") node_levels <- sort(node_levels)
if(node_text == "include_financials") node_levels <- sort(node_levels, decreasing = TRUE)
if(node_text == "include_utilities") node_levels <- sort(node_levels, decreasing = TRUE)
if(node_text == "drop_bookequity") node_levels <- sort(node_levels)
if(node_text == "drop_earnings") node_levels <- sort(node_levels)
if(node_text == "drop_stock_age_at") node_levels <- sort(node_levels)
if(node_text == "drop_price_at") node_levels <- sort(node_levels)
if(node_text == "sv_lag") node_levels <- sort(node_levels)
if(node_text == "formation_time") node_levels <- sort(node_levels, decreasing = TRUE)
if(node_text == "n_portfolios_main") node_levels <- sort(node_levels)
if(node_text == "sorting_method") node_levels <- sort(node_levels)
if(node_text == "n_portfolios_secondary") node_levels <- sort(node_levels)
if(node_text == "exchanges") node_levels <- sort(node_levels)
if(node_text == "value_weighted") node_levels <- sort(node_levels)

# Progress reports
cat(paste(node_text, "for", sv_text, "\n"))

if(length(node_levels) != 2) {
  print(node_levels)
  print("There is an issue with the node levels!")
  cat(paste(rep(".\n", 100), collapse = ""))
  stop()
}

# Main variables
main_grid <- setup_grid |> 
  filter(.data[[node_text]] == node_levels[1],
         sorting_variable == sv_text)

# Only one level
if(length(node_levels) == 1) {
  cat("---> Only one level!\n")
  
  # Main grid
  main_grid <- main_grid |> 
    slice(1) |> 
    mutate(ID = NA) |> 
    select(starts_with("ID")) |> 
    mutate(node = node_text,
           sorting_variable = sv_text,
           alpha = NA_real_,
           alpha_se = NA_real_,
           beta_mkt = NA_real_,
           beta_mkt_se = NA_real_,
           beta_smb = NA_real_,
           beta_smb_se = NA_real_,
           beta_hml = NA_real_,
           beta_hml_se = NA_real_,
           beta_rmw = NA_real_,
           beta_rmw_se = NA_real_,
           beta_cma = NA_real_,
           beta_cma_se = NA_real_,
           beta_mom = NA_real_,
           beta_mom_se = NA_real_,
           beta_liq = NA_real_,
           beta_liq_se = NA_real_,
           r_squared = NA_real_,
           residual_variance = NA_real_)
  
  # Save & stop
  saveRDS(main_grid, file = paste0("Project_NSE/Factor_diff/cluster_ID", cluster_id, ".rds"))
  stop("DONE")
}

# Second level
# Add new id
further_grid <- main_grid |> 
  mutate({{ node_text }} := node_levels[2]) |> 
  select(-starts_with("ID")) |> 
  left_join(setup_grid) |> 
  rename(ID_2 := ID)

# Merge back
main_grid <- main_grid |> 
  bind_cols(further_grid |> select(starts_with("ID_")))

rm(further_grid)


# Select relevant information from grid
main_grid <- main_grid |> 
  rename(ID_1 = ID) |> 
  select(starts_with("ID_")) |> 
  mutate(node = node_text,
         sorting_variable = sv_text,
         alpha = NA_real_,
         alpha_se = NA_real_,
         beta_mkt = NA_real_,
         beta_mkt_se = NA_real_,
         beta_smb = NA_real_,
         beta_smb_se = NA_real_,
         beta_hml = NA_real_,
         beta_hml_se = NA_real_,
         beta_rmw = NA_real_,
         beta_rmw_se = NA_real_,
         beta_cma = NA_real_,
         beta_cma_se = NA_real_,
         beta_mom = NA_real_,
         beta_mom_se = NA_real_,
         beta_liq = NA_real_,
         beta_liq_se = NA_real_,
         r_squared = NA_real_,
         residual_variance = NA_real_)


# Compute statistics -----------------------------------------------
for(setup_row in 1:nrow(main_grid)) {
  # Take IDs
  IDs <- main_grid |> 
    slice(setup_row) |> 
    select(starts_with("ID_")) |> 
    unlist()
  
  # First ID
  data_resultset <- readRDS(paste0("Project_NSE/Timeseries/set_", IDs[1], ".rds"))
  data_result <- data_resultset
  rm(data_resultset)
  
  # Second ID
  data_resultset <- readRDS(paste0("Project_NSE/Timeseries/set_", IDs[2], ".rds"))
  data_result <- data_result |> 
    full_join(data_resultset, by = "month")
  rm(data_resultset)

  # Difference
  data_result$diff <- unlist(data_result[,2]) - unlist(data_result[,3])
  
  # Regression
  data_result <- data_result |> 
    select(month, diff) |> 
    left_join(factors_monthly, by = "month") |> 
    left_join(factors_additional, by = "month")
  
  regression_data <- summary(lm(diff ~ 1 + mkt_excess + smb + hml + rmw + cma + mom + liq,
                                data = data_result))
  
  # Output results
  main_grid$alpha[setup_row] <- regression_data$coefficients[1,1]
  main_grid$alpha_se[setup_row] <- regression_data$coefficients[1,2]
  main_grid$beta_mkt[setup_row] <- regression_data$coefficients[2,1]
  main_grid$beta_mkt_se[setup_row] <- regression_data$coefficients[2,2]
  main_grid$beta_smb[setup_row] <- regression_data$coefficients[3,1]
  main_grid$beta_smb_se[setup_row] <- regression_data$coefficients[3,2]
  main_grid$beta_hml[setup_row] <- regression_data$coefficients[4,1]
  main_grid$beta_hml_se[setup_row] <- regression_data$coefficients[4,2]
  main_grid$beta_rmw[setup_row] <- regression_data$coefficients[5,1]
  main_grid$beta_rmw_se[setup_row] <- regression_data$coefficients[5,2]
  main_grid$beta_cma[setup_row] <- regression_data$coefficients[6,1]
  main_grid$beta_cma_se[setup_row] <- regression_data$coefficients[6,2]
  main_grid$beta_mom[setup_row] <- regression_data$coefficients[7,1]
  main_grid$beta_mom_se[setup_row] <- regression_data$coefficients[7,2]
  main_grid$beta_liq[setup_row] <- regression_data$coefficients[8,1]
  main_grid$beta_liq_se[setup_row] <- regression_data$coefficients[8,2]
  main_grid$r_squared[setup_row] <- regression_data$r.squared
  main_grid$residual_variance[setup_row] <- var(regression_data$residuals)

  # Output
  cat(round(setup_row/nrow(main_grid)*100, 2), "% \n")
}


# Save results -----------------------------------------------------
# Collapse IDs
main_grid <- main_grid |> unite("ID", starts_with("ID_"))

# Save final
saveRDS(main_grid, file = paste0("Project_NSE/Factor_diff/cluster_ID", cluster_id, ".rds"))

cat("\n\n Saved results.\n")
cat(as.character(Sys.time()))