# MAD and correlation analysis of TS
## This file is meant to be run in parallel on a cluster
## Fil 24 aggregates the time series locally

# Setup -------------------------------------------------------------------

# Packages 
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(corrr, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# ID
cluster_id = as.integer(Sys.getenv("SGE_TASK_ID"))
cat(paste("Starting Esimation", cluster_id, " - ", Sys.time()), "\n")

# Fama French Factors
load("Project_NSE/Data/data_FF.Rdata")

# Sorting data
load("Project_NSE/Data/data_grid.RData")

# Decision nodes & sorting variables
input_decision_nodes <- 14
input_sorting_variables <- 41

# ID set ------------------------------------------------------------------
node <- (cluster_id - 1) %/% input_sorting_variables + 1
sv <- cluster_id %% input_sorting_variables + 1

# Text
node_text <- colnames(setup_grid)[1+node]
sv_text <- unique(setup_grid$sorting_variable)[sv]

# Node levels
node_levels <- setup_grid %>% 
  filter(sorting_variable == sv_text) %>%
  pull({{ node_text }}) %>% 
  unique()
  
## sorting method
if(node_text == "sorting_method") node_levels <- node_levels[node_levels != "single"]
if(node_text == "n_portfolios_secondary") node_levels <- node_levels[!is.na(node_levels)]

# Progress reports
cat(paste(node_text, "for", sv_text, "\n"))

# Main variables
main_grid <- setup_grid %>% 
  filter(.data[[node_text]] == node_levels[1],
         sorting_variable == sv_text)

# Only one level
if(length(node_levels) == 1) {
  cat("---> Only one level!")
  
  # Main grid
  main_grid <- main_grid %>%
    slice(1) %>%
    mutate(ID = NA) %>%
    select(starts_with("ID")) %>%
    mutate(node = node_text,
           sorting_variable = sv_text,
           cor = as.numeric(NA),
           mad = as.numeric(NA),
           cor_C = as.numeric(NA),
           mad_C = as.numeric(NA),
           cor_F = as.numeric(NA),
           mad_F = as.numeric(NA))
  
  # Save & stop
  saveRDS(main_grid, file = paste0("Project_NSE/TS_information/cluster_ID", cluster_id, ".rds"))
  stop("DONE")
}

# Second, third, etc
for(node_level in 2:length(node_levels)) {
  # Define name
  node_ID <- paste0("ID_", {{node_level}})
  
  # Add new id
  further_grid <- main_grid %>% 
    mutate({{ node_text }} := node_levels[node_level]) %>% 
    select(-starts_with("ID")) %>%
    left_join(setup_grid) %>%
    rename({{ node_ID }} := ID)
  
  # Merge back
  main_grid <- main_grid %>%
    bind_cols(further_grid %>% select(starts_with("ID_")))
  
  rm(node_ID, further_grid)
}

# Select relevant information from grid
main_grid <- main_grid %>%
  rename(ID_1 = ID) %>%
  select(starts_with("ID_")) %>%
  mutate(node = node_text,
         sorting_variable = sv_text,
         cor = as.numeric(NA),
         mad = as.numeric(NA),
         cor_C = as.numeric(NA),
         mad_C = as.numeric(NA),
         cor_F = as.numeric(NA),
         mad_F = as.numeric(NA))


# Functions ---------------------------------------------------------------
# Compute CAPM-adjusted TS
compute_CAPM_TS <- function(data) {
  # Compute CAPM
  data_CAPM <- data %>%
    left_join(factors_ff_monthly, by = "month") %>%
    drop_na(mkt_excess)
  
  for(i in 1:(ncol(data)-1)) {
    # Variables
    prem <- unlist(data_CAPM[, i + 1])
    regs <- as.matrix(data_CAPM[,c("mkt_excess")])
    
    # Regression & TS
    reg <- lm(prem ~ regs)
    TS <- reg$coefficients[1] + reg$residual
    
    # Slot data back
    data_CAPM[which(!is.na(data_CAPM[i + 1])), i + 1] <- TS
    rm(TS, prem, regs, reg)
  }
  
  # Output
  data_CAPM %>% select(-rf, -mkt_excess, -smb, -hml)
}

# Compute FF-adjusted TS
compute_FF_TS <- function(data) {
  # Compute CAPM
  data_FF <- data %>%
    left_join(factors_ff_monthly, by = "month") %>%
    drop_na(mkt_excess)
  
  for(i in 1:(ncol(data)-1)) {
    # Variables
    prem <- unlist(data_FF[, i + 1])
    regs <- as.matrix(data_FF[,c("mkt_excess", "smb", "hml")])
    
    # Regression & TS
    reg <- lm(prem ~ regs)
    TS <- reg$coefficients[1] + reg$residual
    
    # Slot data back
    data_FF[which(!is.na(data_FF[i + 1])), i + 1] <- TS
    rm(TS, prem, regs, reg)
  }
  
  # Output
  data_FF %>% select(-rf, -mkt_excess, -smb, -hml)
}

# Correlation function
compute_cor <- function(data) {
  # Compute correlation
  data_corr <- data %>%
    select(-month) %>%
    as.matrix() %>%
    cor(., use = "complete.obs")
  
  # Average correlation (for nodes with more than 2 outcomes)
  sum(data_corr*upper.tri(data_corr), na.rm = T)/sum(upper.tri(data_corr))
}

# Difference function
take_diff <- function(x, y) mean(abs(x - y), na.rm = T)

# Mean absolute difference function
compute_mad <- function(data) {
  # Compute MAD
  data_mad <- data %>%
    select(-month) %>%
    colpair_map(take_diff) %>%
    select(-term) %>% 
    as.matrix()
  
  # Average MADs (for nodes with more than 2 outcomes)
  sum(data_mad*upper.tri(data_mad), na.rm = T)/sum(upper.tri(data_mad))
}


# Compute statistics ------------------------------------------------------
for(setup_row in 1:nrow(main_grid)) {
  # Take IDs
  IDs <- main_grid %>%
    slice(setup_row) %>%
    select(starts_with("ID_")) %>%
    unlist()
  
  # First ID
  data_resultset <- readRDS(paste0("Project_NSE/Timeseries2/set_", IDs[1], ".rds"))
  data_result <- data_resultset
  rm(data_resultset)
  
  # Rest of IDs
  for(i in 2:length(IDs)) {
    data_resultset <- readRDS(paste0("Project_NSE/Timeseries2/set_", IDs[i], ".rds"))
    data_result <- data_result %>%
      full_join(data_resultset, by = "month")
    rm(data_resultset)
  }
  
  # Get CAPM- and FF-adjusted data
  data_result_CAPM <- compute_CAPM_TS(data_result)
  data_result_FF <- compute_FF_TS(data_result)
  
  # Compute correlations
  main_grid$cor[setup_row]<- compute_cor(data_result)
  main_grid$cor_C[setup_row]<- compute_cor(data_result_CAPM)
  main_grid$cor_F[setup_row]<- compute_cor(data_result_FF)
  
  # Mean absolute difference
  main_grid$mad[setup_row]<- compute_mad(data_result)
  main_grid$mad_C[setup_row]<- compute_mad(data_result_CAPM)
  main_grid$mad_F[setup_row]<- compute_mad(data_result_FF)
  
  # Output
  cat(round(setup_row/nrow(main_grid)*100, 2), "% \n")
}


# Save results ------------------------------------------------------------
# Collapse IDs
main_grid <- main_grid %>% unite("ID", starts_with("ID_"))

# Save final
saveRDS(main_grid, file = paste0("Project_NSE/TS_information/cluster_ID", cluster_id, ".rds"))

cat("\n\n Saved results.\n")
cat(as.character(Sys.time()))
