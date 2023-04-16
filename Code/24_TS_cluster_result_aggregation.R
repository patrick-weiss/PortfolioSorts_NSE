# Aggregate TS results on the cluster


# Packages ----------------------------------------------------------------
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(DBI, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(RSQLite, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# Access Database 
data_nse_TS <- dbConnect(SQLite(), 
                         "Project_NSE/data_nse_TS.sqlite", 
                         extended_types = TRUE)


# Aggregation code --------------------------------------------------------
# First aggregation
files_ts <- list.files("Project_NSE/TS_information")
files_number <- length(files_ts)

# Loop
for(j in 1:files_number) {
  # Feedback
  cat(paste0(files_ts[[j]], "\n"))
  
  # Load data
  data_TS_results_set <- readRDS(paste0("Project_NSE/TS_information/", files_ts[[j]]))
  
  # Escape small sets
  if(nrow(data_TS_results_set) == 1) next()
  
  # Vector results ------------------------------------------------------
  # Split results vector results
  data_TS_results_vector <- data_TS_results_set |> 
    select(-ends_with("_ts"))

  # Store vector results in sqlite database
  dbWriteTable(
    conn = data_nse_TS,
    name = "data_TS_results",
    value = data_TS_results_vector,
    overwrite = ifelse(j == 1, TRUE, FALSE),
    append = ifelse(j != 1, TRUE, FALSE)
  )
  
  # Free memory
  rm(data_TS_results_vector)

  # MAD -----------------------------------------------------------------
  # Unwrap MADs
  ## RAW
  data_mad_ts_R <- data_TS_results_set |> 
    select(mad_ts) |> 
    unnest(cols = c(mad_ts)) |> 
    group_by(month) |> 
    summarize(diff_avg_R = mean(diff, na.rm = T),
              obs_R = n(),
              .groups = "drop") |> 
    mutate(node = unique(data_TS_results_set$node),
           sorting_variable = unique(data_TS_results_set$sorting_variable)) |> 
    select(month, node, sorting_variable, ends_with("_R"))
  
  ## CAPM
  data_mad_ts_C <- data_TS_results_set |> 
    select(mad_C_ts) |> 
    unnest(cols = c(mad_C_ts)) |> 
    group_by(month) |> 
    summarize(diff_avg_C = mean(diff, na.rm = T),
              obs_C = n(),
              .groups = "drop") 
  
  ## FF
  data_mad_ts_F <- data_TS_results_set |> 
    select(mad_F_ts) |> 
    unnest(cols = c(mad_F_ts)) |> 
    group_by(month) |> 
    summarize(diff_avg_F = mean(diff, na.rm = T),
              obs_F = n(),
              .groups = "drop") 
  
  ## Q
  data_mad_ts_Q <- data_TS_results_set |> 
    select(mad_Q_ts) |> 
    unnest(cols = c(mad_Q_ts)) |> 
    group_by(month) |> 
    summarize(diff_avg_Q = mean(diff, na.rm = T),
              obs_F = n(),
              .groups = "drop") 
  
  
  # Merge MADs' timeseries
  data_mad_ts <- data_mad_ts_R |> 
    full_join(data_mad_ts_C, by = "month") |> 
    full_join(data_mad_ts_F, by = "month") |> 
    full_join(data_mad_ts_Q, by = "month") |> 
    mutate(node = unique(data_TS_results_set$node),
           sorting_variable = unique(data_TS_results_set$sorting_variable)) |> 
    select(month, node, sorting_variable, 
           ends_with("_R"), ends_with("_C"), ends_with("_F"), ends_with("_Q"))
  
  
  # Store MAD results in sqlite database
  dbWriteTable(
    conn = data_nse_TS,
    name = "data_MAD_TS_results",
    value = data_mad_ts,
    overwrite = ifelse(j == 1, TRUE, FALSE),
    append = ifelse(j != 1, TRUE, FALSE)
  )
  
  # Free memory
  rm(data_mad_ts, data_mad_ts_R, data_mad_ts_C, data_mad_ts_F, data_mad_ts_Q)
  rm(data_TS_results_set)
}


# Disconnect ------------------------------------------------------------
dbDisconnect(data_nse_TS)

