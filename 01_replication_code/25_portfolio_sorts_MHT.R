# Run multiple hypothesis tests on portfolio sorts (cluster per sorting variable)
## Step 1: Aggregate time series
## Step 2: Compute MHT for mean/CAPM alpha and against median

# Setup ------------------------------------------------------------
# ID 
cluster_id = as.integer(Sys.getenv("SGE_TASK_ID"))

# Packages 
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(janitor, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(wildrwolf, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(fixest, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(DBI, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(RSQLite, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# Fama French Factors
load("Project_NSE/Data/data_factors.Rdata")

## Subset FF factors
factors_monthly <- factors_monthly |> 
  select(month, mkt_excess)

# Sorting data
load("Project_NSE/Data/data_grid_MHT.RData")

# Variable
the_variable <- unique(setup_grid_MHT$sorting_variable)[cluster_id]

# Starting
cat(paste("Starting Esimation", the_variable, " - ", Sys.time()), "\n")


# Create & fill database -------------------------------------------
# IDS
SV_ids <- setup_grid_MHT |> 
  filter(sorting_variable == the_variable) |> 
  pull(ID)

# Check if completed
# Existing files
files_ran <- list.files("Project_NSE/TS_aggregates_MHT")
files_ran <- substr(files_ran, 9, nchar(files_ran)-7)

# Check if file ran w/o problems before
if(the_variable %in% files_ran) {
  # SV's time series already aggregated
  cat(paste("Aggregation for", the_variable, "completed"))
} else {
  # Aggregate SV's time series
  # Database 
  data_TS <- dbConnect(SQLite(), 
                       paste0("Project_NSE/TS_aggregates_MHT/data_TS_", 
                              the_variable, 
                              ".sqlite"), 
                       extended_types = TRUE)
  
  # SV's time series aggregating
  cat(paste("Starting aggregation for", the_variable))
  
  # Loop through IDs and save the time series
  for(the_id in SV_ids) {
    # Load data
    data_timeseries <- readRDS(paste0("Project_NSE/Timeseries/set_", 
                                      the_id, 
                                      ".rds")) 
    
    # Add id
    data_timeseries <- data_timeseries |> 
      mutate(ID = the_id)
    
    # Store vector results in sqlite database
    dbWriteTable(
      conn = data_TS,
      name = "data_timeseries",
      value = data_timeseries,
      overwrite = ifelse(the_id == SV_ids[1], TRUE, FALSE),
      append = ifelse(the_id != SV_ids[1], TRUE, FALSE)
    )
    
    # Free memory
    rm(data_timeseries)
  }
  
  # Disconnect
  dbDisconnect(data_TS)
}


# MHT --------------------------------------------------------------
# Database
data_TS <- dbConnect(SQLite(),
                     paste0("Project_NSE/TS_aggregates_MHT/data_TS_",
                            the_variable,
                            ".sqlite"),
                     extended_types = TRUE)

# Read full table
data_timeseries <- dbReadTable(data_TS, "data_timeseries")

# Compute overall median
sv_median <- data_timeseries |>
  group_by(ID) |>
  summarize(mean = mean(premium, na.rm = TRUE),
            .groups = "drop") |>
  pull(mean) |>
  median()

# Pivot time series wider
data_timeseries <- data_timeseries |>
  arrange(ID, month) |> 
  pivot_wider(values_from = premium,
              names_from = ID,
              names_prefix = "spec_") |>
  arrange(month) |>
  left_join(factors_monthly, by = "month") |> 
  select(-month)

# Select specifications
specifications <- data_timeseries |>
  select(starts_with("spec_")) |>
  colnames()

# Feedback
cat(paste("Observed specifications:", length(specifications), "\n"))


# Premia's difference from median ----------------------------------
# Regressions
regression_median <- data_timeseries |>
  mutate(across(starts_with("spec_"), ~ .x - sv_median)) |>
  feols(fml = .[specifications] ~ 1)

# MHT p-values
pvalues_median <- rwolf(models = regression_median,
                        param = "(Intercept)",
                        B = 9999,
                        seed = 2023) |> 
  janitor::clean_names() |> 
  mutate(specification = specifications,
         ID = substr(specification, 6, nchar(specification))) |> 
  select(ID, p_adjusted = rw_pr_t)

# Free memory
rm(regression_median)
gc()

# Feedback
cat(paste("Completed median p-values at", Sys.time(), "\n\n"))


# Premia's difference from mean ------------------------------------
# Regressions
regression_mean <- data_timeseries |>
  feols(fml = .[specifications] ~ 1)

# MHT p-values
pvalues_mean <- rwolf(models = regression_mean,
                        param = "(Intercept)",
                        B = 9999,
                        seed = 2023) |> 
  janitor::clean_names() |> 
  mutate(specification = specifications,
         ID = substr(specification, 6, nchar(specification))) |> 
  select(ID, p_adjusted = rw_pr_t)

# Free memory
rm(regression_mean)
gc()

# Feedback
cat(paste("Completed mean p-values at", Sys.time(), "\n\n"))


# Premia's difference from CAPM ------------------------------------
# Regressions
regression_capm <- data_timeseries |>
  feols(fml = .[specifications] ~ 1 + mkt_excess)

# MHT p-values
pvalues_capm <- rwolf(models = regression_capm,
                      param = "(Intercept)",
                      B = 9999,
                      seed = 2023) |> 
  janitor::clean_names() |> 
  mutate(specification = specifications,
         ID = substr(specification, 6, nchar(specification))) |> 
  select(ID, p_adjusted = rw_pr_t)

# Free memory
rm(regression_capm)
gc()

# Feedback
cat(paste("Completed CAPM p-values at", Sys.time(), "\n\n"))


# Finish -----------------------------------------------------------
# Store
save(list = c("pvalues_median", 
              "pvalues_mean", 
              "pvalues_capm"), 
     file = paste0("Project_NSE/MHT/mht_", 
                   the_variable, 
                   ".rdata"))

# Close connection 
dbDisconnect(data_TS)