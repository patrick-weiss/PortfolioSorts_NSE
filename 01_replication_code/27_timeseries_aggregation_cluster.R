# Aggregate all time series of AG


# Setup ------------------------------------------------------------
# Packages 
library(tidyverse, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(DBI, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(RSQLite, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# Sorting data
load("Project_NSE/Data/data_grid.RData")

# Variable
the_variable <- "sv_ag"

# Starting
cat(paste("Starting Aggregation - ", Sys.time()), "\n")


# Create & fill database -------------------------------------------
# IDS
SV_ids <- setup_grid |> 
  filter(sorting_variable == the_variable) |> 
  pull(ID)

# Aggregate SV's time series
# Database 
data_TS <- dbConnect(SQLite(), 
                     paste0("Project_NSE/data_TS_", 
                            the_variable, 
                            "_full.sqlite"), 
                     extended_types = TRUE)

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