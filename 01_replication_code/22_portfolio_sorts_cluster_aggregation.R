# Aggregate Portfolio Sort estimation
## This file is meant to be run in parallel on a cluster

# Setup ------------------------------------------------------------
library(DBI, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')
library(RSQLite, lib.loc = '/gpfs/home/home/pweiss/R/x86_64-pc-linux-gnu-library/4.1')

# Database 
data_grid_results <- dbConnect(SQLite(), 
                               "Project_NSE/data_grid_results.sqlite", 
                                extended_types = TRUE)


# Aggregation code -------------------------------------------------
# Inputs
data_files <- list.files("Project_NSE/Results")
files_number <- length(data_files)

# Loop
for(j in 1:files_number) {
  # Feedback
  cat(paste0(data_files[[j]], "\n"))
  
  # Load data
  data_resultset <- readRDS(paste0("Project_NSE/Results/", data_files[[j]]))
  
    # Store results in sqlite database
  dbWriteTable(
    conn = data_grid_results,
    name = "data_grid_results",
    value = data_resultset,
    overwrite = ifelse(j == 1, TRUE, FALSE),
    append = ifelse(j != 1, TRUE, FALSE)
  )
  
  # Free memory
  rm(data_resultset)
}


# Disconnect -------------------------------------------------------
dbDisconnect(data_grid_results)

# Print report
cat("\n\n Saved main results.\n")
cat(as.character(Sys.time()))