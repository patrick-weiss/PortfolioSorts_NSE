# Aggregate Portfolio Sort estimation
## This file is meant to be run in parallel on a cluster

# Setup -------------------------------------------------------------------
# The files
data_files <- list.files("Project_NSE/Results")


# Aggregate ---------------------------------------------------------------
# Aggregate files
# First
# Load
data_resultset <- readRDS(paste0("Project_NSE/Results/", data_files[1]))

# Create results table
data_result <- data_resultset

# Remove first
data_files <- data_files[-1]
rm(data_resultset)

# Loop through remaining files
for(data_file in data_files) {
  cat(".")
  # Load 
  data_resultset <- readRDS(paste0("Project_NSE/Results/", data_file))
  
  # Add to results table
  data_result <- data_result %>%
    bind_rows(data_resultset)
  
  # Remove
  rm(data_resultset)
}



# Save file ---------------------------------------------------------------
save(data_result, file = "Project_NSE/data_grid_results.Rdata")

# Print report
cat("\n\n Saved main results.\n")

