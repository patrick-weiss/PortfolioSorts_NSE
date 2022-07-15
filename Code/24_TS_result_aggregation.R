# Aggregate TS results locally
## This code is highly inefficient

# Packages ----------------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Aggregation code --------------------------------------------------------

# First aggregation
files_ts <- list.files("Data/TS_information")

## First
data_TS_results <- readRDS(paste0("Data/TS_information/", files_ts[1]))
files_ts <- files_ts[-1]

# Loop
for(file_is in files_ts) {
  cat(file_is)
  
  data_TS_results_set <-readRDS(paste0("Data/TS_information/", file_is))

  data_TS_results <- data_TS_results %>%
    bind_rows(data_TS_results_set)
}


# Save results ------------------------------------------------------------
data_TS_results %>%
  dbWriteTable(data_nse, "data_TS_results", ., overwrite = TRUE)

