# Load ans save the paths that were taken in the original reference papers for our 68 sorting variables  

# Packages and Setup -----------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)

# Load dataset (CSV file)

paths_original_paper <- read.csv("Data/Paths_Original_Reference_Papers.csv", 
                                 header=TRUE, 
                                 stringsAsFactors=FALSE, 
                                 sep = ";")

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

# Store the paths in the original paper ----------------------------
# Store
paths_original_paper |> 
  dbWriteTable(conn = data_nse, 
               name = "paths_original_paper", 
               value = _, 
               overwrite = TRUE)

