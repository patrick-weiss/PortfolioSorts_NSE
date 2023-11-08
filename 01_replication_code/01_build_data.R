# This script builds all data

# House keeping function -------------------------------------------
# Echo?
option_echo <- TRUE

# Implement source for file
run_source <- function(file_name) {
  # Print time
  print(as.character(Sys.time()))
  
  # Source file
  source(file_name, echo = option_echo)
  
  # Clean afterwards (not perfect in any way, restart strictly better)
  rm(list = ls()[which(!(ls() %in% c("run_source", "option_echo")))])
  invisible(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
                   detach,
                   character.only=TRUE,
                   unload=TRUE))
}


# Remove legacy files ----------------------------------------------
file.remove("Data/data_nse.sqlite")
file.remove("Data/data_factors.Rdata")
file.remove("Data/data_grid.RData")
file.remove("Data/data_grid_MHT.RData")
file.remove("Data/data_sorts.RData")
file.remove("Data/data_grid_results.sqlite")
file.remove("Data/data_nse_TS.sqlite")


# Source files -----------------------------------------------------
set.seed("20211021")
run_source("10_data.R")
run_source("11_Sorting_variables_daily.R")
run_source("12_Sorting_variables_monthly.R")
run_source("13_Sorting_variables_quarterly.R")
run_source("14_Sorting_variables_yearly.R")
run_source("15_Sorting_variable_groups.R")
run_source("16_Sorting_variables_direction.R")
run_source("17_Decision_Nodes.R")
run_source("18_Significance_in_original_paper.R")
run_source("19_Paths_original_paper.R")
run_source("20_portfolio_sorts_cluster_prep.R")

# Post cluster -----------------------------------------------------
if(!any(list.files("Data") == "data_grid_results.sqlite")) stop("Is the cluster done?")
set.seed("20211021")
run_source("28_result_final_prep.R")
run_source("29_result_final_model_comp.R")
