# Rebuild figures and tables


# Figures -----------------------------------------------------------------
# All figure files
files_figures <- list.files()[grepl("^4", list.files())]

# Loop through files
for(the_file in files_figures) {
  # Show file
  cat(the_file, "\n")
  
  # Source
  source(the_file)
  
  # Remove all 
  rm(list = ls()[!(grepl("^files", ls()))])
}


# Tables ------------------------------------------------------------------
# All table files
files_tables <- list.files()[grepl("^5", list.files())]

# Loop through files
for(the_file in files_tables) {
  # Show file
  cat(the_file, "\n")
  
  # Source
  source(the_file)
  
  # Remove all 
  rm(list = ls()[!(grepl("^files", ls()))])
}


# IA Figures --------------------------------------------------------------
# All IA figure files
files_figures_IA <- list.files()[grepl("^6", list.files())]

# Loop through files
for(the_file in files_figures_IA) {
  # Show file
  cat(the_file, "\n")
  
  # Source
  source(the_file)
  
  # Remove all 
  rm(list = ls()[!(grepl("^files", ls()))])
}

# Tables ------------------------------------------------------------------
# All table files
files_tables_IA <- list.files()[grepl("^7", list.files())]

# Loop through files
for(the_file in files_tables_IA) {
  # Show file
  cat(the_file, "\n")
  
  # Source
  source(the_file)
  
  # Remove all 
  rm(list = ls()[!(grepl("^files", ls()))])
}
