
# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(xtable)


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results") |> 
  select(SV, group) |> 
  distinct()

# Significance in original paper
original_significance <- dbReadTable(data_nse, "significance_orig_paper") |> 
  mutate(sv = toupper(substr(sv, 4, nchar(sv))))


# Aggregate MHT results --------------------------------------------
# MHT files
MHT_output_files <- list.files("Data/MHT_results")

# All sorting variables
svs <- substr(MHT_output_files, 5, nchar(MHT_output_files) - 6)

# Initialize output
mht_results <- tibble("sv" = svs,
                      "median_p_min" = NA_real_,
                      "median_p_frac" = NA_real_,
                      "mean_p_min" = NA_real_,
                      "mean_p_frac" = NA_real_,
                      "capm_p_min" = NA_real_,
                      "capm_p_frac" = NA_real_,
                      "number_tests" = NA_real_)

# Loop through files
for(file_index in 1:length(MHT_output_files)) {
  # Load data
  load(paste0("Data/MHT_results/", MHT_output_files[file_index]))
  
  # Number of tests
  mht_results$number_tests[file_index] <- nrow(pvalues_median)
  
  # Median 
  ## Summary statistics
  pvalues_median <- pvalues_median |> 
    summarize(p_min = min(p_adjusted),
              p_frac = sum(p_adjusted < 0.05)/n())
  
  ## Assign
  mht_results$median_p_min[file_index] <- pvalues_median$p_min
  mht_results$median_p_frac[file_index] <- pvalues_median$p_frac
  
  # Mean 
  ## Summary statistics
  pvalues_mean <- pvalues_mean |> 
    summarize(p_min = min(p_adjusted),
              p_frac = sum(p_adjusted < 0.05)/n())
  
  ## Assign
  mht_results$mean_p_min[file_index] <- pvalues_mean$p_min
  mht_results$mean_p_frac[file_index] <- pvalues_mean$p_frac
  
  # CAPM 
  ## Summary statistics
  pvalues_capm <- pvalues_capm |> 
    summarize(p_min = min(p_adjusted),
              p_frac = sum(p_adjusted < 0.05)/n())
  
  ## Assign
  mht_results$capm_p_min[file_index] <- pvalues_capm$p_min
  mht_results$capm_p_frac[file_index] <- pvalues_capm$p_frac
  
  # Clear
  rm(pvalues_median, pvalues_mean, pvalues_capm)
}


# Table function ---------------------------------------------------
compute_total_averages <- function(data) {
  sv_means <- data |> 
    arrange(Group, SV) |>
    select(Group, SV, significance_orig_paper, Median:CAPM)
  
  # Overall means
  mean_all <- sv_means |> 
    summarize(Group = "Overall",
              SV = "All",
              across(Median:CAPM, ~ mean(.x)))
  
  # Sig means
  mean_sig <- sv_means |> 
    filter(significance_orig_paper == 1) |> 
    summarize(Group = "Overall",
              SV = "Orig. Sig.",
              across(Median:CAPM, ~ mean(.x)))
  
  # Insig means
  mean_insig <- sv_means |> 
    filter(significance_orig_paper == 0) |> 
    summarize(Group = "Overall",
              SV = "Orig. Insig.",
              across(Median:CAPM, ~ mean(.x)))
  
  # Combine
  mean_all |> 
    add_row(mean_sig) |> 
    add_row(mean_insig) |> 
    select(-Group)
}

# Function for columnnames
wrap_columnnames <- function(text) {
  for(i in 1:length(text)) {
    if(text[[i]] %in% c("Node", "Group", "SV")) {
      next
    } else {
      text[[i]] <- paste0("\\multicolumn{1}{l}{", text[[i]], "}") 
    }
  }
  
  return(text)
}

print_tex_table <- function(data, file = NA) {
  # Layout horizontal lines
  additional_layout <- list() 
  additional_layout$pos <- as.list(data |> nrow() - 1)
  additional_layout$command <- as.vector(rep("\\midrule ", length(additional_layout$pos)), mode = "character")
  if(nrow(data) == 1 | any(data$SV == "All")) additional_layout$command <- ""
  
  # Actual printing
  if(is.na(file)) {
    print(xtable(data),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  } else {
    print(xtable(data),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = paste0("Paper_Tables/", file, ".tex"),
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  }
}


# Table D01 --------------------------------------------------------
# Compute summaries
mht_results <- mht_results |>      
  mutate(sv = toupper(substr(sv, 4, nchar(sv)))) |> 
  mutate(Median = median_p_frac,
         Premia = mean_p_frac,
         CAPM = capm_p_frac) |> 
  select(sv, Median:CAPM)

# Combine additional data & add significance stars
table_across_sv <- mht_results |>           
  left_join(data_premium_results, join_by(sv == SV)) |> 
  left_join(original_significance, join_by(sv == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(sv, "*"), sv)) |> 
  select(Group = group, SV, Median:CAPM) |> 
  arrange(Group, SV)
  
# Loop through groups and create panels
for(the_group in 1:length(unique(table_across_sv$Group))) {
  # Subset full table
  the_table <- table_across_sv |> 
    filter(Group == unique(table_across_sv$Group)[the_group])
  
  # Compute mean
  the_table_mean <- the_table |> 
    summarize(Group = "",
              SV = "Mean",
              across(.cols = Median:CAPM, mean))
  
  ## No summary for single variable
  if(nrow(the_table) == 1) {the_table_mean <- NULL}
  
  # Add mean and remove group
  the_table <- the_table |> 
    bind_rows(the_table_mean) |> 
    select(-Group)
  
  # Print table
  ## Table name
  the_table_name <- paste0("D01", 
                           letters[the_group], 
                           "_MHT_acrosssvs_", 
                           str_to_lower(unique(table_across_sv$Group)[the_group]))
  
  ## Final print
  the_table |> print_tex_table(file = the_table_name)
}

# Overall panel's construction 
table_overall <- table_across_sv |> 
  left_join(original_significance |> 
              mutate(sv = if_else(significance_orig_paper == 0, 
                                  paste0(sv, "*"), 
                                  sv)), by = join_by(SV == sv)) |> 
  compute_total_averages()

# Print table
table_overall |> print_tex_table(file = "D01i_MHT_acrosssvs_overall")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)