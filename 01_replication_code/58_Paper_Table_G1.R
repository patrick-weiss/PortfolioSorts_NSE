
# Packages ----------------------------------------------------------------
library(tidyverse)
library(lmtest)
library(sandwich)
library(RSQLite)
library(xtable)


# Data ------------------------------------------------------------------
# Access regular database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Node text 
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")

# Factor differences
if(any(list.files("Data") == "data_factor_differences.RData")) {
  # Load existing data
  load("Data/data_factor_differences.RData")
} else {
  # List files
  data_files <- list.files("Data/Factor_diff")
  
  # Aggregate data
  data_overall <- readRDS(paste0("Data/Factor_diff/", data_files[1]))
  
  # Loop through remaining sets
  for(i in 2:length(data_files)) {
    # New data
    dataset <- readRDS(paste0("Data/Factor_diff/", data_files[i]))
    
    # Combine
    data_overall <- data_overall |> 
      bind_rows(dataset)
    
    # Finish
    cat(i, "\n")
    rm(dataset)
  }
  
  # Save
  save(data_overall, file = "Data/data_factor_differences.RData")
  rm(i)
}


# Table function --------------------------------------------------------
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

print_tex_table_part1 <- function(data, file = NA) {
  line1 <- " & \\multicolumn{3}{c}{Alpha} & \\multicolumn{3}{c}{MKT} & \\multicolumn{3}{c}{SMB} & \\multicolumn{3}{c}{HML} \\\\"
  line2 <- paste0("Fork ", paste0(rep("& Q10 & Mean & Q90", 4), collapse = ""), " \\\\")
  
  # Layout horizontal lines
  additional_layout <- list() 
  additional_layout$pos <- as.list(0)
  additional_layout$command <- as.vector(paste(line1, line2), mode = "character")
  

  # Actual printing
  if(is.na(file)) {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  } else {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = paste0("Paper_Tables/", file, ".tex"),
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  }
}

print_tex_table_part2 <- function(data, file = NA) {
  line1 <- " & \\multicolumn{3}{c}{RMW} & \\multicolumn{3}{c}{CMA} & \\multicolumn{3}{c}{LIQ} & \\multicolumn{3}{c}{MOM} \\\\"
  line2 <- paste0("Fork ", paste0(rep("& Q10 & Mean & Q90", 4), collapse = ""), " \\\\")
  
  # Layout horizontal lines
  additional_layout <- list() 
  additional_layout$pos <- as.list(0)
  additional_layout$command <- as.vector(paste(line1, line2), mode = "character")
  
  
  # Actual printing
  if(is.na(file)) {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  } else {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = paste0("Paper_Tables/", file, ".tex"),
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  }
}


# Analysis --------------------------------------------------------------
# Quantiles
data_results <- data_overall |> 
  select(-ends_with("_se")) |> 
  drop_na() |> 
  mutate(alpha = alpha * 100) |> 
  group_by(node, sorting_variable) |> 
  summarize(across(alpha:residual_variance, ~ quantile(.x, 0.1, na.rm = TRUE), .names = "{col}_q10"),
            across(alpha:residual_variance, ~ mean(.x, na.rm = TRUE), .names = "{col}_q50"),
            across(alpha:residual_variance, ~ quantile(.x, 0.9, na.rm = TRUE), .names = "{col}_q90"),
            .groups = "drop") |> 
  group_by(node) |> 
  summarize(across(!sorting_variable, ~ round(mean(.x), 2)),
            .groups = "drop")

# Rearrange
table_output <- mapping_nodes |> 
  left_join(data_results, by = "node") |> 
  select(node_name,
         node_order_mad,
         contains("alpha"), 
         contains("mkt"), 
         contains("smb"), 
         contains("hml"), 
         contains("rmw"), 
         contains("cma"), 
         contains("mom"), 
         contains("liq")) |> 
  arrange(node_order_mad) |>
  select(-node_order_mad) |> 
  rename_with(~gsub("beta_", "", .x, ))

# Create first output Table 

table_output_part1 <- table_output |>
  select(-rmw_q10, -rmw_q50, -rmw_q90, - cma_q10, -cma_q50, -cma_q90, -mom_q10, -mom_q50, -mom_q90, -liq_q10, -liq_q50, -liq_q90)

table_output_part2 <- table_output |>
  select(-alpha_q10, -alpha_q50, -alpha_q90, -mkt_q10, -mkt_q50, -mkt_q90, -smb_q10, -smb_q50, -smb_q90, -hml_q10, -hml_q50, -hml_q90)


# Print table part 1 
table_output_part1 |> 
  print_tex_table_part1(file = "G01a_factor_differences")

table_output_part2 |> 
  print_tex_table_part2(file = "G01b_factor_differences")

# Beta significance tests -----------------------------------------------
# Function
beta_test <- function(betas, standard_errors) {
  # Get t-stats
  tstats <- betas / standard_errors
  
  # Output
  return(tibble(Left = sum(tstats <= qnorm(p = 0.975)),
                Right = sum(tstats >= qnorm(p = 0.975))))
}


# Summary of significant betas
data_coef_significance <- data_overall |> 
  drop_na() |> 
  group_by(node, sorting_variable) |> 
  summarize(sig_beta_mkt = beta_test(beta_mkt, beta_mkt_se),
            sig_beta_smb = beta_test(beta_smb, beta_smb_se),
            sig_beta_hml = beta_test(beta_hml, beta_hml_se),
            sig_beta_rmw = beta_test(beta_rmw, beta_rmw_se),
            sig_beta_cma = beta_test(beta_cma, beta_cma_se),
            sig_beta_mom = beta_test(beta_mom, beta_mom_se),
            sig_beta_liq = beta_test(beta_liq, beta_liq_se),
            sig_n = n(),
            .groups = 'drop') |> 
  unnest(starts_with("sig_beta_"), names_repair = "universal") |> 
  select(-sorting_variable) |> 
  group_by(node) |> 
  summarize(across(everything(), ~ sum(.x)),
            .groups = 'drop')


# Significance ----------------------------------------------------------
# Test mean
data_significance <- data_overall |> 
  select(-ends_with("_se")) |> 
  drop_na() |> 
  mutate(alpha = alpha * 100) |> 
  group_by(node, sorting_variable) |> 
  summarize(across(alpha:residual_variance, ~ mean(.x, na.rm = TRUE), .names = "{col}_q50"),
            across(alpha:residual_variance, ~ coeftest(lm(.x ~ 1), vcov = vcovHC)[1,3], .names = "{col}_t"),
            .groups = "drop") |> 
  select(node, 
         sorting_variable,
         contains("alpha"), 
         contains("mkt"), 
         contains("smb"), 
         contains("hml"), 
         contains("rmw"), 
         contains("cma"), 
         contains("mom"), 
         contains("liq")) |> 
  rename_with(~gsub("beta_", "", .x, ))


# Median tests ----------------------------------------------------------
# Function
median_test <- function(betas, standard_errors) {
  # Median
  beta_median <- median(betas)
  
  # Get t-stats
  tstats <- abs((betas - beta_median)) / standard_errors
  
  # Output
  return(sum(tstats > qnorm(p = 0.95)))
}

# Apply function
median_results <- data_overall |> 
  drop_na() |> 
  group_by(node, sorting_variable) |> 
  summarize(median_alpha = median_test(alpha, alpha_se),
            median_beta_mkt = median_test(beta_mkt, beta_mkt_se),
            median_beta_smb = median_test(beta_smb, beta_smb_se),
            median_beta_hml = median_test(beta_hml, beta_hml_se),
            median_beta_rmw = median_test(beta_rmw, beta_rmw_se),
            median_beta_cma = median_test(beta_cma, beta_cma_se),
            median_beta_mom = median_test(beta_mom, beta_mom_se),
            median_beta_liq = median_test(beta_liq, beta_liq_se),
            median_n = n(),
            .groups = 'drop') |> 
  select(-sorting_variable) |> 
  group_by(node) |> 
  summarize(across(everything(), ~ sum(.x)),
            .groups = 'drop')

