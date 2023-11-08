
# Packages ---------------------------------------------------------
library(tidyverse)
library(moments)
library(RSQLite)
library(xtable)
library(furrr)


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")

# Significance in original paper
original_significance <- dbReadTable(data_nse, "significance_orig_paper") |> 
  mutate(sv = toupper(substr(sv, 4, nchar(sv))))

# Probabilities 
probability_grid <- dbReadTable(data_nse, "probability_grid") |> 
  mutate(sorting_variable = toupper(substr(sorting_variable, 4, nchar(sorting_variable))))


# Simulation function ----------------------------------------------
simulate_data <- function(data, sorting_variable, probability_set) {
  # Subset probability grid to get IDs and probabilities
  ## Which sorting variable
  ids <- probability_grid |> 
    filter(sorting_variable == {{ sorting_variable }})
  
  ## Which probability set
  if(probability_set == "56") {
    ids <- ids |> 
      select(ID, probability = probability_56)
  } else if(probability_set == "109") {
    ids <- ids |> 
      select(ID, probability = probability_109)
  } else stop("Probability not specified.")
  
  # Premiums to sample
  set.seed(2023)
  premiums_sample <- sample(x = ids$ID, 
                            size = 10^8, 
                            replace = TRUE,
                            prob = ids$probability)
  
  # Merge back
  premiums_output <- tibble(ID = premiums_sample) |> 
    left_join(data |> select(ID, 
                             premiums = mean, 
                             tstats = t, 
                             se = se),
              by = "ID") |> 
    select(-ID)
  
  # Return
  return(premiums_output)
}


# Table function ---------------------------------------------------
# IQR function
iqr <- function(premiums, quantiles = 0.25) {
  quantile(premiums, 1 - quantiles) - quantile(premiums, quantiles) |> 
    as.numeric()
}

# NSE significance
nse_test <- function(premiums, standard_errors, left_tail = TRUE, sigifiance_level = 0.05) {
  # Median premium
  prem_median <- median(premiums)
  
  # Individual deviations from the mean
  t_values <- (premiums - prem_median) / standard_errors
  
  # Return sum
  sum(t_values * ifelse(left_tail, -1, 1) > qnorm(1 - sigifiance_level/2))
}

# Compute statistics
compute_summaries <- function(data, quantiles = 0.25, probability_set) {
  # Feedback
  cat(paste0(unique(data$the_SV), "\n"))
  
  # Data simulation
  simulated_data <- data |> 
    simulate_data(data = _,
                  sorting_variable = unique(data$the_SV),
                  probability_set = probability_set)
  
  # Data summaries
  data |> 
    summarize(Group = unique(group),
              Mean = mean(simulated_data$premiums),
              NSE = iqr(simulated_data$premiums, quantiles = quantiles),
              Left = nse_test(simulated_data$premiums, simulated_data$se, left_tail = TRUE)/nrow(simulated_data),
              Right = nse_test(simulated_data$premiums, simulated_data$se, left_tail = FALSE)/nrow(simulated_data),
              Ratio = sd(simulated_data$premiums)/mean(simulated_data$se),
              Skew. = skewness(simulated_data$premiums),
              Kurt. = kurtosis(simulated_data$premiums),
              Pos. = sum(simulated_data$premiums > 0)/nrow(simulated_data),
              Sig. = sum(simulated_data$tstats > qnorm(0.975))/nrow(simulated_data))
}

# Overall panel construction
compute_total_averages <- function(data) {
  sv_means <- data |> 
    arrange(Group, SV) |>
    select(Group, SV, significance_orig_paper, Mean:Sig.)
  
  # Overall means
  mean_all <- sv_means |> 
    summarize(Group = "Overall",
              SV = "All",
              across(Mean:Sig., ~ mean(.x)))
  
  # Sig means
  mean_sig <- sv_means |> 
    filter(significance_orig_paper == 1) |> 
    summarize(Group = "Overall",
              SV = "Orig. Sig.",
              across(Mean:Sig., ~ mean(.x)))
  
  # Insig means
  mean_insig <- sv_means |> 
    filter(significance_orig_paper == 0) |> 
    summarize(Group = "Overall",
              SV = "Orig. Insig.",
              across(Mean:Sig., ~ mean(.x)))
  
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
  
  # Merge columns
  data <- data |> 
    mutate(Left = paste0("(",
                         formatC(Left, digits = 2, format = "f"),
                         ", ",
                         formatC(Right, digits = 2, format = "f"),
                         ")")) |> 
    rename("Left-right"= Left) |> 
    select(-Right)
  
  
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



# Table IA21: CAPM -------------------------------------------------
# Main panels' production
data_premium_nested <- data_premium_results |>
  mutate(mean = alpha_CAPM,
         se = se_CAPM,
         t = t_CAPM,
         the_SV = SV) |> 
  select(ID, SV, the_SV, group, mean, se, t) |> 
  nest(.by = SV)

# Parallelization
plan(multisession, workers = 6)

table_across_sv <- data_premium_nested |> 
  mutate(results = future_map(data, ~ compute_summaries(data = .x, 
                                                        probability_set = "109"),
                              .options = furrr_options(seed = NULL))) |> 
  unnest(cols = results) |>
  arrange(Group, SV) |>
  select(Group, SV, Mean:Sig.)

plan(sequential)

# Add significance indicator
table_across_sv <- table_across_sv |> 
  left_join(original_significance, by = join_by(SV == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(SV, "*"), SV)) |> 
  select(-significance_orig_paper)

# Loop through groups and create panels
for(the_group in 1:length(unique(table_across_sv$Group))) {
  # Subset full table
  the_table <- table_across_sv |> 
    filter(Group == unique(table_across_sv$Group)[the_group])
  
  # Compute mean
  the_table_mean <- the_table |> 
    summarize(Group = "",
              SV = "Mean",
              across(.cols = Mean:Sig., mean))
  
  ## No summary for single variable
  if(nrow(the_table) == 1) {the_table_mean <- NULL}
  
  # Add mean and remove group
  the_table <- the_table |> 
    bind_rows(the_table_mean) |> 
    select(-Group)
  
  # Print table
  ## Table name
  the_table_name <- paste0("IA07", 
                           letters[the_group], 
                           "_NSE_CAPM_109_acrosssvs_", 
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
table_overall |> print_tex_table(file = "IA07i_NSE_CAPM_109_acrosssvs_overall")


# Table IA22: FF5 --------------------------------------------------
# Main panels' production
data_premium_nested <- data_premium_results |>
  mutate(mean = alpha_FF5,
         se = se_FF5,
         t = t_FF5,
         the_SV = SV) |> 
  select(ID, SV, the_SV, group, mean, se, t) |> 
  nest(.by = SV)

# Parallelization
plan(multisession, workers = 6)

table_across_sv <- data_premium_nested |> 
  mutate(results = future_map(data, ~ compute_summaries(data = .x, 
                                                        probability_set = "109"),
                              .options = furrr_options(seed = NULL))) |> 
  unnest(cols = results) |>
  arrange(Group, SV) |>
  select(Group, SV, Mean:Sig.)

plan(sequential)

# Add significance indicator
table_across_sv <- table_across_sv |> 
  left_join(original_significance, by = join_by(SV == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(SV, "*"), SV)) |> 
  select(-significance_orig_paper)

# Loop through groups and create panels
for(the_group in 1:length(unique(table_across_sv$Group))) {
  # Subset full table
  the_table <- table_across_sv |> 
    filter(Group == unique(table_across_sv$Group)[the_group])
  
  # Compute mean
  the_table_mean <- the_table |> 
    summarize(Group = "",
              SV = "Mean",
              across(.cols = Mean:Sig., mean))
  
  ## No summary for single variable
  if(nrow(the_table) == 1) {the_table_mean <- NULL}
  
  # Add mean and remove group
  the_table <- the_table |> 
    bind_rows(the_table_mean) |> 
    select(-Group)
  
  # Print table
  ## Table name
  the_table_name <- paste0("IA08", 
                           letters[the_group], 
                           "_NSE_FF5_109_acrosssvs_", 
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
table_overall |> print_tex_table(file = "IA08i_NSE_FF5_109_acrosssvs_overall")


# Table IA23: Q5 ---------------------------------------------------
# Main panels' production
data_premium_nested <- data_premium_results |>
  mutate(mean = alpha_q5,
         se = se_q5,
         t = t_q5,
         the_SV = SV) |> 
  select(ID, SV, the_SV, group, mean, se, t) |> 
  nest(.by = SV)

# Parallelization
plan(multisession, workers = 6)

table_across_sv <- data_premium_nested |> 
  mutate(results = future_map(data, ~ compute_summaries(data = .x, 
                                                        probability_set = "109"),
                              .options = furrr_options(seed = NULL))) |> 
  unnest(cols = results) |>
  arrange(Group, SV) |>
  select(Group, SV, Mean:Sig.)

plan(sequential)

# Add significance indicator
table_across_sv <- table_across_sv |> 
  left_join(original_significance, by = join_by(SV == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(SV, "*"), SV)) |> 
  select(-significance_orig_paper)

# Loop through groups and create panels
for(the_group in 1:length(unique(table_across_sv$Group))) {
  # Subset full table
  the_table <- table_across_sv |> 
    filter(Group == unique(table_across_sv$Group)[the_group])
  
  # Compute mean
  the_table_mean <- the_table |> 
    summarize(Group = "",
              SV = "Mean",
              across(.cols = Mean:Sig., mean))
  
  ## No summary for single variable
  if(nrow(the_table) == 1) {the_table_mean <- NULL}
  
  # Add mean and remove group
  the_table <- the_table |> 
    bind_rows(the_table_mean) |> 
    select(-Group)
  
  # Print table
  ## Table name
  the_table_name <- paste0("IA09", 
                           letters[the_group], 
                           "_NSE_Q5_109_acrosssvs_", 
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
table_overall |> print_tex_table(file = "IA09i_NSE_Q5_109_acrosssvs_overall")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)
