
# Packages ---------------------------------------------------------
library(tidyverse)
library(moments)
library(RSQLite)
library(xtable)
library(DescTools)


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")

# Significance in original paper
original_significance <- dbReadTable(data_nse, "significance_orig_paper")

# Probabilities 
probability_grid <- dbReadTable(data_nse, "probability_grid")


# Table function ---------------------------------------------------
# IQR functions
## Standard
iqr <- function(premiums, quantiles = 0.25) {
  quantile(premiums, 1 - quantiles) - quantile(premiums, quantiles) |> 
    as.numeric()
}

## Weighted
iqr_weighted <- function(premiums, weights, quantiles = 0.25) {
  # Left
  left_quantile <- Quantile(x = premiums, 
                            weights = weights*10^7, 
                            probs = 1 - quantiles) |> 
    as.numeric()
  
  # Right
  right_quantile <- Quantile(x = premiums, 
                             weights = weights*10^7, 
                             probs = quantiles) |> 
    as.numeric()
  
  return(left_quantile - right_quantile)
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

# Overall panel construction
compute_total_averages <- function(data) {
  sv_means <- data |> 
    group_by(SV) |> 
    summarize(Group = "Overall",
              significance_orig_paper = unique(significance_orig_paper),
              Mean = mean(mean),
              NSE = iqr(mean),
              NSE_109 = iqr_weighted(mean, probability_109),
              Left = nse_test(mean, se, left_tail = TRUE)/n(),
              Right = nse_test(mean, se, left_tail = FALSE)/n(),
              Ratio = sd(mean)/mean(se),
              Skew. = skewness(mean),
              Kurt. = kurtosis(mean),
              Pos. = sum(mean > 0)/n(),
              Sig. = sum(t > qnorm(0.975))/n(),
              .groups = 'drop') |>
    arrange(Group, SV) |>
    select(Group, SV, significance_orig_paper:Sig.)
  
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
    } else if(text[[i]] == "NSE_56") {
      text[[i]] <- "\\multicolumn{1}{l}{$\\text{NSE}_\\text{56}$}"
    } else if(text[[i]] == "NSE_109") {
      text[[i]] <- "\\multicolumn{1}{l}{$\\text{NSE}_\\text{w}$}"
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


# Table 2 ----------------------------------------------------------
# Add probabilities to results
data_premium_results <- data_premium_results |> 
  full_join(probability_grid |> 
              select(-sorting_variable), 
            by = join_by(ID == ID))

## Sanity check
stopifnot("NA probabilities" = all(!is.na(data_premium_results$probability_56)))

# Main panels' production
table_across_sv <- data_premium_results |>
  mutate(mean = log_mean * 100,
         se = log_se * 100,
         t = log_t) |> 
  group_by(SV) |>
  drop_na(mean) |> 
  summarize(Group = unique(group),
            Mean = mean(mean),
            NSE = iqr(mean),
            NSE_109 = iqr_weighted(mean, probability_109),
            Left = nse_test(mean, se, left_tail = TRUE)/n(),
            Right = nse_test(mean, se, left_tail = FALSE)/n(),
            Ratio = sd(mean)/mean(se),
            Skew. = skewness(mean),
            Kurt. = kurtosis(mean),
            Pos. = sum(mean > 0)/n(),
            Sig. = sum(t > qnorm(0.975))/n(),
            .groups = 'drop') |>
  arrange(Group, SV) |>
  select(Group, SV, Mean:Sig.)

# Add significance indicator
table_across_sv <- table_across_sv |> 
  mutate(sorting_variable = paste0("sv_", str_to_lower(SV))) |> 
  left_join(original_significance, by = join_by(sorting_variable == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(SV, "*"), SV)) |> 
  select(-significance_orig_paper, -sorting_variable)

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
  the_table_name <- paste0("IA18", 
                           letters[the_group], 
                           "_NSE_log_acrosssvs_", 
                           str_to_lower(unique(table_across_sv$Group)[the_group]))
  
  ## Final print
  the_table |> print_tex_table(file = the_table_name)
}


# Overall panel's construction -------------------------------------
table_overall <- data_premium_results |>
  left_join(original_significance, by = join_by(sorting_variable == sv)) |> 
  mutate(mean = log_mean * 100,
         se = log_se * 100,
         t = log_t) |> 
  drop_na(mean) |> 
  compute_total_averages()

# Print table
table_overall |> print_tex_table(file = "IA18i_NSE_log_acrosssvs_overall")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)