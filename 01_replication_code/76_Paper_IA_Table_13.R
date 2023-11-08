
# Packages ---------------------------------------------------------
library(tidyverse)
library(moments)
library(RSQLite)
library(xtable)


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results_hxz <- dbReadTable(data_nse, "data_premium_results")  |> 
  filter(n_portfolios_main == 10) |>
  filter(sorting_method == "Single") |>
  filter(exchanges == "NYSE") |>
  filter(value_weighted == "VW")

data_premium_results_svv <- dbReadTable(data_nse, "data_premium_results")  |> 
  filter(n_portfolios_main == 10) |>
  filter(sorting_method == "Single") |>
  filter(drop_smallNYSE_at == 0.2) |>
  filter(exchanges == "NYSE") |>
  filter(value_weighted == "VW")

# Significance in original paper
original_significance <- dbReadTable(data_nse, "significance_orig_paper")


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

# Overall panel construction
compute_total_averages <- function(data) {
  sv_means <- data |> 
    group_by(SV) |> 
    summarize(Group = "Overall",
              significance_orig_paper = unique(significance_orig_paper),
              Mean = mean(mean),
              NSE = iqr(mean),
              Left = nse_test(mean, se, left_tail = TRUE)/n(),
              Right = nse_test(mean, se, left_tail = FALSE)/n(),
              .groups = 'drop') |>
    arrange(Group, SV) |>
    select(Group, SV, significance_orig_paper:Right)
  
  # Overall means
  mean_all <- sv_means |> 
    summarize(Group = "Overall",
              SV = "All",
              across(Mean:Right, ~ mean(.x)))
  
  # Sig means
  mean_sig <- sv_means |> 
    filter(significance_orig_paper == 1) |> 
    summarize(Group = "Overall",
              SV = "Orig. Sig.",
              across(Mean:Right, ~ mean(.x)))
  
  # Insig means
  mean_insig <- sv_means |> 
    filter(significance_orig_paper == 0) |> 
    summarize(Group = "Overall",
              SV = "Orig. Insig.",
              across(Mean:Right, ~ mean(.x)))
  
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
    } else if(text[[i]] == "NSE_hxz") {
      text[[i]] <- "\\multicolumn{1}{l}{$\\text{NSE}_\\text{HXZ}$}"
    } else if(text[[i]] == "Mean_hxz") {
      text[[i]] <- "\\multicolumn{1}{l}{$\\text{Mean}_\\text{HXZ}$}"
    } else if(text[[i]] == "NSE_svv") {
      text[[i]] <- "\\multicolumn{1}{l}{$\\text{NSE}_\\text{SVV}$}"
    } else if(text[[i]] == "Mean_svv") {
      text[[i]] <- "\\multicolumn{1}{l}{$\\text{Mean}_\\text{SVV}$}"
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
    mutate(Left_hxz = paste0("(",
                          formatC(Left_hxz, digits = 2, format = "f"),
                          ", ",
                          formatC(Right_hxz, digits = 2, format = "f"),
                          ")"), 
           Left_svv = paste0("(",
                             formatC(Left_svv, digits = 2, format = "f"),
                             ", ",
                             formatC(Right_svv, digits = 2, format = "f"),
                             ")")) |> 
    rename("$\\text{Left-right}_\\text{HXZ}$"= Left_hxz, 
           "$\\text{Left-right}_\\text{SVV}$"= Left_svv) |> 
    select(-Right_hxz, -Right_svv)
  
  
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
# Main panels' production for HXZ Forks 
table_across_sv_hxz <- data_premium_results_hxz |>
  mutate(mean = mean,
         se = se) |> 
  group_by(SV) |>
  drop_na(mean) |> 
  summarize(Group = unique(group),
            Mean = mean(mean),
            NSE = iqr(mean),
            Left = nse_test(mean, se, left_tail = TRUE)/n(),
            Right = nse_test(mean, se, left_tail = FALSE)/n(),
            .groups = 'drop') |>
  arrange(Group, SV) |>
  select(Group, SV, Mean:Right) |>
  rename(Mean_hxz = Mean, 
         NSE_hxz = NSE, 
         Left_hxz = Left, 
         Right_hxz = Right)

# Table production for SVV Forks 
table_across_sv_svv <- data_premium_results_svv |>
  mutate(mean = mean,
         se = se) |> 
  group_by(SV) |>
  drop_na(mean) |> 
  summarize(Group = unique(group),
            Mean = mean(mean),
            NSE = iqr(mean),
            Left = nse_test(mean, se, left_tail = TRUE)/n(),
            Right = nse_test(mean, se, left_tail = FALSE)/n(),
            .groups = 'drop') |>
  arrange(Group, SV) |>
  select(Group, SV, Mean:Right) |>
  rename(Mean_svv = Mean, 
         NSE_svv = NSE, 
         Left_svv = Left, 
         Right_svv = Right) |>
  select(-Group)

table_across_sv <- table_across_sv_hxz |>
  left_join(table_across_sv_svv, by = c("SV"))

rm(table_across_sv_hxz, table_across_sv_svv)
  
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
              across(.cols = Mean_hxz:Right_svv, mean))
  
  ## No summary for single variable
  if(nrow(the_table) == 1) {the_table_mean <- NULL}
  
  # Add mean and remove group
  the_table <- the_table |> 
    bind_rows(the_table_mean) |> 
    select(-Group)
  
  # Print table
  ## Table name
  the_table_name <- paste0("IA13", 
                           letters[the_group], 
                           "_NSE_acrosssvs_hxz_svv_", 
                           str_to_lower(unique(table_across_sv$Group)[the_group]))

  ## Final print
  the_table |> print_tex_table(file = the_table_name)

}

# Overall panel's construction -------------------------------------
table_overall_hxz <- data_premium_results_hxz |>
  left_join(original_significance, by = join_by(sorting_variable == sv)) |> 
  mutate(mean = mean,
         se = se) |> 
  drop_na(mean) |> 
  compute_total_averages() |>
  rename(Mean_hxz = Mean, 
         NSE_hxz = NSE, 
         Left_hxz = Left, 
         Right_hxz = Right) 
  
table_overall_svv <- data_premium_results_svv |>
  left_join(original_significance, by = join_by(sorting_variable == sv)) |> 
  mutate(mean = mean,
         se = se) |> 
  drop_na(mean) |> 
  compute_total_averages() |>
  rename(Mean_svv = Mean, 
         NSE_svv = NSE, 
         Left_svv = Left, 
         Right_svv = Right) 


table_overall <- table_overall_hxz |>
  left_join(table_overall_svv, by = c("SV"))

rm(table_overall_hxz, table_overall_svv)

# Print table
table_overall |> print_tex_table(file = "IA13i_NSE_acrosssvs_hxz_svv_overall")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)