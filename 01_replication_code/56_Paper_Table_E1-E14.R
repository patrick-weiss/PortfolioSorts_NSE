
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
data_premium_results <- dbReadTable(data_nse, "data_premium_results")

# Node Text 
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")

mapping_nodes <- mapping_nodes |> 
  arrange(node_order)

# Table function ---------------------------------------------------
# IQR
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

# Function for columnnames
wrap_columnnames <- function(text) {
  for(i in 1:length(text)) {
    if(text[[i]] %in% c("Node", "Group", "SV", "Branch", mapping_nodes$node_name)) {
      next
    } else {
      text[[i]] <- paste0("\\multicolumn{1}{l}{", text[[i]], "}") 
    }
  }
  
  return(text)
}

print_tex_table <- function(data, file = NA, add.to.row = NULL, include.colnames = TRUE, booktabs = TRUE) {
  if(is.na(file)) {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = include.colnames,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = booktabs,
          hline.after = NULL,
          sanitize.colnames.function = wrap_columnnames,
          add.to.row = add.to.row)
  } else {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = include.colnames,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = file,
          booktabs = booktabs,
          hline.after = NULL,
          sanitize.colnames.function = wrap_columnnames,
          add.to.row = add.to.row)
  }
}

compute_summary_across_nodes <- function(data, 
                                         decision_node,
                                         mean_spec = mean, 
                                         se_spec = se,
                                         t_spec = t) {
  
  # Filter for nodes where certain sv are excluded
  ## Formation time and sv lag
  if(decision_node %in% c("formation_time")) {
    data <- data |>
      group_by(sorting_variable) |> 
      mutate(nodes = length(unique(formation_time))) |> 
      ungroup() |> 
      filter(nodes > 1) |> 
      select(-nodes)
  }
  
  ## Stock age
  if(decision_node %in% c("drop_stock_age_at")) {
    data <- data |>
      filter(!(sorting_variable %in% c("sv_rmom", "sv_rev", "sv_csi", "sv_cfv", 
                                       "sv_eprd", "sv_beta", "sv_bfp")))
  }
  
  ## Size restriction
  if(decision_node %in% c("drop_smallNYSE_at")) {
    data <- data |>
      filter(drop_smallNYSE_at %in% c(0, 0.2))
  }
  
  ## Secondary portfolio
  if(decision_node %in% c("n_portfolios_secondary")) {
    data <- data |>
      filter(!is.na(n_portfolios_secondary))
  }
  
  # Compute summary statistics
  sum_stats <- data |>
    drop_na({{ mean_spec }}) |> 
    mutate(mean = {{ mean_spec }},
           se = {{ se_spec }},
           t = {{ t_spec }}) |> 
    group_by(across(all_of(decision_node)), SV)
    
  # Decision node: n_portfolios_main?
  if(decision_node == "n_portfolios_main") {
    ## Yes n_portfolios_main
    sum_stats <- sum_stats |> 
      summarize(Group = unique(group),
                Mean = mean(mean),
                NSE = iqr(mean),
                Left = nse_test(mean, se, left_tail = TRUE)/n(),
                Right = nse_test(mean, se, left_tail = FALSE)/n(),
                Ratio = sd(mean)/mean(se),
                Skew. = skewness(mean),
                Kurt. = kurtosis(mean),
                Pos. = sum(mean > 0)/n(),
                Sig. = sum(t > qnorm(0.975))/n(),
                Mon. = sum(mono_all < 0.10)/sum(!is.na(mono_all)),
                .groups = 'drop')
  } else {
    ## Not n_portfolios_main
    sum_stats <- sum_stats |> 
      summarize(Group = unique(group),
                Mean = mean(mean),
                NSE = iqr(mean),
                Left = nse_test(mean, se, left_tail = TRUE)/n(),
                Right = nse_test(mean, se, left_tail = FALSE)/n(),
                Ratio = sd(mean)/mean(se),
                Skew. = skewness(mean),
                Kurt. = kurtosis(mean),
                Pos. = sum(mean > 0)/n(),
                Sig. = sum(t > qnorm(0.975))/n(),
                Mon. = sum(mono_all[n_portfolios_main == 5] < 0.10)/sum(!is.na(mono_all[n_portfolios_main == 5])),
                .groups = 'drop')
  }
  
  # Final output
  sum_stats |>
    group_by(Group, across(all_of(decision_node))) |>
    summarize(across(Mean:Mon., ~ mean(.)),
              .groups = 'drop') |>
    mutate(Branch = get(decision_node)) |>
    mutate(Branch = as.character(Branch)) |>
    select(Group, Branch, Mean:Mon.) |>
    mutate(Branch = ifelse(Branch == "Yes" & decision_node == "drop_earnings", "Excluded", Branch), 
           Branch = ifelse(Branch == "No" & decision_node == "drop_earnings", "Included", Branch), 
           Branch = ifelse(Branch == "Yes" & decision_node == "drop_bookequity", "Excluded", Branch), 
           Branch = ifelse(Branch == "No" & decision_node == "drop_bookequity", "Included", Branch)) 
}

print_tex_table_nodes <- function(table, file = NA) {
  # Remove groups
  table_all <- table |>
    select(-Group)
  
  # Merge columns
  table_all <- table_all |> 
    rename(Tests = Left) |> 
    mutate(Tests = paste0("(",
                          formatC(Tests, digits = 2, format = "f"),
                          ", ",
                          formatC(Right, digits = 2, format = "f"),
                          ")")) |> 
    select(-Right)

  # Additional lines
  col_headline <- "Branch & \\multicolumn{1}{l}{Mean} & \\multicolumn{1}{l}{NSE} & \\multicolumn{1}{l}{Left-right} & \\multicolumn{1}{l}{Ratio} & \\multicolumn{1}{l}{Skew.} & \\multicolumn{1}{l}{Kurt.} & \\multicolumn{1}{l}{Pos.} & \\multicolumn{1}{l}{Sig.} & \\multicolumn{1}{l}{Mon.}"
  the_groups_labels <- unique(table$Group)
  the_groups_labels <- paste0("Panel ", LETTERS[1:length(the_groups_labels)], ": ", the_groups_labels)
  the_groups_labels <- paste0("\\\\[-6px] \n \\multicolumn{10}{l}{\\textbf{", the_groups_labels, "}}\\Tstrut\\Bstrut\\\\[6px] \n ",
                              "\\toprule \n ",
                              col_headline, 
                              "\\\\ \\midrule \n ")
  the_groups_labels[2:length(the_groups_labels)] <- paste0("\\bottomrule \n ", the_groups_labels[2:length(the_groups_labels)])
  
  additional_layout <- list() 
  additional_layout$pos <- as.list(c(0, table |> mutate(node = as_factor(Group)) |> group_by(node) |> summarize(freq = n()) |> mutate(freq = cumsum(freq)) |> pull(freq)))
  #additional_layout$pos <- as.list(seq(0, nrow(table_all) - nrow(unique(table_all[,1])), by = nrow(unique(table_all[,1]))))
  additional_layout$command <- as.vector(the_groups_labels, mode = "character")
  additional_layout$command <- c(additional_layout$command, "\\bottomrule")
  
  table_all |>
    print_tex_table(add.to.row = additional_layout, 
                    include.colnames = FALSE, 
                    booktabs = FALSE,
                    file = file)
}


# Table E1-14 ------------------------------------------------------
# Define nodes for IA
IA_nodes <- NA

# Loop for table production
## counter
the_variable_counter <- 1
the_variable_IA_counter <- 4

## Actual loop
for(the_variable in mapping_nodes$node) {
  # IA indicator
  ia_indicator <- the_variable %in% IA_nodes
  
  # Table name
  table_name <- paste0("Paper_Tables/", ifelse(ia_indicator, "IA", "E"),
                       ifelse(ia_indicator, formatC(the_variable_IA_counter, width = 2, flag = "0"), ""),
                       ifelse(!ia_indicator, formatC(the_variable_counter, width = 2, flag = "0"), ""),
                       "_NSE_nodes_",
                       the_variable,".tex")
  
  # Table production
  data_premium_results |>
    compute_summary_across_nodes(decision_node = the_variable) |> 
    print_tex_table_nodes(file = table_name)

  # Increase counter
  if(ia_indicator) {
    the_variable_IA_counter <- the_variable_IA_counter + 1 
  } else {
    the_variable_counter <- the_variable_counter + 1
  }
}


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)