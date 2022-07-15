
# Packages ----------------------------------------------------------------
library(tidyverse)
library(moments)
library(DBI)
library(RSQLite)
library(xtable)

# Data --------------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")

# Node Text 
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")


# Table function ----------------------------------------------------------
# Function for columnnames
wrap_columnnames <- function(text) {
  for(i in 1:length(text)) {
    if(text[[i]] %in% c("Node", "Group", "SV", "Branch", mapping_nodes$node_name)) {
      next
    } else {
      text[[i]] <- paste0("\\multicolumn{1}{c}{", text[[i]], "}") 
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
  if(decision_node %in% c("formation_time", "sv_lag")) {
    data <- data %>%
      filter(sorting_variable != "sv_csi",
             group != "Momentum",
             group != "Size",
             group != "Trading Frictions")
  }
  
  ## Stock age
  if(decision_node %in% c("drop_stock_age_at")) {
    data <- data %>%
      filter(sorting_variable != "sv_csi")
  }
  
  ## Secondary portfolio
  if(decision_node %in% c("n_portfolios_secondary")) {
    data <- data %>%
      filter(!is.na(n_portfolios_secondary))  %>%
      filter(!(sorting_variable == "sv_size"))
  }
  
  ## Double sorting
  if(decision_node %in% c("sorting_method")) {
    data <- data %>%
      filter(!(sorting_variable == "sv_size"))
  }
  
  ## Size restriction
  if(decision_node %in% c("drop_smallNYSE_at")) {
    data <- data %>%
      filter(drop_smallNYSE_at %in% c(0, 0.2))
  }
  
  # Compute summary statistics
  data %>%
    drop_na({{ mean_spec }}) %>% 
    mutate(mean = {{ mean_spec }},
           se = {{ se_spec }},
           t = {{ t_spec }}) %>% 
    group_by(across(all_of(decision_node)), SV) %>%
    summarize(Mean = mean(mean),
              NSE = sd(mean),
              ASE = mean(se),
              Ratio = NSE/ASE,
              Skew. = skewness(mean),
              Kurt. = kurtosis(mean),
              Pos. = sum(mean > 0)/n(),
              Sig. = sum(t > qnorm(0.975))/n(),
              .groups = 'drop') %>%
    group_by(across(all_of(decision_node))) %>%
    summarize(across(Mean:Sig., ~ mean(.)),
              .groups = 'drop') %>%
    mutate(node = mapping_nodes %>% filter(node == decision_node) %>% pull(node_name),
           Branch = get(decision_node)) %>%
    mutate(Branch = as.character(Branch)) %>%
    select(node, Branch, Mean:Sig.)
}

print_tex_table_nodes <- function(table, file = NA) {
  # Remove groups
  table_all <- table %>%
    select(-node)
  
  # Additional lines
  col_headline <- "Branch & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{NSE} & \\multicolumn{1}{c}{ASE} & \\multicolumn{1}{c}{Ratio} & \\multicolumn{1}{c}{Skew.} & \\multicolumn{1}{c}{Kurt.} & \\multicolumn{1}{c}{Pos.} & \\multicolumn{1}{c}{Sig.}"
  the_groups_labels <- unique(table$node)
  the_groups_labels <- paste0("Panel ", LETTERS[1:length(the_groups_labels)], ": ", the_groups_labels)
  the_groups_labels <- paste0("\\\\ \\multicolumn{9}{l}{\\textbf{", the_groups_labels, "}} \\\\ ",
                              "\\toprule ",
                              col_headline, 
                              "\\\\ \\midrule ")
  the_groups_labels[2:length(the_groups_labels)] <- paste0("\\bottomrule ", the_groups_labels[2:length(the_groups_labels)])
  
  additional_layout <- list() 
  additional_layout$pos <- as.list(c(0, table %>% mutate(node = as_factor(node)) %>% group_by(node) %>% summarize(freq = n()) %>% mutate(freq = cumsum(freq)) %>% pull(freq)))
  additional_layout$command <- as.vector(the_groups_labels, mode = "character")
  additional_layout$command <- c(additional_layout$command, "\\bottomrule")
  
  table_all %>%
    print_tex_table(add.to.row = additional_layout, 
                    include.colnames = FALSE, 
                    booktabs = FALSE,
                    file = file)
}

# Table 15 --------------------------------------------------------------
# CAPM Table
# Define nodes for IA
IA_nodes <- mapping_nodes %>%
  slice(7:14) %>%
  pull(node)

# Loop for table production
## Tables
table_main <- tibble()
table_IA <- tibble()

## Main loop
for(the_variable in mapping_nodes$node) {
  # IA indicator
  ia_indicator <- the_variable %in% IA_nodes
  
  if(ia_indicator) { 
    table_IA <- table_IA %>%
      bind_rows(compute_summary_across_nodes(data = data_premium_results, 
                                             mean_spec = alpha_CAPM, 
                                             se_spec = se_CAPM,
                                             t_spec = t_CAPM,
                                             decision_node = the_variable))
  } else {
    table_main <- table_main %>%
      bind_rows(compute_summary_across_nodes(data = data_premium_results,  
                                             mean_spec = alpha_CAPM, 
                                             se_spec = se_CAPM,
                                             t_spec = t_CAPM,
                                             decision_node = the_variable))
  }
}

# Print tables
## Main table
table_main %>%
  print_tex_table_nodes(file = "Paper_Tables/15_NSE_nodes_CAPM.tex")

## Internet appendix table
table_IA %>%
  print_tex_table_nodes(file = "Paper_Tables/IA10_NSE_nodes_CAPM.tex")

# Table 16 --------------------------------------------------------------
# CAPM Table
# Define nodes for IA
IA_nodes <- mapping_nodes %>%
  slice(7:14) %>%
  pull(node)

# Loop for table production
## Tables
table_main <- tibble()
table_IA <- tibble()

## Main loop
for(the_variable in mapping_nodes$node) {
  # IA indicator
  ia_indicator <- the_variable %in% IA_nodes
  
  if(ia_indicator) { 
    table_IA <- table_IA %>%
      bind_rows(compute_summary_across_nodes(data = data_premium_results, 
                                             mean_spec = alpha_FF3, 
                                             se_spec = se_FF3,
                                             t_spec = t_FF3,
                                             decision_node = the_variable))
  } else {
    table_main <- table_main %>%
      bind_rows(compute_summary_across_nodes(data = data_premium_results,  
                                             mean_spec = alpha_FF3, 
                                             se_spec = se_FF3,
                                             t_spec = t_FF3,
                                             decision_node = the_variable))
  }
}

# Print tables
## Main table
table_main %>%
  print_tex_table_nodes(file = "Paper_Tables/IA11_NSE_nodes_FF3.tex")

## Internet appendix table
table_IA %>%
  print_tex_table_nodes(file = "Paper_Tables/IA12_NSE_nodes_FF3.tex")
