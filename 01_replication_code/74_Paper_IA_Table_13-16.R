
# Analysis of Mean Absolute Differences over time

# Packages -------------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(broom)
library(lmtest)
library(sandwich)
library(xtable)


# Set up ---------------------------------------------------------------------
# SQLite database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Access TS database
data_nse_TS <- dbConnect(SQLite(), 
                         "Data/data_nse_TS.sqlite", 
                         extended_types = TRUE)


# Load Data ------------------------------------------------------------------
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")
sentiment_data <- dbReadTable(data_nse, "sentiment_data") 
nber_recession <- dbReadTable(data_nse, "nber_recession") 
cboe_data <- dbReadTable(data_nse, "cboe_data")
liquidity_data <- dbReadTable(data_nse, "liquidity_data")

# TS data
mad_ts_data <- dbReadTable(data_nse_TS, "data_MAD_TS_results")

# Mapping nodes + ID
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes") |> 
  rowid_to_column()

# Sorting groups
sv_groups <- dbReadTable(data_nse, "sv_groups") |> 
  rename(sorting_variable = sv)


# Standardization function ----------------------------------------------
standardize <- function(data) {
  return((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE))
}


# Monthly dispersion measure --------------------------------------------
# CRSP
economic_measures <- crsp_monthly |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  group_by(month) |>
  summarise(dispersion_sd = sd(ret, na.rm = TRUE),
            dispersion = quantile(ret, probs = c(0.95), na.rm = TRUE) - quantile(ret, probs = c(0.05), na.rm = TRUE), 
            dispersion_percent = dispersion * 100,
            stock_num = n() / 1000,
            .groups = 'drop') |>
  select(month, dispersion, dispersion_percent, stock_num, dispersion_sd) |>
  left_join(cboe_data, by = "month") |>
  left_join(liquidity_data, by = "month") |>
  left_join(nber_recession, by = "month") |>
  left_join(sentiment_data, by = "month") |>
  mutate(vix_std = standardize(vix)) 


# MADs over time (by decision node) -------------------------------------
# Filter data
mad_ts_data <- mad_ts_data |>
  filter(!(sorting_variable %in% c("sv_rmom", "sv_rev", "sv_csi", "sv_cfv", 
                                   "sv_eprd", "sv_beta", "sv_bfp") & node == "drop_stock_age_at")) |>  
  filter(obs_R > 100) |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |> 
  select(!starts_with("obs"))
  
# Rescale
mad_ts_data <- mad_ts_data |> 
  mutate(across(starts_with("diff_avg"), ~ . * 100))

# Join other data
mad_ts_data <- mad_ts_data |> 
  left_join(economic_measures, by = "month") |>
  left_join(sv_groups, by = "sorting_variable")

# Set up panel structure
mad_ts_data <- mad_ts_data |> 
  mutate(dummy_mom = if_else(group == "Momentum", 1, 0),
         dummy_siz = if_else(group == "Size", 1, 0),
         dummy_val = if_else(group == "Valuation", 1, 0),
         dummy_inv = if_else(group == "Investment", 1, 0),
         dummy_fin = if_else(group == "Financing", 1, 0),
         dummy_pro = if_else(group == "Profitability", 1, 0),
         dummy_int = if_else(group == "Intangibles", 1, 0),
         dummy_tra = if_else(group == "Trading frictions", 1, 0),
         year = year(month)) |> 
  arrange(month, node, sorting_variable)

# Calculate residuals relative to dispersion 
mad_ts_data <- mad_ts_data |> 
  group_by(node) |> 
  mutate(across(starts_with("diff_avg"), 
                ~ lm(. ~ dispersion_sd + 
                       dummy_mom + 
                       dummy_siz + 
                       dummy_val + 
                       dummy_inv + 
                       dummy_fin + 
                       dummy_pro + 
                       dummy_int + 
                       dummy_tra) |> residuals(), 
                .names = "residual_{.col}")) |> 
  ungroup()

# Standardize MAD's
mad_ts_data <- mad_ts_data |> 
  group_by(node) |> 
  mutate(across(residual_diff_avg_R:residual_diff_avg_Q, ~ standardize(.), .names = "{.col}_std")) |> 
  ungroup()


# Functions -------------------------------------------------------------
panel_reg <- function(data, node_level, dep_var, ind_var) {
  # Escape errors
  if(node_level == "formation_time") {
    if(str_split_i(ind_var[1], "_", -2) == "mom") return(tibble(estimate = NA_character_, statistic = NA_character_))
    if(str_split_i(ind_var[1], "_", -2) == "siz") return(tibble(estimate = NA_character_, statistic = NA_character_))
    if(str_split_i(ind_var[1], "_", -2) == "tra") return(tibble(estimate = NA_character_, statistic = NA_character_))
  }
  
  # Specify formula
  formula_in <- formula(paste0(dep_var, " ~ ", paste0(ind_var, collapse = " + ")))
  
  # Run regression
  reg_results <- lm(formula_in, data = data)

  # Output regression
  coeftest(x = reg_results, 
           vcov = vcovHC(reg_results, type = "HC0",cluster = "year")) |> 
    tidy() |> 
    filter(term == ind_var[1]) |> 
    select(estimate, statistic) |>
    mutate(estimate = format(round(estimate, digits = 2), nsmall = 2), 
           estimate = ifelse(abs(statistic) >= qnorm(0.995), 
                             paste0(estimate, "^{***}"), 
                             estimate),
           estimate = ifelse(abs(statistic) >= qnorm(0.975) & abs(statistic) < qnorm(0.995), 
                             paste0(estimate, "^{**}"), 
                             estimate), 
           estimate = ifelse(abs(statistic) >= qnorm(0.95) & abs(statistic) < qnorm(0.975), 
                             paste0(estimate, "^{*}"), 
                             estimate),
           statistic = format(round(statistic, digits = 2), nsmall = 2),
           statistic = paste0("(",statistic, paste0(")", collapse = "")))
}

# Function for columnnames
wrap_columnnames <- function(text) {
  for(i in 1:length(text)) {
    if(text[[i]] %in% c("Node")) {
      next
    } else {
      text[[i]] <- paste0("\\multicolumn{1}{c}{", text[[i]], "}") 
    }
  }
  
  return(text)
}

# General printing function
print_tex_table <- function(data, file = NA, add.to.row = NULL, include.colnames = FALSE, booktabs = TRUE) {
  if(is.na(file)) {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = include.colnames,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = booktabs,
          sanitize.text.function = function(x) return(x),
          sanitize.colnames.function = wrap_columnnames,
          hline.after = c(-1, 0, nrow(data)),
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
          sanitize.text.function = function(x) return(x),
          sanitize.colnames.function = wrap_columnnames,
          hline.after = c(-1, 0, nrow(data)),
          add.to.row = add.to.row)
  }
}

# Specific table consturction
produce_table <- function(data, file = NA) {
  # Paste all
  ## First
  ### Column name
  col_name <- str_split_i(colnames(data)[2], "_", 2) |> 
    paste0(".") |> str_to_title()
  
  ### Table
  the_table <- data |> 
    select(node, colnames(data)[2]) |> 
    unnest(cols = !node, names_sep = "_") |> 
    pivot_longer(cols = !node, values_to = col_name, names_to = "names") |> 
    mutate(names = str_split_i(names, "_", -1))
  
  # Rest
  for(j in 3:ncol(data)) {
    # Column name
    col_name <- str_split_i(colnames(data)[j], "_", 2) |> 
      paste0(".") |> str_to_title()
    
    # Table
    new <- data |> 
      select(node, colnames(data)[j]) |> 
      unnest(cols = !node, names_sep = "_") |> 
      pivot_longer(cols = !node, values_to = col_name, names_to = "names") |> 
      mutate(names = str_split_i(names, "_", -1))
    
    # Merge
    the_table <- the_table |> 
      left_join(new, by = c("node", "names"))
  }
  
  # Finalize
  data_all <- the_table |> 
    inner_join(mapping_nodes, by = "node") |> 
    arrange(rowid, names) |> 
    mutate(node_name = if_else(names == "statistic", "", node_name)) |> 
    select(ANode = node_name, ends_with(".")) 
  
  # Rearrange
  data_all <- data_all |> 
    select(sort(colnames(data_all))) |> 
    rename(Node = ANode)
  
  # Print table
  data_all |> 
    print_tex_table(booktabs = TRUE,
                    include.colnames = TRUE,
                    file = file)
}


# VIX Panel -------------------------------------------------------------
# Panel regression
reg_VIX <- mad_ts_data |>
  mutate(across(starts_with("dummy_"), 
                ~ . * vix_std,
                .names = "{.col}_inter"))  |> 
  group_by(node) |> 
  summarise(node = unique(node),
            across(.cols = ends_with("_inter"), 
                   ~ panel_reg(data = pick(everything()), 
                               node_level = cur_group()$node,
                               dep_var = "residual_diff_avg_R_std",
                               ind_var = c(cur_column(), "vix_std",
                                           "dummy_mom", "dummy_siz", "dummy_val", "dummy_inv", 
                                           "dummy_fin", "dummy_pro", "dummy_int", "dummy_tra")),
                   .names = "{.col}_reg"))

# Table output
reg_VIX |>
  produce_table("Paper_Tables/IA13_mad_TS_interact_VIX.tex")


# NBER Panel ------------------------------------------------------------
# Panel regression
reg_NBER <- mad_ts_data |>
  mutate(across(starts_with("dummy_"), 
                ~ . * rec_indicator,
                .names = "{.col}_inter"))  |> 
  group_by(node) |> 
  summarise(node = unique(node),
            across(.cols = ends_with("_inter"), 
                   ~ panel_reg(data = pick(everything()), 
                               node_level = cur_group()$node,
                               dep_var = "residual_diff_avg_R_std",
                               ind_var = c(cur_column(), "rec_indicator",
                                           "dummy_mom", "dummy_siz", "dummy_val", "dummy_inv", 
                                           "dummy_fin", "dummy_pro", "dummy_int", "dummy_tra")),
                   .names = "{.col}_reg"))

# Table output
reg_NBER |>
  produce_table("Paper_Tables/IA14_mad_TS_interact_NBER.tex")


# Liquidity Panel -------------------------------------------------------
# Panel regression
reg_liquidity <- mad_ts_data |>
  mutate(across(starts_with("dummy_"), 
                ~ . * liquidity_std,
                .names = "{.col}_inter"))  |> 
  group_by(node) |> 
  summarise(node = unique(node),
            across(.cols = ends_with("_inter"), 
                   ~ panel_reg(data = pick(everything()), 
                               node_level = cur_group()$node,
                               dep_var = "residual_diff_avg_R_std",
                               ind_var = c(cur_column(), "liquidity_std",
                                           "dummy_mom", "dummy_siz", "dummy_val", "dummy_inv", 
                                           "dummy_fin", "dummy_pro", "dummy_int", "dummy_tra")),
                   .names = "{.col}_reg"))

# Table output
reg_liquidity |>
  produce_table("Paper_Tables/IA15_mad_TS_interact_liquidity.tex")


# Sentiment Panel -------------------------------------------------------
# Panel regression
reg_sentiment <- mad_ts_data |>
  mutate(across(starts_with("dummy_"), 
                ~ . * sent_std,
                .names = "{.col}_inter"))  |> 
  group_by(node) |> 
  summarise(node = unique(node),
            across(.cols = ends_with("_inter"), 
                   ~ panel_reg(data = pick(everything()), 
                               node_level = cur_group()$node,
                               dep_var = "residual_diff_avg_R_std",
                               ind_var = c(cur_column(), "sent_std",
                                           "dummy_mom", "dummy_siz", "dummy_val", "dummy_inv", 
                                           "dummy_fin", "dummy_pro", "dummy_int", "dummy_tra")),
                   .names = "{.col}_reg"))

# Table output
reg_sentiment |>
  produce_table("Paper_Tables/IA16_mad_TS_interact_sentiment.tex")


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)