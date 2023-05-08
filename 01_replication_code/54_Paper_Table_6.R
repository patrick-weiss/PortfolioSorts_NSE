# Analysis of Mean Absolute Differences over time

# Packages --------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(broom)
library(lmtest)
library(sandwich)
library(xtable)


# Set up ----------------------------------------------------------------
# SQLite database
data_nse <- dbConnect(SQLite(), 
            "Data/data_nse.sqlite", 
            extended_types = TRUE)


# Load Data -------------------------------------------------------------
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")
sentiment_data <- dbReadTable(data_nse, "sentiment_data") 
nber_recession <- dbReadTable(data_nse, "nber_recession") 
cboe_data <- dbReadTable(data_nse, "cboe_data")
liquidity_data <- dbReadTable(data_nse, "liquidity_data")
mad_ts_data <- dbReadTable(data_nse, "data_TS_timeseries_all")

# Mapping nodes + ID
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes") |> 
  rowid_to_column()


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
# Aggregate over all sorting variables 
mad_ts_data <- mad_ts_data |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  left_join(economic_measures, by = "month")

# Calculate residuals relative to dispersion 
mad_ts_data <- mad_ts_data |> 
  group_by(node) |> 
  mutate(across(mad_R:mad_Q, ~ lm(. ~ dispersion_sd) |> residuals(), 
                .names = "residual_{.col}")) |> 
  ungroup()

# Standardize residuals for each node 
mad_ts_data <- mad_ts_data |>
  group_by(node) |> 
  mutate(across(residual_mad_R:residual_mad_Q, ~ standardize(.), .names = "{.col}_std")) |> 
  ungroup()


# Functions -------------------------------------------------------------
# Regression
reg_function <- function(dependent, independent) {
  lm(dependent ~ independent) |> 
    coeftest(x = _, vcov = NeweyWest) |> 
    tidy() |> 
    filter(term == "independent") |> 
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
  # VIX
  data_vix <- data |> 
    select(node, contains("vix_std")) |> 
    pivot_longer(cols = !node, values_to = "VIX", names_to = "names") |> 
    mutate(names = str_split_i(names, "_", -1))
  
  # NBER
  data_nber <- data |> 
    select(node, contains("rec_indicator")) |> 
    pivot_longer(cols = !node, values_to = "NBER", names_to = "names") |> 
    mutate(names = str_split_i(names, "_", -1))
    
  # Liquidity
  data_liquidity <- data |> 
    select(node, contains("liquidity_std")) |> 
    pivot_longer(cols = !node, values_to = "Liquidity", names_to = "names") |> 
    mutate(names = str_split_i(names, "_", -1))
  
  # Sentiment
  data_sentiment <- data |> 
    select(node, contains("sent_std")) |> 
    pivot_longer(cols = !node, values_to = "Sentiment", names_to = "names") |> 
    mutate(names = str_split_i(names, "_", -1))
  
  # Combine
  data_all <- data_vix |> 
    full_join(data_nber, by = c("node", "names")) |> 
    full_join(data_liquidity, by = c("node", "names")) |> 
    full_join(data_sentiment, by = c("node", "names")) |> 
    inner_join(mapping_nodes, by = "node") |> 
    arrange(rowid, names) |> 
    mutate(node_name = if_else(names == "statistic", "", node_name)) |> 
    select(Node = node_name, VIX, NBER, Liquidity, Sentiment)
  
  # Print table
  data_all |> 
    print_tex_table(booktabs = TRUE,
                    include.colnames = TRUE,
                    file = file)
}


# Generate Latex output -------------------------------------------------
# Produce plot
table_residual_mad_R_std <- mad_ts_data |>
  group_by(node) |> 
  summarise(across(.cols = c(vix_std, rec_indicator, sent_std, liquidity_std),
                   ~ reg_function(dependent = residual_mad_R_std, independent = .),
                   .names = "R_residual_std_{.col}")) |> 
  unnest(cols = !node, names_sep = "_") 

# Print plot
table_residual_mad_R_std |>
  produce_table(file = "Paper_Tables/06_mad_TS_regression.tex")


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)