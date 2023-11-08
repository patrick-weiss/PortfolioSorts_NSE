
# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(broom)
library(lmtest)
library(sandwich)
library(xtable)
library(moments)


# Set up -----------------------------------------------------------
# SQLite database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Load Data --------------------------------------------------------
# CRSP data
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")

# Macro data
nber_recession <- dbReadTable(data_nse, "nber_recession") 
cboe_data <- dbReadTable(data_nse, "cboe_data")
liquidity_data <- dbReadTable(data_nse, "liquidity_data")

# MADs
mad_ts_data <- dbReadTable(data_nse, "data_TS_timeseries_all")

# Mapping nodes + ID
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes") |> 
  rowid_to_column()


# Standardization function -----------------------------------------
standardize <- function(data) {
  return((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE))
}


# Monthly dispersion measure ---------------------------------------
# CRSP distperion
economic_measures <- crsp_monthly |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  group_by(month) |>
  summarize(dispersion = mean(abs(ret - mean(ret, na.rm = TRUE))), 
            skew = skewness(ret, na.rm = TRUE),
            kurtos = kurtosis(ret, na.rm = TRUE),
            stock_num = n() / 1000,
            .groups = 'drop')

# Add macro data
economic_measures <- economic_measures |> 
  left_join(cboe_data, by = "month") |>
  left_join(liquidity_data, by = "month") |>
  left_join(nber_recession, by = "month") |>
  mutate(vix_std = standardize(vix), 
         dispersion_std = standardize(dispersion), 
         skew_std = standardize(skew), 
         kurtosis_std = standardize(kurtos)) 


# MADs over time (by decision node) --------------------------------
# Aggregate over all sorting variables 
mad_ts_data <- mad_ts_data |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  left_join(economic_measures, by = "month")

# Standardize MADs for each node 
mad_ts_data <- mad_ts_data |>
  group_by(node) |> 
  mutate(across(mad_R:mad_Q, ~ standardize(.), .names = "{.col}_std")) |> 
  ungroup()


# Functions --------------------------------------------------------
# Regression function 1 
reg_function1 <- function(dependent, independent) {
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

# Regression function 2 
reg_function2 <- function(dependent, independent, independent2) {
  lm(dependent ~ independent + independent2) |> 
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
    if(text[[i]] %in% c("Fork")) {
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
          include.colnames = TRUE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = booktabs,
          sanitize.colnames.function = wrap_columnnames,
          sanitize.text.function = function(x) x,
          hline.after = c(-1, 0, nrow(data)),
          add.to.row = add.to.row)
  } else {
    print(xtable(data),
          include.rownames = FALSE,
          include.colnames = TRUE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = file,
          booktabs = booktabs,
          sanitize.colnames.function = wrap_columnnames,
          sanitize.text.function = function(x) x,
          hline.after = c(-1, 0, nrow(data)),
          add.to.row = add.to.row)
  }
}

# Specific table consturction
produce_table <- function(data) {
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
  
  # Combine
  data_all <- data_vix |> 
    full_join(data_nber, by = c("node", "names")) |> 
    full_join(data_liquidity, by = c("node", "names")) |> 
    inner_join(mapping_nodes, by = "node") |> 
    mutate(node_name = if_else(names == "statistic", "", node_name)) |> 
    select(Node = node_name, VIX, NBER, Liquidity)
  
  return(data_all)
}


# Generate Latex output -------------------------------------------------
# Produce table part without controls 
table_residual_mad_R_std_without <- mad_ts_data |>
  group_by(node) |> 
  summarise(across(.cols = c(vix_std, rec_indicator, liquidity_std),
                   ~ reg_function1(dependent = mad_R_std, independent = .),
                   .names = "R_residual_std_{.col}")) |> 
  unnest(cols = !node, names_sep = "_") |>
  left_join(mapping_nodes, by = c("node")) |>
  select(-node_name, -node_order) |>
  arrange(node_order_mad) |>
  select(-node_order_mad) 

# Produce table part with controls 
table_residual_mad_R_std <- mad_ts_data |>
  group_by(node) |> 
  summarise(across(.cols = c(vix_std, rec_indicator, liquidity_std),
                   ~ reg_function2(dependent = mad_R_std, independent = ., independent2 = dispersion_std),
                   .names = "R_residual_std_{.col}")) |> 
  unnest(cols = !node, names_sep = "_") |>
  left_join(mapping_nodes, by = c("node")) |>
  select(-node_name, -node_order) |>
  arrange(node_order_mad) |>
  select(-node_order_mad) 

# Produce the average R-squared across all forks 
average_r2 <- mad_ts_data |> 
  group_by(node) |> 
  summarise(model_vix = list(lm(mad_R_std ~ vix_std)),
            model_vix_control = list(lm(mad_R_std ~ vix_std + dispersion_std)),
            model_nber = list(lm(mad_R_std ~ rec_indicator)),
            model_nber_control = list(lm(mad_R_std ~ rec_indicator + dispersion_std)),
            model_liquidity = list(lm(mad_R_std ~ liquidity_std)),
            model_liquidity_control = list(lm(mad_R_std ~ liquidity_std + dispersion_std)),
            R2_vix = summary(model_vix[[1]])$r.sq,
            R2_vix_control = summary(model_vix_control[[1]])$r.sq,
            R2_nber = summary(model_nber[[1]])$r.sq,
            R2_nber_control = summary(model_nber_control[[1]])$r.sq,
            R2_liquidity = summary(model_liquidity[[1]])$r.sq,
            R2_liquidity_control = summary(model_liquidity_control[[1]])$r.sq,
            .groups = 'drop') |>
  select(node, R2_vix, R2_vix_control, R2_nber, R2_nber_control, R2_liquidity, R2_liquidity_control) |>
  mutate(VIX = mean(R2_vix, na.rm = TRUE) * 100,
         VIX_c = mean(R2_vix_control, na.rm = TRUE) * 100,
         NBER = mean(R2_nber, na.rm = TRUE) * 100,
         NBER_c = mean(R2_nber_control, na.rm = TRUE) * 100,
         Liquidity = mean(R2_liquidity, na.rm = TRUE) * 100,
         Liquidity_c = mean(R2_liquidity_control, na.rm = TRUE) * 100, 
         VIX = format(round(VIX, digits = 1), nsmall = 2), 
         VIX_c = format(round(VIX_c, digits = 1), nsmall = 2),
         NBER = format(round(NBER, digits = 1), nsmall = 2), 
         NBER_c = format(round(NBER_c, digits = 1), nsmall = 2),
         Liquidity = format(round(Liquidity, digits = 1), nsmall = 2), 
         Liquidity_c = format(round(Liquidity_c, digits = 1), nsmall = 2),
         VIX = paste0(VIX, "\\%"), 
         VIX_c = paste0(VIX_c, "\\%"),
         NBER = paste0(NBER, "\\%"), 
         NBER_c = paste0(NBER_c, "\\%"),
         Liquidity = paste0(Liquidity, "\\%"), 
         Liquidity_c = paste0(Liquidity_c, "\\%")) |>
  slice(1) |>
  select(node, VIX, VIX_c, NBER, NBER_c, Liquidity, Liquidity_c) |>
  mutate(node = replace(node, node == "drop_bookequity", "R2")) |>
  rename("Node" = node)

# Merge all togehter 
part1 <- produce_table(data = table_residual_mad_R_std_without) |> 
  mutate(number = 1,
         index = cumsum(number)) |>
  select(-number)

part2 <- produce_table(data = table_residual_mad_R_std) |> 
  mutate(number = 1,
         index = cumsum(number))|>
  select(-Node, -number) |>
  rename("VIX_c" = VIX, 
         "NBER_c" = NBER,    
          "Liquidity_c" = Liquidity)

Table <- part1 |>
  left_join(part2, by = c("index")) |>
  select(-index) |>
  relocate(Node, VIX, VIX_c, NBER, NBER_c, Liquidity, Liquidity_c) |>
  rbind(average_r2) |>
  mutate(Node = replace(Node, Node == "R2", "$\\Bar{R^{2}}$")) |>
  rename("$VIX$" = VIX, 
         "$VIX_{c}$" = VIX_c,
         "$NBER$" = NBER, 
         "$NBER_{c}$" = NBER_c,
         "$Liquidity$" = Liquidity, 
         "$Liquidity_{c}$" = Liquidity_c,
         "Fork" = Node)
 
rm(part1, part2, average_r2)


# Print Table and save ---------------------------------------------
Table |> 
  print_tex_table(booktabs = TRUE,
                  include.colnames = TRUE,
                  file = "Paper_Tables/05_mad_TS_regression.tex")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)