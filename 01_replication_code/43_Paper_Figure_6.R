
# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(broom)
library(lmtest)
library(sandwich)
library(slider)
library(furrr)
library(car)
library(tikzDevice)


# Options ----------------------------------------------------------
source("z_options_figures.R")


# Set up -----------------------------------------------------------
# SQLite database
data_ts <- dbConnect(SQLite(), 
                     "Data/data_TS_sv_ag_full.sqlite", 
                     extended_types = TRUE)

data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Load Data --------------------------------------------------------
# SV timeseries
data_timeseries <- dbReadTable(data_ts, "data_timeseries")

# Liquiditiy data
liquidity_data <- dbReadTable(data_nse, "liquidity_data")

# Factor data
factors_ff_monthly <- dbReadTable(data_nse, "factors_ff_monthly")


# Compute factor adjusted premia -----------------------------------
# Function
model_all <- function(data) {
  # Add factors
  data <- data |>
    left_join(factors_ff_monthly, by = "month")
  
  # Run regressions for CAPM and FF5
  reg_capm <- lm(premium ~ 1 + mkt_excess, data = data)
  reg_ff3 <- lm(premium ~ 1 + mkt_excess + smb + hml, data = data)
  reg_ff5 <- lm(premium ~ 1 + mkt_excess + smb + hml + rmw + cma, data = data)
  
  # Return premia ts (intercept + residuals)
  return(tibble(month = data$month,
                premium_capm = reg_capm$coefficients[1] + reg_capm$residuals,
                premium_ff3 = reg_ff3$coefficients[1] + reg_ff3$residuals,
                premium_ff5 = reg_ff5$coefficients[1] + reg_ff5$residuals))
}

# Parallelization setup
alphas <- data_timeseries |>
  group_by(ID) |>
  nest() |> 
  ungroup()

# Parallel environment
plan(multisession, workers = 4)
alphas <- alphas |>
  mutate(svs = future_map(data, ~ model_all(.))) |>
  unnest(svs) |>
  select(ID, month, premium_capm, premium_ff3, premium_ff5)

# End plan
plan(sequential)

# Merge back
data_timeseries <- data_timeseries |> 
  left_join(alphas, by = c("ID", "month"))

# Clear memory
rm(alphas)


# Add macro data ---------------------------------------------------
# Lag macro data by 12 months 
liquidity_data_lag12 <- liquidity_data |>
  mutate(month = month %m+% months(12)) |> 
  rename_with(.cols = liquidity, ~ paste0(.x, "_lag12")) |>
  select(month, liquidity_lag12)

rm(liquidity_data)

# Merge Tables
data_timeseries <- data_timeseries |>
  left_join(liquidity_data_lag12, by = "month") |>
  arrange(ID) 


# Compute annual premia --------------------------------------------
# Calculate yearly premium estimates with for each month
roll_sum <- function(data, months) {
  data <- bind_rows(data)
  
  returns <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ if_else(nrow(.x) < months, 
                   NA_real_, # if less than 12 months 
                   sum(.x[,2])),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(returns)
}

data_timeseries <- data_timeseries |>
  mutate(premium_log = log(1 + premium),
         premium_capm_log = log(1 + premium_capm),
         premium_ff3_log = log(1 + premium_ff3),
         premium_ff5_log = log(1 + premium_ff5)) |>
  arrange(ID, month) |>
  group_by(ID) |> 
  mutate(sum_premium_log = roll_sum(pick(month, premium_log), 12),
         sum_premium_capm_log = roll_sum(pick(month, premium_capm_log), 12),
         sum_premium_ff3_log = roll_sum(pick(month, premium_ff3_log), 12),
         sum_premium_ff5_log = roll_sum(pick(month, premium_ff5_log), 12)) |> 
  ungroup() |>
  mutate(premium_yearly = exp(sum_premium_log) - 1,
         premium_capm_yearly = exp(sum_premium_capm_log) - 1,
         premium_ff3_yearly = exp(sum_premium_ff3_log) - 1,
         premium_ff5_yearly = exp(sum_premium_ff5_log) - 1) |> 
  select(-ends_with("_log"))


# Predictive regression --------------------------------------------
predictive_regression <- function(data) {
  # Unadjusted
  reg_raw <- lm(premium_yearly ~ 1 + liquidity_lag12, data = data)
  reg_raw <- coeftest(reg_raw, vcov = NeweyWest(reg_raw, lag = 12))

  # CAPM
  reg_capm <- lm(premium_capm_yearly ~ 1 + liquidity_lag12, data = data)
  reg_capm <- coeftest(reg_capm, vcov = NeweyWest(reg_capm, lag = 12))
  
  # FF3
  reg_ff3 <- lm(premium_ff3_yearly ~ 1 + liquidity_lag12, data = data)
  reg_ff3 <- coeftest(reg_ff3, vcov = NeweyWest(reg_ff3, lag = 12))
  
  # FF5
  reg_ff5 <- lm(premium_ff5_yearly ~ 1 + liquidity_lag12, data = data)
  reg_ff5 <- coeftest(reg_ff5, vcov = NeweyWest(reg_ff5, lag = 12))
  
  # Output
  return(tibble(beta_raw = reg_raw[2,1],
                beta_raw_se = reg_raw[2,2],
                beta_raw_t = reg_raw[2,3],
                beta_capm = reg_capm[2,1],
                beta_capm_se = reg_capm[2,2],
                beta_capm_t = reg_capm[2,3],
                beta_ff3 = reg_ff3[2,1],
                beta_ff3_se = reg_ff3[2,2],
                beta_ff3_t = reg_ff3[2,3],
                beta_ff5 = reg_ff5[2,1],
                beta_ff5_se = reg_ff5[2,2],
                beta_ff5_t = reg_ff5[2,3]))
}

# Liquidity
reg_data_liquidity <- data_timeseries |>
  group_by(ID) |>
  nest() |> 
  mutate(estimates = map(data, ~predictive_regression(.x))) |> 
  unnest(estimates) |> 
  select(-data) |> 
  ungroup()

# Save
saveRDS(reg_data_liquidity, file = "Data/betas_ag_liquidity.rds")
reg_data_liquidity <- readRDS("Data/betas_ag_liquidity.rds")


# Median test ------------------------------------------------------
# Function
median_test <- function(betas, standard_errors) {
  # Median
  beta_median <- median(betas)
  
  # Get t-stats
  tstats <- abs((betas - beta_median)) / standard_errors
  
  # Output
  return(sum(tstats > qnorm(p = 0.95)))
}

# Liquidity
deviations_from_median <- reg_data_liquidity |> 
  summarize(raw = median_test(beta_raw, beta_raw_se),
            capm = median_test(beta_capm, beta_capm_se),
            ff3 = median_test(beta_ff3, beta_ff3_se),
            ff5 = median_test(beta_ff5, beta_ff5_se))
deviations_from_median


# Significance of paths --------------------------------------------
# Number of segnificant premia
significant_paths <- reg_data_liquidity |> 
  summarize(positive = across(ends_with("_t"), ~ sum(.x > qnorm(0.95))),
            negative = across(ends_with("_t"), ~ sum(.x < qnorm(0.05))))

bind_rows(significant_paths$positive, significant_paths$negative) |> 
  mutate(direction = c("positive", "negative")) |> 
  rename_with(~ gsub("_t", "", .x)) |> 
  rename_with(~ gsub("beta_", "", .x)) |> 
  mutate(across(raw:ff5, ~ .x / nrow(reg_data_liquidity) * 100)) |> 
  select(direction, raw:ff5)


# Difference in variance -------------------------------------------
# Long table of betas
variance_data <- reg_data_liquidity |> 
  select(beta_raw, beta_ff3, beta_ff5) |> 
  pivot_longer(cols = everything(), 
               names_to = "model",
               values_to = "beta") |> 
  mutate(model = as_factor(model))

# Run levenes test
leveneTest(beta ~ model, data = variance_data)


# Produce graph ----------------------------------------------------
graph_coefficients <- reg_data_liquidity |> 
  rename(Unadjusted = beta_raw,
         CAPM = beta_capm,
         FF3 = beta_ff3,
         FF5 = beta_ff5) |> 
  ggplot() +
  geom_density(aes(x  = Unadjusted), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = FF3), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  geom_density(aes(x  = FF5), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "darkgreen", colour = "darkgreen", linetype = 'dotted') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "Liquidity gamma",
       y = NULL,
       title = NULL,
       subtitle = NULL)
  
# Output plot
tikz("Paper_Plots/tex/06_predictive_liquidity.tex",
     standAlone = T, width = 6, height = 3)
graph_coefficients
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/06_predictive_liquidity.tex"))
system(paste0("rm 06_predictive_liquidity.log"))
system(paste0("rm 06_predictive_liquidity.aux"))
setwd(dirname(getwd()))


# Close ------------------------------------------------------------
dbDisconnect(data_nse)
dbDisconnect(data_ts)