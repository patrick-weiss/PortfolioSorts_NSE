# Stylized facts across decision nodes


# Packages ----------------------------------------------------------------
library(tidyverse)
library(moments)
library(DBI)
library(RSQLite)


# Data --------------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Sorting data
load("Data/data_sorts.RData")


# Tests -------------------------------------------------------------------


# Size filter -------------------------------------------------------------
# Mktcap and number of stocks
data_3m %>% 
  select(mktcap, exchange, month) %>%
  group_by(month) %>%
  mutate(NYSE_5 = quantile(mktcap[exchange == "NYSE"], 0.05),
         NYSE_20 = quantile(mktcap[exchange == "NYSE"], 0.20)) %>%
  summarise(mktcap_5 = sum(mktcap[mktcap <= NYSE_5])/sum(mktcap),
            mktcap_20 = sum(mktcap[mktcap <= NYSE_20])/sum(mktcap),
            num_5 = sum(mktcap <= NYSE_5)/n(),
            num_20 = sum(mktcap <= NYSE_20)/n(),
            .groups = 'drop') %>%
  summarise(across(everything(), mean))

# Volatility of returns
data_3m %>% 
  select(mktcap, exchange, permno, month, ret_excess) %>%
  group_by(month) %>%
  mutate(NYSE_5 = quantile(mktcap[exchange == "NYSE"], 0.05),
         NYSE_20 = quantile(mktcap[exchange == "NYSE"], 0.20)) %>%
  group_by(permno, month) %>%
  summarize(sd = sd(ret_excess),
            NYSE_5 = unique(NYSE_5),
            NYSE_20 = unique(NYSE_20),
            mktcap = mktcap,
            .groups = 'drop') %>%
  summarize(sd_5 = mean(sd[mktcap <= NYSE_5], na.rm = T),
            sd_20 = mean(sd[mktcap <= NYSE_20], na.rm = T),
            sd = mean(sd, na.rm = T))


# Financials --------------------------------------------------------------
# Number and market cap
data_3m %>%
  select(mktcap, industry, month) %>%
  group_by(month) %>%
  summarize(num_fin = sum(industry == "Finance")/n(),
            mktcap = sum(mktcap[industry == "Finance"])/sum(mktcap),
            .groups = 'drop') %>%
  summarize(across(everything(), mean))

# Utilities ---------------------------------------------------------------
# Number and market cap
data_3m %>%
  select(mktcap, industry, month) %>%
  group_by(month) %>%
  summarize(num_fin = sum(industry == "Utilities")/n(),
            mktcap = sum(mktcap[industry == "Utilities"])/sum(mktcap),
            .groups = 'drop') %>%
  summarize(across(everything(), mean))


# Book equity -------------------------------------------------------------
sum(data_3m$filter_earnings < 0, na.rm = TRUE)/sum(!is.na(data_3m$filter_earnings))


# Stock-age ---------------------------------------------------------------
sum(data_3m$filter_stock_age < 2, na.rm = TRUE)/sum(!is.na(data_3m$filter_stock_age))


# Exchanges ---------------------------------------------------------------
data_3m %>%
  select(exchange, month, mktcap) %>% 
  group_by(exchange, month) %>%
  summarize(mktcap = mean(mktcap, na.rm = T),
            .groups = 'drop_last') %>%
  summarize(mktcap = mean(mktcap))
    

