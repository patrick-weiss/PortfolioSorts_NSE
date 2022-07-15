# Data

# Packages ----------------------------------------------------------------

library(tidyverse)
library(RSQLite)
library(dbplyr)
library(lubridate)
library(readxl)
library(RPostgres)
library(frenchdata)

# Set up ------------------------------------------------------------------
# SQLite database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Dates
start_date <- as.Date("1968-01-01")
end_date <- as.Date("2021-12-31")

# WRDS connection
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("user"),
  password = Sys.getenv("password")
)


# Fama & French factors (monthly) ----------------------------------------------
# Load
## Download with package 'frenchdata'
factors_ff_monthly <- download_french_data("Fama/French 3 Factors")$subsets$data[[1]] %>%
  janitor::clean_names() 

# Manipulate
factors_ff_monthly <- factors_ff_monthly %>%
  transmute(
    month = floor_date(ymd(paste0(date, "01")), "month"),
    rf = as.numeric(rf) / 100,
    mkt_excess = as.numeric(mkt_rf) / 100,
    smb = as.numeric(smb) / 100,
    hml = as.numeric(hml) / 100
  ) %>%
  filter(month >= start_date & month <= end_date)

# Store
factors_ff_monthly %>%
  dbWriteTable(data_nse, "factors_ff_monthly", ., overwrite = TRUE)

# Rdata
save(factors_ff_monthly, file = "Data/data_FF.Rdata")

# Fama & French factors (daily) ----------------------------------------------
# Load
## Download with package 'frenchdata'
factors_ff_daily <- download_french_data("Fama/French 3 Factors [Daily]")$subsets$data[[1]] %>%
  janitor::clean_names() 

# Manipulate
factors_ff_daily <- factors_ff_daily %>%
  transmute(
    month = ymd(date),
    rf = as.numeric(rf) / 100,
    mkt_excess = as.numeric(mkt_rf) / 100,
    smb = as.numeric(smb) / 100,
    hml = as.numeric(hml) / 100
  ) %>%
  filter(month >= start_date & month <= end_date)

# Store
factors_ff_daily %>%
  dbWriteTable(data_nse, "factors_ff_daily", ., overwrite = TRUE)


# CRSP Monthly -----------------------------------------------------------------
# Load
# Connections
msf_db <- tbl(wrds, in_schema("crsp", "msf"))
msf_db

msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))
msenames_db

msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))
msedelist_db


# CRSP
crsp_monthly <- msf_db %>%
  filter(date >= start_date & date <= end_date) %>%
  inner_join(msenames_db %>%
               filter(shrcd %in% c(10, 11)) %>%
               select(permno, exchcd, siccd, namedt, nameendt), by = c("permno")) %>%
  filter(date >= namedt & date <= nameendt) %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(msedelist_db %>%
              select(permno, dlstdt, dlret, dlstcd) %>%
              mutate(month = floor_date(dlstdt, "month")), by = c("permno", "month")) %>%
  select(permno, month, ret, shrout, altprc, exchcd, siccd, dlret, dlstcd) %>%
  mutate(month = as.Date(month)) %>%
  collect() 

# Manipulate
# CRSP mktcap & lag
crsp_monthly <- crsp_monthly %>%
  mutate(mktcap = shrout * 10^3 * abs(altprc) / 10^6) %>%
  mutate(mktcap = if_else(mktcap == 0, as.numeric(NA), mktcap))

mktcap_lag <- crsp_monthly %>%
  mutate(month = month %m+% months(1)) %>%
  select(permno, month, mktcap_lag = mktcap)

crsp_monthly <- crsp_monthly %>%
  left_join(mktcap_lag, by = c("permno", "month"))

# Delisting returns
crsp_monthly <- crsp_monthly %>%
  mutate(ret_adj = case_when(
    is.na(dlstcd) ~ ret,
    !is.na(dlstcd) & !is.na(dlret) ~ dlret,
    dlstcd %in% c(500, 520, 580, 584) |
      (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
    dlstcd == 100 ~ ret,
    TRUE ~ -1
  )) %>%
  select(-c(dlret, dlstcd))

# Excess returns
crsp_monthly <- crsp_monthly %>%
  left_join(factors_ff_monthly %>% select(month, rf), by = "month") %>%
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1)
  ) %>%
  select(-ret_adj, -rf)

# Exchange codes
crsp_monthly <- crsp_monthly %>%
  mutate(exchange = case_when(
    exchcd %in% c(1, 31) ~ "NYSE",
    exchcd %in% c(2, 32) ~ "AMEX",
    exchcd %in% c(3, 33) ~ "NASDAQ",
    TRUE ~ "Other"
  ))

# Industry codes
crsp_monthly <- crsp_monthly %>%
  mutate(industry = case_when(
    siccd >= 1 & siccd <= 999 ~ "Agriculture",
    siccd >= 1000 & siccd <= 1499 ~ "Mining",
    siccd >= 1500 & siccd <= 1799 ~ "Construction",
    siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
    siccd >= 4000 & siccd <= 4899 ~ "Transportation",
    siccd >= 4900 & siccd <= 4999 ~ "Utilities",
    siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
    siccd >= 5200 & siccd <= 5999 ~ "Retail",
    siccd >= 6000 & siccd <= 6799 ~ "Finance",
    siccd >= 7000 & siccd <= 8999 ~ "Services",
    siccd >= 9000 & siccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))

# Drop NAs
crsp_monthly <- crsp_monthly %>%
  drop_na(ret_excess, mktcap, mktcap_lag)

# Save 
crsp_monthly %>%
  dbWriteTable(data_nse, "crsp_monthly", ., overwrite = TRUE)


# CRSP Daily -----------------------------------------------------------------
# Load
# Connections
dsf_db <- tbl(wrds, in_schema("crsp", "dsf"))
dsf_db

msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))
msenames_db

msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))
msedelist_db

# CRSP
crsp_daily <- dsf_db %>%
  filter(date >= start_date & date <= end_date) %>%
  inner_join(msenames_db %>%
               filter(shrcd %in% c(10, 11)) %>%
               select(permno,exchcd,namedt, nameendt), by = c("permno")) %>%
  filter(date >= namedt & date <= nameendt) %>%
  mutate(month = date) %>%
  left_join(msedelist_db %>%
              select(permno, dlstdt, dlret, dlstcd) %>%
              mutate(month = floor_date(dlstdt, "month")), by = c("permno", "month")) %>%
  select(permno, exchcd, month, ret, shrout, prc, dlret, dlstcd, vol) %>%
  mutate(month = as.Date(month)) %>%
  collect() 

# Manipulate
# CRSP mktcap 
crsp_daily <- crsp_daily %>%
  arrange(permno, month) %>%
  mutate(mktcap = shrout * 10^3 * abs(prc) / 10^6) %>%
  mutate(mktcap = if_else(mktcap == 0, as.numeric(NA), mktcap))

# Delisting returns
crsp_daily <- crsp_daily %>%
  mutate(ret_adj = case_when(
    is.na(dlstcd) ~ ret,
    !is.na(dlstcd) & !is.na(dlret) ~ dlret,
    dlstcd %in% c(500, 520, 580, 584) |
      (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
    dlstcd == 100 ~ ret,
    TRUE ~ -1
  )) %>%
  select(-c(dlret, dlstcd))

# Excess returns
crsp_daily <- crsp_daily %>%
  left_join(factors_ff_daily %>% select(month, rf), by = "month") %>%
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1)
  ) 

# Drop NAs & subset columns
crsp_daily <- crsp_daily %>%
  drop_na(ret_excess, mkt_excess) %>%
  select(permno, month, ret_excess, prc, vol)

# Save 
crsp_daily %>%
  dbWriteTable(data_nse, "crsp_daily", ., overwrite = TRUE)


# Compustat Annual --------------------------------------------------------
# Load
compustat <- tbl(wrds, in_schema("comp", "funda")) %>%
  filter(
    indfmt == "INDL" &
      datafmt == "STD" &
      consol == "C" &
      datadate >= start_date & datadate <= end_date
  ) %>%
  select(
    gvkey, # Firm identifier
    datadate, # Date of the accounting data
    act, # Current assets
    ajex, # stock adjustment factor
    at, # Total assets
    ap, # accounts payable 
    apalch, # change in accounts payable and accrued liabilities
    capx, # Capital expenditure
    ceq, # Total common/ordinary equity
    che, # cash and short term investment
    cogs, # Cost of Goods Sold
    cdvc, # Cash Dividends on Common Stock
    csho, # Common Shares Outstanding
    dltt, # long-term debt
    dlc, # Debt in Current Liabilities - Total
    dd1, # Long-Term Debt Due in One Year
    dd2, #Long-term Debt due in second year
    dd3, #Long-term Debt due in third year
    dd4, #Long-term Debt due in fourth year
    dd5, #Long-term Debt due in fifth year
    dp, # Depreciation and Amortization (DP)
    drc, # Current deferred revenues
    drlt, # Long term deferred revenues
    dp, # depriciation
    dv, # Cash Dividends(Cash Flow)
    dvc, # dividends on common stock
    emp, # Employees
    epspx, # earnings per share
    fatb, # building cost
    fatl, # leases cost
    fincf, # financing cash flows
    fopt, # funds from operations
    gp, # Gross Profit (Loss)
    ib, # income before extraordinary items 
    invch, # change in inventory 
    invt, # Inventory
    ivncf, # investing cash flows
    itcb, # Investment tax credit
    ivao, # long term investments
    ivpt, # Investments - Permanent - Total
    ivst, # Short-term Investments - Total
    lct, # Current liabilities
    lt, # Total liabilities
    mib, # minority interest
    ni, # Net income
    oancf, # net cash flow from operations
    oibdp, # Operating income before depreciation
    pi, # funds provided from opperations
    ppegt, # Property, Plant and Equipment - Total (Gross),
    ppenb, # buildings
    ppenls, # capital leases
    ppent, # property, plant and equipment
    pstkrv, # Preferred stock redemption value
    pstkl, # Preferred stock liquidating value
    pstk, # Preferred stock par value
    prstkc, # Purchase of Common and Preferred Stock (PRSTKC)
    prcc_f, # price close
    recch, # change in accounts receivable 
    rect, # Accounts receivable
    revt, # Revenue - Total
    sale, # Sales/Turnover (Net)
    seq, # Stockholders' equity
    sstk, # Sale of Common and Preferred Stock (SSTK)
    txditc, # Deferred taxes and investment tax credit
    txdi, # deferred taxes
    txdb, # Deferred taxes
    txp, # taxes payable
    wcap, # working capital 
    xacc, # accrued expenses
    xad, # advertising expenses
    xint, # Interest and Related Expense - Total
    xlr,  # Staff Expense - Total
    xpp,  # Prepaid expenses 
    xrd, # R&D expenses
    xsga # Selling, General and Administrative Expense
  ) %>%
  collect()

# Manipulate
compustat <- compustat %>%
  mutate(year = year(datadate)) %>%
  group_by(gvkey, year) %>%
  filter(datadate == max(datadate)) %>%
  ungroup()

# Save
compustat %>%
  dbWriteTable(data_nse, "compustat", ., overwrite = TRUE)


# Compustat Quarter --------------------------------------------------------
# Currently not used
# # Load
# compustat_quarterly <- tbl(wrds, in_schema("comp", "fundq")) %>%
#   filter(
#     indfmt == "INDL" &
#       datafmt == "STD" &
#       consol == "C" &
#       datadate >= start_date & datadate <= end_date
#   ) %>%
#   select(
#     gvkey, # Firm identifier
#     datadate, # Date of the accounting data
#     fqtr, # fiscal quarter
#     fyearq, # fiscal year of quarter
#     atq, # Total assets
#     cdvcy, # Cash Dividends on Common Stock
#     ceqq, # common equity
#     cheq, # cash and short term investments
#     cogsq, # cost of goods sold
#     cogsy, # Cost of Goods Sold
#     cshoq, # Common Shares Outstanding
#     dlttq, # long-term debt
#     dlcq, # debt in current liabilities
#     dd1q, # Long-Term Debt Due in One Year
#     dvy, # Cash Dividends(Cash Flow)
#     ibq, # income before extraordinary items
#     ivltq, # Total Long-term investments
#     ivstq, # Short-term Investments - Total
#     ivaoq, # other investments and advances
#     ltq, # Total liabilities
#     mibq, # minority interests
#     niq, # Net income
#     pstkq, # Preferred stock par value
#     prccq, # price close
#     ppegtq, # Property, Plant and Equipment - Total (Gross)
#     pstkrq, # redemption value
#     rdq, # earnings' announcement date
#     revtq, # revenues
#     saley, # Sales/Turnover (Net)
#     saleq, # sales 
#     seqq, # shareholders' equity
#     txditcq, # Deferred taxes and investment tax credit
#     txdbq, # Deferred taxes
#     xintq, # Interest and Related Expense - Total
#     xrdq, # R&D expenses
#     xsgaq, # Selling, General and Administrative Expense
#   ) %>%
#   collect()
# 
# # Manipulate
# compustat_quarterly <- compustat_quarterly %>%
#   mutate(timepoint = paste0(fyearq, fqtr)) %>%
#   group_by(gvkey, timepoint) %>%
#   filter(datadate == max(datadate)) %>%
#   ungroup()
# 
# # Save
# compustat_quarterly %>%
#   dbWriteTable(data_nse, "compustat_quarterly", ., overwrite = TRUE)


# CRSP Compustat link -----------------------------------------------------
# Load
ccmxpf_linktable <- tbl(wrds, in_schema("crsp", "ccmxpf_linktable")) %>%
  collect()

# Manipulate
ccmxpf_linktable <- ccmxpf_linktable %>%
  filter(linktype %in% c("LU", "LC") &
           linkprim %in% c("P", "C") &
           usedflag == 1) %>%
  select(permno = lpermno, gvkey, linkdt, linkenddt) %>%
  mutate(linkenddt = replace_na(linkenddt, Sys.Date()))

ccm_links <- crsp_monthly %>%
  inner_join(ccmxpf_linktable, by = "permno") %>%
  filter(!is.na(gvkey) & (month >= linkdt & month <= linkenddt)) %>%
  select(permno, gvkey, month)

# Add to crsp_monthly
crsp_monthly <- crsp_monthly %>%
  left_join(ccm_links, by = c("permno", "month"))

# Save 
crsp_monthly %>%
  dbWriteTable(data_nse, "crsp_monthly", ., overwrite = TRUE)

