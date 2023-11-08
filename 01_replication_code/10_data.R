# Data download

# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(readxl)
library(RPostgres)
library(frenchdata)


# Set up -----------------------------------------------------------
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
  user = Sys.getenv("user"), # put your information here
  password = Sys.getenv("password") # put your information here
)


# Fama & French 5 factors (monthly) --------------------------------
# Load
factors_ff_monthly <- download_french_data("Fama/French 5 Factors (2x3)")$subsets$data[[1]] |>
  janitor::clean_names() 

# Manipulate
factors_ff_monthly <- factors_ff_monthly |>
  transmute(
    month = floor_date(ymd(paste0(date, "01")), "month"),
    mkt_excess = as.numeric(mkt_rf) / 100,
    smb = as.numeric(smb) / 100,
    hml = as.numeric(hml) / 100,
    rmw = as.numeric(rmw) / 100,
    cma = as.numeric(cma) / 100, 
    rf = as.numeric(rf) / 100
  ) |>
  filter(month >= start_date & month <= end_date)

# Store
factors_ff_monthly |>
  dbWriteTable(conn = data_nse, 
               name = "factors_ff_monthly", 
               value = _,
               overwrite = TRUE)


# Fama & French factors (daily) ------------------------------------
# Load
factors_ff_daily <- download_french_data("Fama/French 3 Factors [Daily]")$subsets$data[[1]] |>
  janitor::clean_names() 

# Manipulate
factors_ff_daily <- factors_ff_daily |>
  transmute(
    date = ymd(date),
    rf = as.numeric(rf) / 100,
    mkt_excess = as.numeric(mkt_rf) / 100,
    smb = as.numeric(smb) / 100,
    hml = as.numeric(hml) / 100
  ) |>
  filter(date >= start_date & date <= end_date)

# Store
factors_ff_daily |>
  dbWriteTable(conn = data_nse, 
               name = "factors_ff_daily", 
               value = _,
               overwrite = TRUE)


# Q factors --------------------------------------------------------
# Load & manipulate
factors_q_monthly_link <- 
  "http://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2022.csv"

factors_q_monthly <- read_csv(factors_q_monthly_link) |>
  mutate(month = ymd(str_c(year, month, "01", sep = "-"))) |>
  select(-R_F, -year) |>
  rename_with(~ str_replace(., "R_", "q_")) |>
  rename_with(~ str_to_lower(.)) |>
  mutate(across(-month, ~ . / 100)) |>
  filter(month >= start_date & month <= end_date)

# Store
factors_q_monthly |>
  dbWriteTable(conn = data_nse, 
               name = "factors_q_monthly", 
               value = _,
               overwrite = TRUE)


# CRSP Monthly -----------------------------------------------------
# Load
## Returns
msf_db <- tbl(wrds, in_schema("crsp", "msf"))

## Names
msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))

## Delisting
msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))

# CRSP
crsp_monthly <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  inner_join(msenames_db |>
               filter(shrcd %in% c(10, 11)) |>
               select(permno, ncusip, exchcd, siccd, namedt, nameendt), by = c("permno")) |>
  filter(date >= namedt & date <= nameendt) |>
  mutate(month = floor_date(date, "month")) |>
  left_join(msedelist_db |>
              select(permno, dlstdt, dlret, dlstcd) |>
              mutate(month = floor_date(dlstdt, "month")), by = c("permno", "month")) |>
  select(permno, cusip, ncusip, month, ret, retx, shrout, altprc, exchcd, siccd, dlret, dlstcd) |>
  mutate(month = as.Date(month)) |>
  collect() 

# Manipulate
# CRSP mktcap & lag
crsp_monthly <- crsp_monthly |>
  mutate(mktcap = shrout * 10^3 * abs(altprc) / 10^6) |>
  mutate(mktcap = if_else(mktcap == 0, NA_real_, mktcap))

mktcap_lag <- crsp_monthly |>
  mutate(month = month %m+% months(1)) |>
  select(permno, month, mktcap_lag = mktcap)

crsp_monthly <- crsp_monthly |>
  left_join(mktcap_lag, by = c("permno", "month"))

rm(mktcap_lag)

# Delisting returns
crsp_monthly <- crsp_monthly |>
  mutate(ret_adj = case_when(
    is.na(dlstcd) ~ ret,
    !is.na(dlstcd) & !is.na(dlret) ~ dlret,
    dlstcd %in% c(500, 520, 580, 584) |
      (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
    dlstcd == 100 ~ ret,
    TRUE ~ -1
  )) |>
  select(-c(dlret, dlstcd))

# Excess returns
crsp_monthly <- crsp_monthly |>
  left_join(factors_ff_monthly |> select(month, rf), by = "month") |>
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1)
  ) |>
  select(-ret_adj, -rf)

# Exchange codes
crsp_monthly <- crsp_monthly |>
  mutate(exchange = case_when(
    exchcd %in% c(1, 31) ~ "NYSE",
    exchcd %in% c(2, 32) ~ "AMEX",
    exchcd %in% c(3, 33) ~ "NASDAQ",
    TRUE ~ "Other"
  ))

# Industry codes
crsp_monthly <- crsp_monthly |>
  mutate(industry = case_when(
    siccd >= 1 & siccd <= 999 ~ "Agriculture",
    siccd >= 1000 & siccd <= 1499 ~ "Mining",
    siccd >= 1500 & siccd <= 1799 ~ "Construction",
    siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
    siccd >= 4000 & siccd <= 4899 ~ "Transportation",
    siccd >= 4900 & siccd <= 4999 ~ "Utilities",
    siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
    siccd >= 5200 & siccd <= 5999 ~ "Retail",
    siccd >= 6000 & siccd <= 6999 ~ "Finance",
    siccd >= 7000 & siccd <= 8999 ~ "Services",
    siccd >= 9000 & siccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))

# Drop NAs
crsp_monthly <- crsp_monthly |>
  drop_na(ret_excess, mktcap, mktcap_lag)

# Check for duplicates
crsp_monthly |>
  distinct(permno, month) |> 
  nrow() == nrow(crsp_monthly |> drop_na(permno)) # Check if distinct observations = starting number

# Save 
crsp_monthly |>
  dbWriteTable(conn = data_nse, 
               name = "crsp_monthly", 
               value = _,
               overwrite = TRUE)


# CRSP Daily -------------------------------------------------------
# Connection
dsf_db <- tbl(wrds, in_schema("crsp", "dsf"))
msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))

# Permnos
permnos <- tbl(data_nse, "crsp_monthly") |>
  distinct(permno) |>
  pull()

# Download data
crsp_daily <- dsf_db |>
  filter(permno %in% permnos &
           date >= start_date & date <= end_date) |>
  inner_join(msenames_db |>
               filter(shrcd %in% c(10, 11)) |>
               select(permno, exchcd, namedt, nameendt), by = c("permno")) |>
  filter(date >= namedt & date <= nameendt) |> 
  select(permno, exchcd, date, ret, shrout, prc, vol, cfacpr) |>
  collect() |>
  drop_na() |>
  mutate(month = floor_date(date, "month")) 

# Adjusted returns
crsp_daily <- crsp_daily |>
  left_join(factors_ff_daily |>
              select(date, rf), by = "date") |>
  mutate(ret_excess = ret - rf,
         ret_excess = pmax(ret_excess, -1)) |> 
  drop_na(ret_excess) |>
  mutate(vol = if_else(vol == -99, NA_real_, vol),
         prc = if_else(prc == 0, NA_real_, prc),
         prc_adj = abs(prc) / cfacpr) |>
  select(permno, date, ret_excess, exchcd, shrout, prc_adj, prc, vol)
  
# Dates for volume adjustment (Gao and Ritter, 2010)
date_1 <- as.Date("2001-02-01")
date_2 <- as.Date("2002-01-01")
date_3 <- as.Date("2004-01-01")

# Adjust dollar trading volume according to Gao and Ritter (2010)
crsp_daily <- crsp_daily |>
  mutate(vol_adj = case_when(exchcd == 3 & date < date_1 ~ vol / 2.0,
                             exchcd == 3 & date >= date_1 & date < date_2 ~ vol / 1.8,
                             exchcd == 3 & date >= date_2 & date < date_3 ~ vol / 1.6,
                             exchcd == 3 & date >= date_3 ~ vol / 1.0,
                             TRUE ~ vol)) |> 
  select(-vol, -exchcd)

# Remove dates (safety only)
rm(date_1, date_2, date_3)

# Check for duplicates
crsp_daily |> 
  distinct(permno, date) |>  
  nrow() == nrow(crsp_daily |> drop_na(permno)) # Check if distinct observations = starting number

# Store
crsp_daily |>
  dbWriteTable(data_nse, 
               "crsp_daily", 
               value = _, 
               overwrite = TRUE)

# Free memory
rm(crsp_daily)


# Compustat Annual -------------------------------------------------
# Load
compustat <- tbl(wrds, in_schema("comp", "funda")) |>
  filter(
    indfmt == "INDL" &
      datafmt == "STD" &
      consol == "C" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(
    gvkey, # Firm identifier
    datadate, # Date of the accounting data
    act, # Current assets
    ajex, # stock adjustment factor
    am, # amortization of intangibles 
    ao, # other long-term assets 
    at, # Total assets
    ap, # accounts payable 
    apalch, # change in accounts payable and accrued liabilities
    capx, # Capital expenditure
    capxv, # capital expenditure in property, plant and equipment
    ceq, # Total common/ordinary equity
    che, # cash and short term investment
    cogs, # Cost of Goods Sold
    cdvc, # Cash Dividends on Common Stock
    csho, # Common Shares Outstanding
    dcvt, # convertible debt 
    dltt, # long-term debt
    dlc, # Debt in Current Liabilities - Total
    dlcch, # Net changes in current debt 
    dltis, # Cash proceeds from the issuance of long-term debt 
    dltr, # long-term debt reductions 
    dd1, # Long-Term Debt Due in One Year
    dd2, #Long-term Debt due in second year
    dd3, #Long-term Debt due in third year
    dd4, #Long-term Debt due in fourth year
    dd5, #Long-term Debt due in fifth year
    dm, # mortgages and other secured debt
    dp, # Depreciation and Amortization (DP)
    drc, # Current deferred revenues
    drlt, # Long term deferred revenues
    dp, # depriciation
    dv, # Cash Dividends(Cash Flow)
    dvc, # dividends on common stock
    dvp, # dividends 
    dvpa, # dividends in arrears
    dvpsx_f, # cash dividends 
    emp, # Employees
    epspx, # earnings per share
    fatb, # building cost
    fatl, # leases cost
    fincf, # financing cash flows
    fopt, # funds from operations
    gdwl, # Goodwill 
    gp, # Gross Profit (Loss)
    ib, # income before extraordinary items 
    intan, # intangibles
    invch, # change in inventory 
    invt, # Inventory
    invfg, # inventory of finished goods 
    ivncf, # investing cash flows
    itcb, # Investment tax credit
    ivao, # long term investments
    ivpt, # Investments - Permanent - Total
    ivst, # Short-term Investments - Total
    lct, # Current liabilities
    lo, # other long-term liabilities 
    lt, # Total liabilities
    mib, # minority interest
    ni, # Net income
    oancf, # net cash flow from operations
    ob, # order backlog
    oibdp, # Operating income before depreciation
    oiadp, # Earnings before interest and taxes 
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
    re, # Retained earnings 
    recch, # change in accounts receivable 
    rect, # Accounts receivable
    revt, # Revenue - Total
    sale, # Sales/Turnover (Net)
    seq, # Stockholders' equity
    sstk, # Sale of Common and Preferred Stock (SSTK)
    tstkp, # preferred treasury stocks 
    txditc, # Deferred taxes and investment tax credit
    txdi, # deferred taxes
    txdb, # Deferred taxes
    txp, # taxes payable
    txt, # total income taxes 
    wcap, # working capital 
    xacc, # accrued expenses
    xad, # advertising expenses
    xint, # Interest and Related Expense - Total
    xlr,  # Staff Expense - Total
    xpp,  # Prepaid expenses 
    xrd, # R&D expenses
    xsga # Selling, General and Administrative Expense
  ) |>
  collect()

# Manipulate
compustat <- compustat |>
  mutate(year = year(datadate),
         month = floor_date(datadate, "month")) |>
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |>
  ungroup()

# Check for duplicates
## datadate
compustat |> 
  distinct(gvkey, datadate) |> 
  nrow() == nrow(compustat) # Check if distinct observations = starting number

## year
compustat |> 
  distinct(gvkey, year) |> 
  nrow() == nrow(compustat) # Check if distinct observations = starting number

## month
compustat |> 
  distinct(gvkey, month) |> 
  nrow() == nrow(compustat) # Check if distinct observations = starting number

# Replace negative values of sales, total assets, capital expenditure and inventory with zero
compustat <- compustat |> 
  mutate(sale = if_else(sale < 0, NA_real_, sale),
         at = if_else(at < 0, NA_real_, at),
         capx = if_else(capx < 0, NA_real_, capx),
         invt = if_else(invt < 0, NA_real_, invt))

# Save
compustat |>
  dbWriteTable(conn = data_nse, 
               name = "compustat", 
               value = _,
               overwrite = TRUE)


# Compustat Quarter ------------------------------------------------
# Load
compustat_quarterly <- tbl(wrds, in_schema("comp", "fundq")) |>
 filter(
   indfmt == "INDL" &
     datafmt == "STD" &
     consol == "C" &
     datadate >= start_date & datadate <= end_date
 ) |>
 select(
   gvkey, # Firm identifier
   datadate, # Date of the accounting data
   fqtr, # fiscal quarter
   fyearq, # fiscal year of quarter
   ajexq, # ajdustment factor shares outstanding 
   atq, # Total assets
   cdvcy, # Cash Dividends on Common Stock
   ceqq, # common equity
   cheq, # cash and short term investments
   cogsq, # cost of goods sold
   cogsy, # Cost of Goods Sold
   cshoq, # Common Shares Outstanding
   cshprq, # shares outstanding 
   dlttq, # long-term debt
   dlcq, # debt in current liabilities
   dd1q, # Long-Term Debt Due in One Year
   dpq, # depriciation and amortization 
   dvy, # Cash Dividends(Cash Flow)
   epspxq, # earings per share
   ibq, # income before extraordinary items
   ivltq, # Total Long-term investments
   ivstq, # Short-term Investments - Total
   ivaoq, # other investments and advances
   ltq, # Total liabilities
   mibq, # minority interests
   niq, # Net income
   pstkq, # Preferred stock par value
   prccq, # price close
   ppegtq, # Property, Plant and Equipment - Total (Gross)
   pstkrq, # redemption value
   rdq, # earnings' announcement date
   revtq, # revenues
   saley, # Sales/Turnover (Net)
   saleq, # sales 
   seqq, # shareholders' equity
   txditcq, # Deferred taxes and investment tax credit
   txdbq, # Deferred taxes
   txtq, # tax expense
   wcapq, # working capital 
   xintq, # Interest and Related Expense - Total
   xrdq, # R&D expenses
   xsgaq, # Selling, General and Administrative Expense
 ) |>
 collect()

# Manipulate
compustat_quarterly <- compustat_quarterly |> 
  drop_na(fqtr)|>
  mutate(timepoint = paste0(fyearq, fqtr)) |>
  group_by(gvkey, timepoint) |>
  filter(datadate == max(datadate)) |>
  ungroup() 

# Create date variable 
compustat_quarterly <- compustat_quarterly |>  
  arrange(gvkey, datadate) |> 
  mutate(month = ceiling_date(datadate, "quarter") %m-% months(1))

# Drop data that is announced before datadate
compustat_quarterly <- compustat_quarterly |> 
  filter(if_else(is.na(rdq), TRUE, month < rdq))

# Remove duplicates
compustat_quarterly <- compustat_quarterly |> 
  add_count(gvkey, month) |> 
  filter(n == 1) |> 
  select(-n)

# Replace negative values of total assets with zero and sales 
compustat_quarterly <- compustat_quarterly |> 
  mutate(atq = if_else(atq < 0, NA_real_, atq), 
         saleq = if_else(saleq < 0, NA_real_, saleq))

# Save
compustat_quarterly |>
  dbWriteTable(conn = data_nse, 
               name = "compustat_quarterly", 
               value = _,
               overwrite = TRUE)


# CRSP Compustat link ----------------------------------------------
# Load linking table
ccmxpf_linktable <- tbl(wrds, in_schema("crsp", "ccmxpf_linktable")) |>
  collect()

# Manipulate
ccmxpf_linktable <- ccmxpf_linktable |>
  filter(linktype %in% c("LU", "LC") &
           linkprim %in% c("P", "C") &
           usedflag == 1) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  mutate(linkenddt = replace_na(linkenddt, Sys.Date()))

ccm_links <- crsp_monthly |>
  inner_join(ccmxpf_linktable, by = "permno", multiple = "all") |>
  filter(!is.na(gvkey) & (month >= linkdt & month <= linkenddt)) |>
  select(permno, gvkey, month)

# Add to crsp_monthly
crsp_monthly <- crsp_monthly |>
  left_join(ccm_links, by = c("permno", "month"))

# Check for duplicates
crsp_monthly |> 
  drop_na(gvkey) |>
  distinct(gvkey, month) |> 
  nrow() == nrow(crsp_monthly |> drop_na(gvkey)) # Check if distinct observations = starting number

# Save 
crsp_monthly |>
  dbWriteTable(conn = data_nse, 
               name = "crsp_monthly", 
               value = _,
               overwrite = TRUE)


# CBOE Index -------------------------------------------------------
# Database
cboe_db <- tbl(wrds, in_schema("cboe", "cboe"))

# CBOE Data
cboe_data <- cboe_db |>
  filter(date >= as.Date("1990-01-01") & date <= end_date) |>
  select(date, vix) |>
  mutate(month = floor_date(date, "month")) |>
  collect() |> 
  drop_na() |>
  arrange(date) |>
  group_by(month) |>
  slice_tail(n = 1) |> 
  ungroup()
  
# Calculate the change 
# Lag variables by 1 month
cboe_lag1 <- cboe_data |>
  select(month, vix) |>
  mutate(month = month %m+% months(1)) |>
  rename_with(.cols = vix, ~ paste0(.x, "_lag1"))

# Remerge
cboe_data <- cboe_data |> 
  left_join(cboe_lag1, by = c("month")) |>
  arrange(month) |>
  mutate(change_vix = (vix - vix_lag1) / vix_lag1, 
         vix_mean = mean(vix, na.rm = TRUE), 
         vix_sd = sd(vix, na.rm = TRUE), 
         vix_std = (vix - vix_mean) / vix_sd)|>
  select(month, vix, vix_std, change_vix) 

# Save 
cboe_data |>
  dbWriteTable(conn = data_nse, 
               name = "cboe_data", 
               value = _,
               overwrite = TRUE)

# Clear memory
rm(cboe_db, cboe_data, cboe_lag1)


# NBER recession periods -------------------------------------------
# Link (alternative: Tidyquant)
nber_recession_link <- 
  "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=USREC&scale=left&cosd=1854-12-01&coed=2023-02-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-03-03&revision_date=2023-03-03&nd=1854-12-01"

# Download and manipulation
nber_recession <- read_csv(nber_recession_link) |>
  mutate(month = floor_date(DATE, "month")) |>
  rename(rec_indicator = USREC) |>
  select(month, rec_indicator) 

# Store
nber_recession |>
  dbWriteTable(conn = data_nse, 
               name = "nber_recession", 
               value = _,
               overwrite = TRUE)

rm(nber_recession, nber_recession_link)


# Pastor and Stambaugh liquidity series ----------------------------
# Link
liquidity_link <- "https://finance.wharton.upenn.edu/~stambaug/liq_data_1962_2021.txt"

# Aggregate liquidity
liquidity_data <- read.table(liquidity_link,
                             sep="\t", 
                             skip = 10, 
                             header=TRUE) |>
  rename(yearmo = X..Month, liquidity = Agg.Liq.) |>
  select(yearmo, liquidity) |>
  mutate(month = floor_date(ymd(paste0(yearmo, "01")), "month")) |>
  filter(month >= as.Date("1972-01-01") & month <= end_date) |>
  select(month, liquidity) |>
  mutate(liquidity_mean = mean(liquidity, na.rm = TRUE), 
         liquidity_sd = sd(liquidity, na.rm = TRUE), 
         liquidity_std = (liquidity - liquidity_mean) / liquidity_sd) |>
  select(month, liquidity, liquidity_std)

# Store
liquidity_data |> dbWriteTable(conn = data_nse, 
                               name = "liquidity_data", 
                               value = _,
                               overwrite = TRUE)

# Liquidity factor
factors_liq <- read.table(liquidity_link,
                             sep="\t", 
                             skip = 10, 
                             header=TRUE) |>
  select(yearmo = X..Month, liq = Traded.Liq..LIQ_V.) |>
  mutate(month = floor_date(ymd(paste0(yearmo, "01")), "month")) |>
  filter(month >= start_date & month <= end_date) |>
  filter(liq != -99) |> 
  select(month, liq)

# Store
factors_liq |> dbWriteTable(conn = data_nse, 
                            name = "factors_liq", 
                            value = _,
                            overwrite = TRUE)

rm(liquidity_data, liquidity_link, factor_liq)


# Momentum factor --------------------------------------------------
# Download momentum factor from Kenneth French's website
# Load
factors_mom <- download_french_data("Momentum Factor (Mom)")$subsets$data[[1]] |>
  janitor::clean_names() 

# Manipulate
factors_mom <- factors_mom |>
  transmute(
    month = floor_date(ymd(paste0(date, "01")), "month"),
    mom = as.numeric(mom) / 100
  ) |>
  filter(month >= start_date & month <= end_date) |> 
  select(month, mom)

# Store
factors_mom |>
  dbWriteTable(conn = data_nse, 
               name = "factors_mom", 
               value = _,
               overwrite = TRUE)