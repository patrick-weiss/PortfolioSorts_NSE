

# Construction of yearly sorting variables 


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Packages and Setup ------------------------------------------------------

# Packages 
library(tidyverse)
library(RSQLite)
library(DBI)
library(lubridate)
library(zoo)
library(slider)
library(rollRegres)
library(matrixStats)
library(JWileymisc)
library(modelr)
library(broom)
library(moments)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Annual Variables --------------------------------------------------------

# Load compustat and CRSP

compustat <- dbReadTable(data_nse, "compustat")  
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")  
crsp_input <- crsp_monthly %>% select(gvkey, month, mktcap, siccd) %>% arrange(gvkey, month)

# Check for duplicates
## datadate
compustat %>%
  distinct(gvkey, datadate) %>% 
  nrow() == nrow(compustat) # Check if distinct observations = starting number

## year
compustat %>%
  mutate(year = year(datadate)) %>%
  distinct(gvkey, year) %>% 
  nrow() == nrow(compustat) # Check if distinct observations = starting number

# Bring in CRSP data 

compustat <- compustat %>%
  mutate(month = floor_date(datadate, "month")) %>%
  left_join(crsp_input, by = c("gvkey", "month")) %>%
  arrange(gvkey, month) 

rm(crsp_input)

# Compute sub-variables 
compustat <- compustat %>%
  mutate(wc = (act - che) - (lct -  replace_na(dlc, 0)) ,
         nco = (at - act -  replace_na(ivao, 0)) - (lt - lct -  replace_na(dltt, 0)) ,
         fin = ( replace_na(ivst, 0) +  replace_na(ivao, 0)) - ( replace_na(dltt, 0) + replace_na(dlc, 0) + replace_na(pstk, 0)), 
         nca = at - act - replace_na(ivao, 0),
         ncl = lt - lct - replace_na(dltt, 0),
         nco = nca - ncl, 
         noa = (at - che - replace_na(ivao, 0)) - (at - replace_na(dlc, 0) - replace_na(dltt, 0) - replace_na(mib, 0) - replace_na(pstk, 0) - ceq ),
         ep_share = epspx / ajex ,  # Earnings per share
  )

# Lag variables by one year
compustat_lag <- compustat %>%
  select(gvkey, year, at, capx, ppegt, rect, xpp, drc,drlt,ap,xacc,act, che, lct,dlc, txp, wc, nco, fin, pstkrv, wcap, nca,nco, invt, noa, ep_share, ni) %>%
  mutate(year = year + 1) %>%
  rename_with(.cols = at:ni, ~ paste0(.x, "_lag"))

# Lag variables by two years
compustat_lag2 <- compustat %>%
  select(gvkey, year, ni) %>%
  mutate(year = year + 2) %>%
  rename_with(.cols = ni:ni, ~ paste0(.x, "_lag2"))

# Remerge
compustat <- compustat %>%
  left_join(compustat_lag, by = c("gvkey", "year"))

compustat <- compustat %>%
  left_join(compustat_lag2, by = c("gvkey", "year"))

rm(compustat_lag, compustat_lag2)

# Compute change in variables 
compustat <- compustat %>%
  mutate(c_rect = (rect - rect_lag),
         c_invt = (invt - invt_lag),
         c_xpp = (xpp - xpp_lag),
         c_drc = (drc - drc_lag),
         c_drlt = (drlt - drlt_lag),
         c_ap = (ap - ap_lag),
         c_xacc = (xacc - xacc_lag),
         c_txp = (txp - txp_lag),
         c_pstkrv = (pstkrv - pstkrv_lag),
         c_wcap = (wcap - wcap_lag),
         c_nca = (nca - nca_lag),
         c_nco = (nco - nco_lag),
         c_rect = if_else(is.na(c_rect), 0, c_rect), # TODO Wollen wir NA changes auf Null setzen? Wieso?
         c_invt = if_else(is.na(c_invt), 0, c_invt),
         c_xpp = if_else(is.na(c_xpp), 0, c_xpp),
         c_drc = if_else(is.na(c_drc), 0, c_drc),
         c_drlt = if_else(is.na(c_drlt), 0, c_drlt),
         c_ap = if_else(is.na(c_ap), 0, c_ap),
         c_xacc = if_else(is.na(c_xacc), 0, c_xacc),
         c_txp = if_else(is.na(c_txp), 0, c_txp)
  ) 

# 1. Earnings Predictability

compustat2 <- compustat %>%
  select(gvkey, month, ep_share, ep_share_lag) %>%
  arrange(gvkey, month) %>%
  group_by(gvkey) %>%
  drop_na(ep_share, ep_share_lag)  %>%
  mutate(number = 1, 
         index = cumsum(number),
         max_index = max(index)) %>%
  subset(max_index >= 10) %>%
  select(gvkey, month, ep_share, ep_share_lag)

# Get residuals 

rsd <- function(compustat2){
  lm(formula = ep_share~ep_share_lag, data = as.data.frame(compustat2), na.action = na.omit) %>%
    resid() %>%
    return()
}

width <- 10

residual_data <- compustat2 %>% 
  arrange(gvkey, month) %>%
  group_by(gvkey) %>%
  group_modify(~ {
    cbind(., rollapplyr(.[c("ep_share", "ep_share_lag")], width, rsd, fill = NA, by.column = FALSE))
  }) %>%
  ungroup

# Calculate Volatility of Residuals 

residual_data <- residual_data %>%
  arrange(gvkey, month) %>%  
  mutate(sv_eprd = rowSds(as.matrix(.[5:14]))) %>%
  select(gvkey, month, sv_eprd)
  
# Remerge
  compustat <- compustat %>%
  left_join(residual_data, by = c("gvkey", "month"))

rm(compustat2,residual_data )

# 2. Real Estate Ratio 

compustat2 <- compustat %>%
  select(gvkey, month, year, ppenb, ppenls, ppent, fatb, fatl, ppegt, siccd) %>%
  arrange(gvkey, month) %>%
  group_by(gvkey) %>%
  mutate(rer = ifelse(year <= 1983, (ppenb + ppenls) / ppent, ifelse(year >= 1984, (fatb + fatl) / ppegt , as.numeric(NA))),  # Real estate ratio
         sic = as.numeric(substr(siccd, 1, 2))
         )

compustat2 <- compustat2 %>%
  select(gvkey, month, year, rer, sic) %>%
  arrange(year, gvkey) %>%
  group_by(year) %>%
  mutate(rer = ifelse(rer == "NaN", as.numeric(NA), rer),
         rer_win = winsorizor(rer,0.01)) %>%
  select (gvkey, month, year, rer_win, sic)

rer_data <- compustat2 %>%
  arrange(sic, year) %>%
  group_by(sic,year) %>%
  mutate(rer_ind = mean(rer_win, na.rm = TRUE),
         number = 1, 
         index=cumsum(number),
         max_index = max(index),
         sv_rer = ifelse(max_index >= 5, rer_win - rer_ind, as.numeric(NA)),
         sv_rer = ifelse(is.na(sic), as.numeric(NA),sv_rer)
         ) %>% 
  ungroup() %>%
  select(gvkey, month, sv_rer)
  
# Remerge
compustat <- compustat %>%
  left_join(rer_data, by = c("gvkey", "month"))

rm(compustat2,rer_data )

# 3. Ohlson's O-score 

compustat2 <- compustat %>%
  arrange(gvkey, month) %>%
  mutate(log_ta = log(at), 
         tlta = (dlc + dltt) / at , 
         wcta = (act - lct) / at, 
         clca = (lct) / act , 
         oeneg = if_else(lt > at, 1, 0),
         nita = ni / at, 
         futl = (pi +dp) / lt, 
         in2 = if_else(ni_lag<0, 1, 0),
         chin = (ni - ni_lag) / (abs(ni) + abs(ni_lag))
)
  
# Winsorize 
  
compustat2 <- compustat2 %>%
    arrange(year, gvkey) %>%
    group_by(year) %>%
    mutate(log_ta_win = winsorizor(log_ta,0.01), 
         tlta_win = winsorizor(tlta,0.01), 
         wcta_win = winsorizor(wcta,0.01), 
         clca_win = winsorizor(clca,0.01), 
         nita_win = winsorizor(nita,0.01), 
         futl_win = winsorizor(futl,0.01),
         chin_win = winsorizor(chin,0.01)
  )

compustat2 <- compustat2 %>%
  arrange(year, gvkey) %>%
  group_by(gvkey) %>%
  mutate(sv_o = -1.32 - (0.407*log_ta_win) + (6.03 * tlta_win) - (1.43 * wcta_win) + (0.076 *clca_win) - (1.72 * oeneg) - (2.37 * nita_win) - (1.83 * futl_win) + (0.285 * in2) - (0.521 * chin_win), # Ohlson O-score
  ) %>%
  ungroup %>%
  select(gvkey, month, sv_o)

# Remerge
compustat <- compustat %>%
  left_join(compustat2, by = c("gvkey", "month"))

rm(compustat2)

# 4. Compute additional variables

sorting_variables_comp_y <- compustat %>%
  mutate(sv_ag = (at - at_lag) / at_lag,                                        # Asset growth 
         sv_ig = (capx - capx_lag) / capx_lag,                                  # Investment growth
         sv_iva = ((ppegt - ppegt_lag) + (invt - invt_lag) / at_lag),           # Investments to assets 
         sv_gpa = (revt - cogs) / at,                                           # Gross profits to assets
         sv_be = coalesce(seq, ceq + pstk, at - lt) +                           # Book equity
           coalesce(txditc, txdb + itcb, 0) -
           coalesce(pstkrv, pstkl, pstk, 0),
         filter_be = sv_be,
         sv_be = if_else(sv_be <= 0, as.numeric(NA), sv_be),
         sv_bm = sv_be / mktcap ,                                               # Book to market equity 
         sv_ep = ifelse(ib >= 0, ib / mktcap , as.numeric(NA)) ,                # Earnings to price 
         sv_cfp = ifelse((ib + dp + replace_na(txdi,0)) >= 0, (ib + dp + replace_na(txdi,0)) / mktcap , as.numeric(NA)),    # Cash flow to price 
         sv_npy = ((dvc + prstkc + ifelse(c_pstkrv <0, c_pstkrv, 0)) - sstk - ifelse(c_pstkrv >0, c_pstkrv, 0)) / mktcap ,                                                  # Net payout yield
         sv_sp = ifelse(sale >= 0, sale / mktcap , as.numeric(NA)) ,            # Sales to price
         op_cf = ifelse(year < 1988, fopt - c_wcap, ifelse(year >= 1988, oancf , as.numeric(NA))),
         sv_ocp = ifelse(op_cf >= 0, op_cf / mktcap , as.numeric(NA)) ,         # Operating cash flow to price
         sv_cbop = ifelse(year < 1988, ((revt - cogs - xsga + replace_na(xrd,0)) - c_rect - c_invt - c_xpp + c_drc + c_drlt + c_ap + c_xacc ) /at , ifelse(year >= 1988, ((revt - cogs - xsga + replace_na(xrd,0)) - recch - invch + apalch) / at , as.numeric(NA))),  # Cash based operating profitability 
         sv_noa = ((at - che) - (at - dlc - dltt - mib - pstk - ceq)) / at_lag, # Net operating assets 
         sv_ivg = (invt - invt_lag) / invt_lag ,                                # Inventory growth
         sv_ivc = (invt - invt_lag) / ((at + at_lag) / 2) ,                     # Inventory changes
         sv_dnca = c_nca / at_lag,                                              # Change in noncurrent operating assets
         sv_dnco = c_nco / at_lag,                                              # Change in net noncurrent operating assets
         sv_dpia = ((ppegt - ppegt_lag) + (invt - invt_lag)) / at_lag ,         # Change in PPE and inventory to assets       
         sv_dwc = (wc - wc_lag) / at_lag,                                       # Change in net noncash working capital
         sv_oa = ifelse(year < 1988, ((act - act_lag) - (che - che_lag) - (lct - lct_lag) + (dlc - dlc_lag) + c_txp - replace_na(dp,0)) /at_lag , ifelse(year >= 1988, (ni - oancf) / at_lag , as.numeric(NA))),  # Operating accruals 
         sv_poa = ifelse(year < 1988, ((act - act_lag) - (che - che_lag) - (lct - lct_lag) + (dlc - dlc_lag) + c_txp - replace_na(dp,0)) /abs(ni) , ifelse(year >= 1988, (ni - oancf) / abs(ni) , as.numeric(NA))),  # Percent operating accruals 
         sv_pta = ifelse(year < 1988, ((wc - wc_lag) + (nco - nco_lag) + (fin - fin_lag)) /abs(ni) , ifelse(year >= 1988, (ni - oancf - ivncf - fincf +replace_na(sstk,0) - replace_na(prstkc,0) -replace_na(dv,0)) / abs(ni) , as.numeric(NA))),  # Percent total accruals 
         sv_adm = ifelse(xad >= 0, xad / mktcap , as.numeric(NA)) ,             # Advertisement expense to market ratio
         sv_rdm = ifelse(xrd >= 0, xrd / mktcap , as.numeric(NA)) ,             # R&D expense to market ratio
         sv_ol = (cogs + xsga) / at,                                            # Operating leverage 
         sv_ato = (sale / noa_lag),                                             # Asset turnover
         sv_cto = (sale/ at_lag ),                                              # Capital turnover
         sv_dm = ifelse((dlc + dltt) == 0, as.numeric(NA),(dlc + dltt) / mktcap), # Debt to market equity
         filter_earnings = ib,                                                  # Earnings 
         filter_price = prcc_f                                                  # Fiscal year closing price
         ) %>%
  select(gvkey, month, datadate, starts_with("filter_"), starts_with("sv_")) %>%
  select(-sv_be)


# Filters -----------------------------------------------------------------

# Remove Inf and NaN
sorting_variables_comp_y <- sorting_variables_comp_y %>%
  mutate(across(starts_with("sv_"), ~ na_if(., Inf)),
         across(starts_with("sv_"), ~ na_if(., -Inf)),
         across(starts_with("sv_"), ~ na_if(., NaN)))

# TODO: should we also trim stupidely large/small values? (note, winsorization would not change anything)


# Store variables ---------------------------------------------------------

# Store
sorting_variables_comp_y %>%
  dbWriteTable(data_nse, "sorting_variables_comp_y", ., overwrite = TRUE)




