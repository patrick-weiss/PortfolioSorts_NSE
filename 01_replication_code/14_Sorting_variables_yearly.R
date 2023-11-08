# Construction of yearly sorting variables 


# Packages and Setup ---------------------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)
library(slider)
library(dbplyr)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Annual Variables -----------------------------------------------------------
# Load compustat and CRSP
compustat <- dbReadTable(data_nse, "compustat")  
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")  

# Bring in CRSP data 
compustat <- compustat |> 
  left_join(crsp_monthly |> 
              select(gvkey, month, mktcap, siccd), 
            by = c("gvkey", "month")) 

# Compute sub-variables 
compustat <- compustat |> 
  mutate(wc = (act - che) - (lct -  replace_na(dlc, 0)), 
         coa = act - che,                                                                                                                                     # Current operating assets 
         col = (lct -  replace_na(dlc, 0)),                                                                                                                   # Current operating liabilities 
         nco = (at - act -  replace_na(ivao, 0)) - (lt - lct -  replace_na(dltt, 0)),
         fin = (replace_na(ivst, 0) +  replace_na(ivao, 0)) - ( replace_na(dltt, 0) + replace_na(dlc, 0) + replace_na(pstk, 0)),                              # Net financial assets 
         fnl = (replace_na(dltt, 0) + replace_na(dlc, 0) + replace_na(pstk, 0)),                                                                              # Financial liabilities         
         nca = at - act - replace_na(ivao, 0),
         ncl = lt - lct - replace_na(dltt, 0),
         nco = nca - ncl, 
         noa_sol = (at - che - replace_na(ivao, 0)) - (at - replace_na(dlc, 0) - replace_na(dltt, 0) - replace_na(mib, 0) - replace_na(pstk, 0) - ceq),
         nd = ((dltt + dlc + pstk + replace_na(dvpa, 0) - replace_na(tstkp, 0)) - (che)),                                                                        # Net book debt  
         ndb = ((dltt + dlc + pstk + replace_na(dvpa, 0) - replace_na(tstkp, 0)) - (che)) + (ceq + replace_na(tstkp, 0) - replace_na(dvpa, 0)),                   # Net book debt plus book equity                                                   
         ndm = ((dltt + dlc + pstk + replace_na(dvpa, 0) - replace_na(tstkp, 0)) - (che)) + mktcap,                                                             # Net book debt plus market equity      
         ev = mktcap + dlc + dltt + pstkrv - che,                                                                                                              # Enterprise value 
         noa = ((at - che) - (at - replace_na(dlc, 0) - replace_na(dltt, 0) - replace_na(mib, 0) - replace_na(pstk, 0) - ceq)),              # Net operating assets 
         lno = ppent + intan + ao - lo + dp,                                                                                                                   # Long-term net operating assets 
         ce = if_else(sale >= 10, capx / sale, NA_real_),                                                                                                 # Capital expenditures scaled by sales 
         op = revt - replace_na(cogs, 0) - replace_na(xsga, 0) - replace_na(xint, 0),                                                                           # Operating profits   
         wcta = (act - lct) / at,
         reta = re / at, 
         ebitta = oiadp / at, 
         metl = mktcap / lt, 
         saleta = sale / at, 
         saso = csho * ajex,                                                                                                                                  # Split adjusted shares outstanding
         debt = if_else((dlc + dltt) <= 0, NA_real_, dlc + dltt),                                                                                        # Book value of debt     
         ta_after_1988 = ni - oancf - ivncf - fincf + replace_na(sstk, 0) - replace_na(prstkc, 0) - replace_na(dv, 0),                                          # Total accruals after 1988
         invent = coalesce(invfg, invt),                                                                                                                      # Inventories
         ebt = pi + am,                                                                                                                                       # Earnings before taxes 
         eps = epspx / ajex,   
         ep_share = epspx / ajex, 
         me = (at + (prcc_f * csho) - ceq))                                                                                                                   # Market value of assets    


# Lag variables --------------------------------------------------------------
# Lag variables by one year
compustat_lag <- compustat |> 
  select(gvkey, year, at, capx, ppegt, rect, xpp, drc, drlt, ap, xacc, act, che, lct, dlc, dltt, txp, wc, nco, fin, pstkrv, wcap, nca, nco, invt, noa, ep_share, noa, noa_sol, lno, coa, col, ivst, ivao, pstk, fnl, ceq, ce, emp, sale, saso, capxv, xad, xrd, invent, ebt, txt, eps, prcc_f, me, ppent, ni) |> 
  mutate(year = year + 1) |> 
  rename_with(.cols = at:ni, ~ paste0(.x, "_lag"))

# Lag variables by two years
compustat_lag2 <- compustat |> 
  select(gvkey, year, ni, capx, capxv, xrd, sale, invent, ebt, txt, ce) |>
  mutate(year = year + 2) |>
  rename_with(.cols = ni:ce, ~ paste0(.x, "_lag2"))

# Lag variables by three years
compustat_lag3 <- compustat |>
  select(gvkey, year, capx, capxv, xrd, ebt, txt, ce) |>
  mutate(year = year + 3) |>
  rename_with(.cols = capx:ce, ~ paste0(.x, "_lag3"))

# Lag variables by four years
compustat_lag4 <- compustat |>
  select(gvkey, year, capx, capxv, xrd, ce) |>
  mutate(year = year + 4) |>
  rename_with(.cols = capx:ce, ~ paste0(.x, "_lag4"))

# Lag variables by five years
compustat_lag5 <- compustat |>
  select(gvkey, year, debt) |>
  mutate(year = year + 5) |>
  rename_with(.cols = debt:debt, ~ paste0(.x, "_lag5"))

# Remerge
compustat <- compustat |> 
  left_join(compustat_lag, by = c("gvkey", "year")) |> 
  left_join(compustat_lag2, by = c("gvkey", "year")) |> 
  left_join(compustat_lag3, by = c("gvkey", "year")) |> 
  left_join(compustat_lag4, by = c("gvkey", "year")) |> 
  left_join(compustat_lag5, by = c("gvkey", "year"))

# Free Space
rm(compustat_lag, compustat_lag2, compustat_lag3, compustat_lag4, compustat_lag5)

# Compute change in variables and dummy variables for missing observations 
compustat <- compustat |>
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
         c_noa = (noa - noa_lag), 
         c_lno = (lno - lno_lag), 
         c_coa = (coa - coa_lag), 
         c_col = (col - col_lag),
         c_fin = (fin - fin_lag), 
         c_fnl = (fnl - fnl_lag), 
         c_ceq = (ceq - ceq_lag),
         c_wc = (wc - wc_lag),
         c_ivao = (replace_na(ivao, 0) - replace_na(ivao_lag, 0)),
         c_rect = replace_na(c_rect, 0),
         c_invt = replace_na(c_invt, 0),
         c_xpp = replace_na(c_xpp, 0),
         c_drc = replace_na(c_drc, 0),
         c_drlt = replace_na(c_drlt, 0),
         c_ap = replace_na(c_ap, 0),
         c_xacc = replace_na(c_xacc, 0),
         c_txp = replace_na(c_txp, 0),
         c_eps = (eps - eps_lag) / prcc_f_lag,
         tp = (dvc + prstkc + if_else(c_pstkrv < 0, c_pstkrv, 0)),                                                                                              # Total payouts   
         d_na_ivst = if_else(is.na(ivst) & is.na(ivst_lag), 1, 0),
         d_na_ivao = if_else(is.na(ivao) & is.na(ivao_lag), 1, 0),
         d_na_dltt = if_else(is.na(dltt) & is.na(dltt_lag), 1, 0),
         d_na_dlc = if_else(is.na(dlc) & is.na(dlc_lag), 1, 0),
         d_na_pstk = if_else(is.na(pstk) & is.na(pstk_lag), 1, 0),
         d_na_cogs = if_else(is.na(cogs), 1, 0),
         d_na_xsga = if_else(is.na(xsga), 1, 0),
         d_na_xint = if_else(is.na(xint), 1, 0),
         rc = xrd + (0.8 * xrd_lag) + (0.6 * xrd_lag2) + (0.4 * xrd_lag3) + (0.2 * xrd_lag4),                                                                  # R&D capital         
         rc = if_else(rc < 0, NA_real_, rc), 
         e_sale = if_else(sale < 0, NA_real_, ((sale_lag + sale_lag2) / 2)),                                                                              # Expected sales 
         e_invent = (invent_lag + invent_lag2) / 2,                                                                                                            # Expected inventory
         tax_exp_av = (1/3) * ((txt_lag / ebt_lag) + (txt_lag2 / ebt_lag2) + (txt_lag3 / ebt_lag3)),                                                          # Average tax expenses over the last three years 
         op_cf = if_else(year < 1988, fopt - c_wcap, if_else(year >= 1988, oancf, NA_real_)),   
         log_ta = log(at), 
         tlta = (dlc + dltt) / at, 
         clca = (lct) / act, 
         oeneg = if_else(lt > at, 1, 0),
         nita = ni / at, 
         futl = (pi +dp) / lt, 
         in2 = if_else(ni_lag<0 & ni_lag2 <0, 1, 0),
         chin = (ni - ni_lag) / (abs(ni) + abs(ni_lag))
 ) 

# Winsorization function
winsorize <- function(x, cut) {
  x <- replace(
    x,
    x > quantile(x, 1 - cut, na.rm = T),
    quantile(x, 1 - cut, na.rm = T)
 )
  x <- replace(
    x,
    x < quantile(x, cut, na.rm = T),
    quantile(x, cut, na.rm = T)
 )
  return(x)
}

# Winsorize sub-variables for Altman Z-Score, O-Score
compustat <- compustat |> 
  group_by(year) |> 
  mutate(across(c(wcta, reta, ebitta, metl, saleta, log_ta, tlta, clca, nita, futl, chin), ~winsorize(.x, 0.01), .names = "{.col}_win")) |>                                                          # Winsorize sub-variables for the Altman Z-score) |> 
  ungroup()

# Earnings predictability ----------------------------------------------------
# Residual function
compute_residual_sd <- function(data, min_obs) { 
  data <- data |> drop_na()
  
  if(nrow(data) < min_obs) return(NA)
  
  residuals <- data |> 
    lm(formula = ep_share ~ ep_share_lag, data = _) |> 
    resid()
  
  if(length(residuals) < min_obs) {
    return(NA) 
  } else{
    return(sd(residuals, na.rm = TRUE))
  }
}

# Rolling function
roll_res <- function(data, years, min_obs) {
  data <- bind_rows(data)
  
  variable <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "year",
    .f = ~ compute_residual_sd(.x, min_obs = min_obs),
    .before = years - 1,
    .complete = FALSE
 )
  
  return(variable)
}


# Compute sorting variable
compustat_ep <- compustat |> 
  select(gvkey, month, ep_share, ep_share_lag) |> 
  group_by(gvkey) |> 
  arrange(month) |> 
  mutate(sv_eprd = roll_res(pick(month, ep_share, ep_share_lag), 
                            years = 10, 
                            min_obs = 10)) |> 
  ungroup() |> 
  select(gvkey, month, sv_eprd) |> 
  drop_na()
  
# Remerge
compustat <- compustat |> 
  left_join(compustat_ep, by = c("gvkey", "month"))

# Free space
rm(compustat_ep)


# Real estate ratio ---------------------------------------------------------- 
# Compute pre-data
compustat_rer <- compustat |> 
  select(gvkey, month, year, ppenb, ppenls, ppent, fatb, fatl, ppegt, siccd) |> 
  mutate(rer = if_else(year <= 1983, (ppenb + ppenls) / ppent, if_else(year >= 1984, (fatb + fatl) / ppegt, NA_real_)),  # Real estate ratio
         rer = if_else(is.nan(rer), NA_real_, rer), 
         sic = substr(siccd, 1, 2)) |> 
  select(gvkey, month, year, rer, sic) |> 
  group_by(year) |> 
  mutate(rer_win = winsorize(rer, 0.01)) |> 
  ungroup() |> 
  select (gvkey, month, year, rer_win, sic) |> 
  drop_na()
  
# Compute sorting variable
compustat_rer <- compustat_rer |> 
  group_by(sic, year) |> 
  mutate(rer_ind = mean(rer_win),
         n_ind = n(),
         sv_rer = rer_win - rer_ind, 
         sv_rer = if_else(month >= as.Date("1970-01-01"), sv_rer, NA_real_)) |> 
  ungroup() |> 
  filter(n_ind >= 5) |> 
  select(gvkey, month, sv_rer) |> 
  drop_na()

# Remerge
compustat <- compustat |> 
  left_join(compustat_rer, by = c("gvkey", "month"))

# Free space
rm(compustat_rer)


# Whited and Wu index of financing constraints -------------------------------
# Preliminary computations
compustat_ww <- compustat |> 
  mutate(cf = ((ib + dp) / at) / 4, 
         divpos = if_else(dvpsx_f > 0 & !is.na(dvpsx_f), 1, 0), 
         tltd = dltt / at, 
         lnta = log(at * 1000000), # in millions                                             
         sg =  ((1+ ((sale - sale_lag) / sale_lag))^(1/4)) -1, 
         industry = substr(siccd, 1, 3)) |> 
  select(gvkey, month, cf, divpos, tltd, lnta, sg, industry, sale, sale_lag, year)

# Annual sales per 3-digit industries
compustat_sales <- compustat |> 
  mutate(industry = substr(siccd, 1, 3)) |> 
  group_by(industry, year) |> 
  summarize(sales_ind = sum(sale),
            sales_ind_lag = sum(sale_lag),
            sales_growth = (sales_ind - sales_ind_lag) / sales_ind_lag,
            isg = ((1+ sales_growth)^(1/4)) - 1,
            n = n(),
            .groups = 'drop') |> 
  filter(n >= 2) |> 
  select(industry, year, isg)
  
# Merge & winsorize
compustat_ww <- compustat_ww |> 
  left_join(compustat_sales, by = c("industry", "year")) |> 
  group_by(year) |> 
  mutate(across(c(cf, tltd, lnta, isg, sg), ~winsorize(.x, 0.01), .names = "{.col}_win")) |> 
  ungroup()

# Compute sorting variable
compustat_ww <- compustat_ww |> 
  mutate(sv_ww = (-0.091 * cf_win) - (0.062 * divpos) + (0.021 * tltd_win) - (0.044 * lnta_win) + (0.102 * isg_win) - (0.035 * sg_win)) |> 
  select(gvkey, month, sv_ww) |> 
  drop_na()

# Remerge
compustat <- compustat |> 
  left_join(compustat_ww, by = c("gvkey", "month"))

# Free memory
rm(compustat_ww, compustat_sales)


# Compute additional variables -----------------------------------------------
# Compute other sorting variables
sorting_variables_comp_y <- compustat |> 
  mutate(sv_be = coalesce(seq, ceq + pstk, at - lt) + coalesce(txditc, txdb + itcb, 0) - coalesce(pstkrv, pstkl, pstk, 0),                                                 # Book equity                                                                                                                                      # Fiscal year closing price
         filter_be = sv_be,
         sv_be = if_else(sv_be <= 0, NA_real_, sv_be),                                                                                                                                        
         sv_ato = if_else(noa_sol_lag > 0, (sale / noa_sol_lag), NA_real_),                                                                                                 # Asset turnover
         sv_bl = if_else(at < 0, NA_real_, at / sv_be),                                                                                                                    # Book leverage 
         sv_cbop = ((revt - cogs - xsga + replace_na(xrd, 0)) - c_rect - c_invt - c_xpp + c_drc + c_drlt + c_ap + c_xacc) /at,                                             # Cash based operating profitability 
         sv_cto = (sale/ at_lag),                                                                                                                                          # Capital turnover
         sv_gpa = (revt - cogs) / at,                                                                                                                                      # Gross profits to assets
         sv_o = -1.32 - (0.407*log_ta_win) + (6.03 * tlta_win) - (1.43 * wcta_win) + (0.076 *clca_win) - (1.72 * oeneg) 
                - (2.37 * nita_win) - (1.83 * futl_win) + (0.285 * in2) - (0.521 * chin_win),                                                                              # Ohlson O-Score
         sv_ope = if_else(d_na_cogs == 1 & d_na_xsga == 1 & d_na_xint == 1, NA_real_, op / sv_be),                                                                         # Operating profits to equity
         sv_tbi = if_else (pi < 0 | ni < 0, NA_real_, pi / ni),                                                                                                            # Taxable income to book income
         sv_z = (1.2 * wcta_win) + (1.4 * reta_win) + (3.3 * ebitta_win) + (0.6 * metl_win) + saleta_win,                                                                  # Altmans' Z-score 
         sv_am = if_else(at > 0, at / mktcap, NA_real_),                                                                                                                   # Assets to market equity 
         sv_bm = sv_be / mktcap,                                                                                                                                           # Book to market ratio   
         sv_cfm = if_else((ib + dp) >= 0, (ib + dp) / mktcap, NA_real_),                                                                                                   # Cash flow to price 
         sv_dm = if_else((dlc + dltt) == 0, NA_real_,(dlc + dltt) / mktcap),                                                                                               # Debt to market equity
         sv_ebm = if_else(ndb < 0 | ndm < 0, NA_real_, ndb / ndm),                                                                                                         # Enterprise book to market equity 
         sv_em = if_else(ib >= 0, ib / mktcap, NA_real_),                                                                                                                  # Earnings to price 
         sv_ndm = if_else(nd < 0, NA_real_, nd / mktcap),                                                                                                                  # Net debt to market equity 
         sv_npy = ((dvc + prstkc + if_else(c_pstkrv < 0, c_pstkrv, 0)) - sstk - if_else(c_pstkrv >0, c_pstkrv, 0)) / mktcap,                                                # Net payout yield
         sv_npy = if_else(sv_npy > 0, sv_npy, NA_real_),                                                                                          
         sv_npy = if_else(month >= as.Date("1972-01-01"), sv_npy, NA_real_),
         sv_ocm = if_else(op_cf >= 0, op_cf / mktcap, NA_real_),                                                                                                           # Operating cash flow to price
         sv_ocm = if_else(month >= as.Date("1972-01-01"), sv_ocm, NA_real_),
         sv_sm = if_else(sale >= 0, sale / mktcap, NA_real_),                                                                                                              # Sales to price
         sv_aci = (ce / ((ce_lag + ce_lag2 + ce_lag3) / 3)) - 1,                                                                                                            # Abnormal corporate investment 
         sv_ag = (at - at_lag) / at_lag,                                                                                                                                   # Asset growth 
         sv_dnoa = c_noa / at_lag,                                                                                                                                         # Change in net operating assets 
         sv_dpia = ((ppegt - ppegt_lag) + (invt - invt_lag)) / at_lag,                                                                                                     # Change in PPE and inventory to assets       
         sv_dwc = (wc - wc_lag) / at_lag,                                                                                                                                  # Change in net noncash working capital
         sv_ig = (capx - capx_lag) / capx_lag,                                                                                                                             # Investment growth
         sv_dinv = (invt - invt_lag) / ((at + at_lag) / 2),                                                                                                                # Inventory changes
         sv_noa = noa / at_lag,                                                                                                                                            # Net operating assets 
         sv_oa = if_else(year < 1988, ((act - act_lag) - (che - che_lag) - (lct - lct_lag) + (dlc - dlc_lag) + c_txp - replace_na(dp, 0)) / at_lag, 
                         if_else(year >= 1988, (ni - oancf) / at_lag, NA_real_)),                                                                                          # Operating accruals 
         sv_pta = if_else(year < 1988, ((wc - wc_lag) + (nco - nco_lag) + (fin - fin_lag)) / abs(ni), 
                          if_else(year >= 1988, (ni - oancf - ivncf - fincf + replace_na(sstk, 0) - replace_na(prstkc, 0) - replace_na(dv, 0)) / abs(ni), NA_real_)),      # Percent total accruals 
         sv_cdi = log(debt / debt_lag5),                                                                                                                                   # Composite debt issuance 
         sv_dbe = c_ceq / at_lag,                                                                                                                                          # Change in book equity 
         sv_dcol = c_col / at_lag,                                                                                                                                         # Change in current operating liabilities
         sv_dfnl = if_else(d_na_dltt == 1 & d_na_dlc == 1 & d_na_pstk == 1, NA_real_, c_fnl / at_lag),                                                                     # Change in financial liabilities  
         sv_ndf = (dltis - dltr + replace_na(dlcch, 0)) / ((at + at_lag)/2),                                                                                               # Net debt financing
         sv_ndf = if_else(month >= as.Date("1972-01-01"), sv_ndf, NA_real_),
         sv_nef = (sstk - prstkc - dv) / ((at + at_lag)/2),                                                                                                                # Net equity financing 
         sv_nef = if_else(month >= as.Date("1972-01-01"), sv_nef, NA_real_),
         sv_nxf = ((sstk - prstkc - dv) + (dltis - dltr + replace_na(dlcch, 0))) / ((at + at_lag)/2),                                                                      # Net external financing 
         sv_nxf = if_else(month >= as.Date("1972-01-01"), sv_nxf, NA_real_),
         sv_adm = if_else(xad >= 0, xad / mktcap, NA_real_),                                                                                                               # Advertisement expense to market ratio
         sv_adm = if_else(month >= as.Date("1973-01-01"), sv_adm, NA_real_),
         sv_hr = if_else(emp == 0, NA_real_, (emp - emp_lag) / ((0.5 * emp) + (0.5 * emp_lag))),                                                                           # Hiring rate 
         sv_kzi = (-1.002 * ((ib + dp) / ppent_lag)) + (0.283 * ((at + mktcap - ceq - txdb) / at)) + (3.139 * ((dlc + dltt) / (dlc + dltt + seq)))                         # Kaplan and Zingales Index
                   - (39.368 * ((dvc + dvp) / ppent_lag)) - (1.315 * (che / ppent_lag)),  
         sv_lfe = ((sale / emp) - (sale_lag / emp_lag)) / (sale_lag / emp_lag),                                                                                            # Labor force efficiency 
         sv_ol = (cogs + xsga) / at,                                                                                                                                       # Operating leverage 
         sv_rdm = if_else(xrd >= 0, xrd / mktcap, NA_real_),                                                                                                               # R&D expense to market ratio
         sv_rdm = if_else(month >= as.Date("1976-01-01"), sv_rdm, NA_real_),
         sv_tan = (che + (0.715 * rect) + (0.547 * invt) + (0.535 * ppegt)) / at,                                                                                          # Tangibility 
         filter_earnings = ib,                                                                                                                                             # Earnings 
         filter_price = prcc_f
        ) |> 
  select(gvkey, month, datadate, starts_with("filter_"), starts_with("sv_")) |> 
  select(-sv_be)


# Filters -----------------------------------------------------------------
# Remove Inf and NaN
sorting_variables_comp_y <- sorting_variables_comp_y |> 
  mutate(across(starts_with("sv_"), ~ na_if(., Inf)),
         across(starts_with("sv_"), ~ na_if(., -Inf)),
         across(starts_with("sv_"), ~ na_if(., NaN)))


# Store variables ---------------------------------------------------------

# Store
sorting_variables_comp_y |> 
  dbWriteTable(conn = data_nse, 
               name = "sorting_variables_comp_y", 
               value = _, 
               overwrite = TRUE) 
