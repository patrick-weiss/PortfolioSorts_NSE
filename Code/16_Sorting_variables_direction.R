# Direction of high minus low portfolio  


# Packages and Setup ------------------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 
# Direction of high minus low portfolio------------------------------------
direction_hml_portfolio <- data.frame(matrix(ncol = 0, nrow = 1)) |> 
  mutate(sv_abr_d = 1,                                                          # Abnormal returns around earnings announcements
         sv_mom_d = 1,                                                          # Momentum (11 months)
         sv_rmom_d = 1,                                                         # Residual momentum (11 months)
         sv_rs_d = 1,                                                           # Revenue surprises 
         sv_sue_d = 1,                                                          # Standardized unexpected earnings 
         sv_tes_d = 1,                                                          # Tax expense surprises 
         sv_52w_d = 1,                                                          # 52 week high
         sv_me_d = -1,                                                          # Size 
         sv_ato_d = 1,                                                          # Asset turnover
         sv_bl_d = -1,                                                          # Book leverage
         sv_cbop_d = 1,                                                         # Cash based operating profitability 
         sv_cto_d = 1,                                                          # Capital turnover
         sv_gpa_d = 1,                                                          # Gross profits to assets
         sv_o_d = -1,                                                           # Ohlson O-score
         sv_ope_d = 1,                                                          # Operating profits to book equity 
         sv_roa_d = 1,                                                          # Quarterly return on assets 
         sv_roe_d = 1,                                                          # Quarterly return on equity
         sv_tbi_d = 1,                                                          # Taxable income to book income 
         sv_z_d = -1,                                                           # Altmans' Z-score
         sv_am_d = 1,                                                           # Assets to market equity 
         sv_bm_d = 1,                                                           # Book to market equity 
         sv_cfm_d = 1,                                                          # Cash flow to price 
         sv_dm_d = 1,                                                           # Debt to market equity 
         sv_ebm_d = 1 ,                                                         # Enterprise book to market equity 
         sv_em_d = 1 ,                                                         # Enterprise multiple 
         sv_ndm_d = 1 ,                                                         # Net debt to market equity 
         sv_npy_d = 1 ,                                                         # Net payout yield
         sv_ocm_d = 1,                                                          # Operating cash flow to price 
         sv_rev_d = -1 ,                                                        # Long-term reversal 
         sv_sm_d = 1,                                                           # Sales to price 
         sv_aci_d = -1,                                                         # Abnormal corporate investment 
         sv_ag_d = -1,                                                          # Asset growth 
         sv_dinv_d = -1 ,                                                       # Inventory changes
         sv_dnoa_d = -1,                                                        # Change in net operating assets 
         sv_dpia_d = -1,                                                        # Change in PPE and inventory to assets 
         sv_dwc_d = -1,                                                         # Change in net noncash working capital 
         sv_ig_d = -1,                                                          # Investment growth
         sv_noa_d = -1,                                                         # Net operating assets 
         sv_oa_d = -1,                                                          # Operating accruals 
         sv_pta_d = -1,                                                         # Percent total accruals 
         sv_cdi_d = -1,                                                         # Composite debt issuance 
         sv_csi_d = -1,                                                         # Composite share issuance
         sv_dbe_d = -1,                                                         # Change in book equity 
         sv_dcol_d = -1,                                                        # Change in current operating liabilities 
         sv_dfnl_d = -1,                                                        # Change in financial liabilities
         sv_ndf_d = -1,                                                         # Net debt financing 
         sv_nef_d = -1,                                                         # Net equity financing
         sv_nxf_d = -1,                                                         # Net external financing
         sv_adm_d = 1,                                                          # Advertisement expense to market ratio
         sv_cfv_d = -1,                                                         # Cash flow volatility 
         sv_eprd_d = -1,                                                        # Earnings predictability
         sv_hr_d = -1,                                                          # Hiring rate 
         sv_kzi_d = -1,                                                         # Kaplan and Zingales index 
         sv_lfe_d = 1,                                                          # Labor force efficiency 
         sv_ol_d = 1,                                                           # Operating leverage 
         sv_rdm_d = 1 ,                                                         # R&D expense to market ratio
         sv_rer_d = 1,                                                          # Real estate ratio
         sv_tan_d = 1,                                                          # Tangibility 
         sv_ww_d = 1,                                                           # Whited and Wu index 
         sv_ami_d = 1,                                                          # Amihud illiquidity measure 
         sv_beta_d = -1,                                                        # Market beta
         sv_bfp_d = -1,                                                         # Frazzini and Pedersen beta
         sv_dtv_d = -1,                                                         # Dollar trading volume
         sv_iskew_d = -1,                                                       # Idiosyncratic skewness relative to FF3
         sv_ivol_d = -1,                                                        # Idiosyncratic volatility relative to FF3
         sv_mdr_d = -1,                                                         # Maximum daily return 
         sv_srev_d = -1,                                                        # Short term reversal 
         sv_tur_d = -1                                                          # Share turnover 
         ) |> 
  pivot_longer(everything(), names_to = "sv", values_to = "direction") |> 
  mutate(sv = substr(sv, 1, nchar(sv) - 2)) 

# Store variables ---------------------------------------------------------

# Store
direction_hml_portfolio |> 
  dbWriteTable(conn = data_nse, 
               name = "direction_hml_portfolio", 
               value = _, 
               overwrite = TRUE)

