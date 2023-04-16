# Significance in original paper                                                
# TRUE implies significance in original paper, FALSE implies no significance   


# Packages and Setup -----------------------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

# Significance in original paper -----------------------------------------------
# Significance table
significance_orig_paper <- data.frame(matrix(ncol = 0, nrow = 1)) |> 
  mutate(sv_abr_d = T,                                                          # Abnormal returns around earnings announcements
         sv_mom_d = T,                                                          # Momentum (TT months)
         sv_rmom_d = T,                                                         # Residual momentum (TT months)
         sv_rs_d = T,                                                           # Revenue surprises 
         sv_sue_d = T,                                                          # Standardized unexpected earnings 
         sv_tes_d = T,                                                          # Tax expense surprises 
         sv_52w_d = T,                                                          # 52 week high
         sv_me_d = T,                                                           # Size 
         sv_ato_d = F,                                                          # Asset turnover
         sv_bl_d = F,                                                           # Book leverage
         sv_cbop_d = T,                                                         # Cash based operating profitability 
         sv_cto_d = F,                                                          # Capital turnover
         sv_gpa_d = T,                                                          # Gross profits to assets
         sv_o_d = T,                                                            # Ohlson O-score
         sv_ope_d = T,                                                          # Operating profits to book equity 
         sv_roa_d = T,                                                          # Quarterly return on assets 
         sv_roe_d = T,                                                          # Quarterly return on equity
         sv_tbi_d = T,                                                          # Taxable income to book income 
         sv_z_d = F,                                                            # Altmans' Z-score
         sv_am_d = F,                                                           # Assets to market equity 
         sv_bm_d = T,                                                           # Book to market equity 
         sv_cfm_d = T,                                                          # Cash flow to price 
         sv_dm_d = T,                                                           # Debt to market equity 
         sv_ebm_d = T ,                                                         # Enterprise book to market equity 
         sv_em_d = T ,                                                          # Enterprise multiple 
         sv_ndm_d = T ,                                                         # Net debt to market equity 
         sv_npy_d = T ,                                                         # Net payout yield
         sv_ocm_d = T,                                                          # Operating cash flow to price 
         sv_rev_d = T ,                                                         # Long-term reversal 
         sv_sm_d = T,                                                           # Sales to price 
         sv_aci_d = T,                                                          # Abnormal corporate investment 
         sv_ag_d = T,                                                           # Asset growth 
         sv_dinv_d = T,                                                         # Inventory changes
         sv_dnoa_d = T,                                                         # Change in net operating assets 
         sv_dpia_d = T,                                                         # Change in PPE and inventory to assets 
         sv_dwc_d = T,                                                          # Change in net noncash working capital 
         sv_ig_d = T,                                                           # Investment growth
         sv_noa_d = T,                                                          # Net operating assets 
         sv_oa_d = T,                                                           # Operating accruals 
         sv_pta_d = T,                                                          # Percent total accruals 
         sv_cdi_d = T,                                                          # Composite debt issuance 
         sv_csi_d = T,                                                          # Composite share issuance
         sv_dbe_d = T,                                                          # Change in book equity 
         sv_dcol_d = T,                                                         # Change in current operating liabilities 
         sv_dfnl_d = T,                                                         # Change in financial liabilities
         sv_ndf_d = T,                                                          # Net debt financing 
         sv_nef_d = T,                                                          # Net equity financing
         sv_nxf_d = T,                                                          # Net external financing
         sv_adm_d = T,                                                          # Advertisement expense to market ratio
         sv_cfv_d = T,                                                          # Cash flow volatility 
         sv_eprd_d = F,                                                         # Earnings predictability
         sv_hr_d = T,                                                           # Hiring rate 
         sv_kzi_d = F,                                                          # Kaplan and Zingales index 
         sv_lfe_d = F,                                                          # Labor force efficiency 
         sv_ol_d = T,                                                           # Operating leverage 
         sv_rdm_d = T ,                                                         # R&D expense to market ratio
         sv_rer_d = T,                                                          # Real estate ratio
         sv_tan_d = F,                                                          # Tangibility 
         sv_ww_d = F,                                                           # Whited and Wu index 
         sv_ami_d = T,                                                          # Amihud illiquidity measure 
         sv_beta_d = T,                                                           # Market beta
         sv_bfp_d = T,                                                            # Frazzini and Pedersen beta
         sv_dtv_d = T,                                                          # Dollar trading volume
         sv_iskew_d = T,                                                        # Idiosyncratic skewness relative to FF3
         sv_ivol_d = T,                                                         # Idiosyncratic volatility relative to FF3
         sv_mdr_d = T,                                                          # Maximum daily return 
         sv_srev_d = T,                                                         # Short term reversal 
         sv_tur_d = T                                                           # Share turnover 
         ) |> 
  pivot_longer(everything(), names_to = "sv", values_to = "significance_orig_paper") |> 
  mutate(sv = substr(sv, 1, nchar(sv) - 2)) 


# Store variables ---------------------------------------------------------
# Store
significance_orig_paper |> 
  dbWriteTable(conn = data_nse, 
               name = "significance_orig_paper", 
               value = _, 
               overwrite = TRUE)

