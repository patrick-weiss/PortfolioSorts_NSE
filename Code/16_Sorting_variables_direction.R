# Direction of high minus low portfolio  


# Packages and Setup ------------------------------------------------------

# Packages 
library(tidyverse)
library(RSQLite)
library(DBI)


# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 
# Direction of high minus low portfolio------------------------------------

direction_hml_portfolio <- data.frame(matrix(ncol = 0, nrow = 1)) %>%
  mutate(identifier_direction = 1,                                              # Identifier to merge later with Compustat / CRSP Data
         sv_ag_d = -1,                                                          # Asset growth 
         sv_ig_d = -1,                                                          # Investment growth
         sv_iva_d = -1,                                                         # Investments to assets 
         sv_gpa_d = 1,                                                          # Gross profits to assets
         sv_bm_d = 1,                                                           # Book to market equity 
         sv_ep_d = 1 ,                                                          # Earnings to price 
         sv_cfp_d = 1,                                                          # Cash flow to price 
         sv_npy_d = 1 ,                                                         # Net payout yield
         sv_sp_d = 1,                                                           # Sales to price 
         sv_ocp_d = 1,                                                          # Operating cash flow to price 
         sv_cbop_d = 1,                                                         # Cash based operating profitability 
         sv_noa_d = -1,                                                         # Net operating assets 
         sv_ivg_d = -1 ,                                                        # Inventory growth
         sv_ivc_d = -1 ,                                                        # Inventory changes
         sv_dnca_d = -1,                                                        # Change in noncurrent operating assets 
         sv_dnco_d = -1,                                                        # Change in net noncurrent operating assets
         sv_dpia_d = -1,                                                        # Change in PPE and inventory to assets 
         sv_dwc_d = -1,                                                         # Change in net noncash working capital 
         sv_oa_d = -1,                                                          # Operating accruals 
         sv_poa_d = -1,                                                         # Percent operating accruals 
         sv_pta_d = -1,                                                         # Percent total accruals 
         sv_adm_d = 1,                                                          # Advertisement expense to market ratio
         sv_rdm_d = 1 ,                                                         # R&D expense to market ratio
         sv_ol_d = 1,                                                           # Operating leverage 
         sv_roaq_d = 1,                                                         # Quarterly return on assets 
         sv_roeq_d = 1,                                                         # Quarterly return on equity
         sv_olaq_d = 1,                                                         # Quarterly operating profits to assets 
         sv_atoq_d = 1,                                                         # Quarterly asset turnover 
         sv_size_d = -1,                                                        # Size 
         sv_mom_d = 1,                                                          # Momentum (11 months)
         sv_mom_6_d = 1,                                                        # Momentum (6 months)
         sv_e_6_d = 1,                                                          # Residual momentum (6 months)
         sv_e_11_6_d = 1,                                                       # Residual momentum (11 months)
         sv_csi_d = -1,                                                         # Composite share issuance
         sv_eprd_d = -1,                                                        # Earnings predictability
         sv_rer_d = 1,                                                          # Real estate ratio
         sv_cto_d = 1,                                                          # Capital turnover
         sv_ato_d = 1,                                                          # Asset turnover
         sv_o_d = -1,                                                           # Ohlson O-score
         sv_dm_d = 1,                                                           # Debt to market equity 
         sv_dtv_d = -1,                                                         # Dollar trading volume
         sv_ivolc_d = -1,                                                       # Idiosyncratic volatility relative to CAPM
         sv_ivolff_d = -1,                                                      # Idiosyncratic volatility relative to FF3
         sv_iscc_d = -1,                                                        # Idiosyncratic skewness relative to CAPM
         sv_iscff_d = -1,                                                       # Idiosyncratic skewness relative to FF3
         )

# Store variables ---------------------------------------------------------

# Store
direction_hml_portfolio %>%
  dbWriteTable(data_nse, "direction_hml_portfolio", ., overwrite = TRUE)

