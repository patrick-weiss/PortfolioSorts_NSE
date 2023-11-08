# Sorting variable groups


# Packages and Setup -----------------------------------------------
# Packages 
library(tidyverse)
library(RSQLite)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

# Sorting Variable Groups ------------------------------------------
sv_groups <- tibble(sv = "sv_abr", group = "Momentum") |>
  bind_rows(tibble(sv = "sv_mom", group = "Momentum")) |>
  bind_rows(tibble(sv = "sv_rmom", group = "Momentum")) |>
  bind_rows(tibble(sv = "sv_rs", group = "Momentum")) |>
  bind_rows(tibble(sv = "sv_sue", group = "Momentum")) |>
  bind_rows(tibble(sv = "sv_tes", group = "Momentum")) |>
  bind_rows(tibble(sv = "sv_52w", group = "Momentum")) |>
  bind_rows(tibble(sv = "sv_me", group = "Size")) |>
  bind_rows(tibble(sv = "sv_ato", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_bl", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_cbop", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_cto", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_gpa", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_o", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_ope", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_roa", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_roe", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_tbi", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_z", group = "Profitability")) |>
  bind_rows(tibble(sv = "sv_am", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_bm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_cfm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_dm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_ebm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_em", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_ndm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_npy", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_ocm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_rev", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_sm", group = "Valuation")) |>
  bind_rows(tibble(sv = "sv_aci", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_ag", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_dinv", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_dnoa", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_dpia", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_dwc", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_ig", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_noa", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_oa", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_pta", group = "Investment")) |>
  bind_rows(tibble(sv = "sv_cdi", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_csi", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_dbe", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_dcol", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_dfnl", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_ndf", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_nef", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_nxf", group = "Financing")) |>
  bind_rows(tibble(sv = "sv_adm", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_cfv", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_eprd", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_hr", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_kzi", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_lfe", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_ol", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_rdm", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_rer", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_tan", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_ww", group = "Intangibles")) |>
  bind_rows(tibble(sv = "sv_ami", group = "Trading frictions"))|>
  bind_rows(tibble(sv = "sv_beta", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_bfp", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_dtv", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_iskew", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_ivol", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_mdr", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_srev", group = "Trading frictions")) |>
  bind_rows(tibble(sv = "sv_tur", group = "Trading frictions")) 


# Store variables --------------------------------------------------
# Store
sv_groups |>
  dbWriteTable(conn = data_nse, 
               name = "sv_groups", 
               value = _, 
               overwrite = TRUE)
