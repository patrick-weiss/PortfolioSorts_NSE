# Sorting variable groups


# Packages and Setup ------------------------------------------------------

# Packages 
library(tidyverse)
library(RSQLite)
library(DBI)

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)
 

# Sorting Variable Groups -------------------------------------------------

sv_groups <- tibble(sv = "sv_mom", group = "Momentum") %>%
  bind_rows(tibble(sv = "sv_mom_6", group = "Momentum")) %>%
  bind_rows(tibble(sv = "sv_e_6", group = "Momentum")) %>%
  bind_rows(tibble(sv = "sv_e_11", group = "Momentum")) %>%
  bind_rows(tibble(sv = "sv_size", group = "Size")) %>%
  bind_rows(tibble(sv = "sv_ato", group = "Profitability")) %>%
  bind_rows(tibble(sv = "sv_cbop", group = "Profitability")) %>%
  bind_rows(tibble(sv = "sv_cto", group = "Profitability")) %>%
  bind_rows(tibble(sv = "sv_gpa", group = "Profitability")) %>%
  bind_rows(tibble(sv = "sv_o", group = "Profitability")) %>%
  bind_rows(tibble(sv = "sv_bm", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_cfp", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_dm", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_ep", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_npy", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_ocp", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_sp", group = "Valuation")) %>%
  bind_rows(tibble(sv = "sv_ag", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_csi", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_dnca", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_dnco", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_dpia", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_dwc", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_ig", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_iva", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_ivc", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_ivg", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_noa", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_oa", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_poa", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_pta", group = "Investment and financing")) %>%
  bind_rows(tibble(sv = "sv_adm", group = "Intangibles")) %>%
  bind_rows(tibble(sv = "sv_eprd", group = "Intangibles")) %>%
  bind_rows(tibble(sv = "sv_ol", group = "Intangibles")) %>%
  bind_rows(tibble(sv = "sv_rdm", group = "Intangibles")) %>%
  bind_rows(tibble(sv = "sv_rer", group = "Intangibles")) %>%
  bind_rows(tibble(sv = "sv_dtv", group = "Trading frictions")) %>%
  bind_rows(tibble(sv = "sv_ivolc", group = "Trading frictions")) %>%
  bind_rows(tibble(sv = "sv_ivolff", group = "Trading frictions")) %>%
  bind_rows(tibble(sv = "sv_iscc", group = "Trading frictions")) %>%
  bind_rows(tibble(sv = "sv_iscff", group = "Trading frictions"))

# Store variables ---------------------------------------------------------

# Store
sv_groups %>%
  dbWriteTable(data_nse, "sv_groups", ., overwrite = TRUE)
