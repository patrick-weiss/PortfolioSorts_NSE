

# Construction of quarterly sorting variables 

# TODO: Currently NOT!
stop()

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
# Quarterly Variables -------------------------------------------------------

# Load CRSP
 
compustat_quarterly <- dbReadTable(data_nse, "compustat_quarterly")  

# Create date variable 
compustat_quarterly <- compustat_quarterly %>%
 arrange(gvkey, datadate) %>%
 mutate(month = floor_date(datadate, "month")) 

# Check for duplicates: YES THERE ARE DUPLICATES !! Drop duplicates

compustat_quarterly %>%
 distinct(gvkey, datadate) %>% 
 nrow() == nrow(compustat_quarterly) # Check if distinct observations = starting number

compustat_quarterly %>%
 distinct(gvkey, month) %>% 
 nrow() == nrow(compustat_quarterly) # Check if distinct observations = starting number

compustat_quarterly <- compustat_quarterly %>% arrange(gvkey, month, fyearq, fqtr)
compustat_quarterly <- compustat_quarterly %>% 
 group_by(gvkey, datadate) %>%
 mutate(number = 1, 
        index = cumsum(number) ,
        max_index = max(index))

sum(compustat_quarterly$index > 1, na.rm=TRUE)
sum(compustat_quarterly$max_index > 1, na.rm=TRUE)                              # There are 2408 observations with the same gvkey date combination
 
compustat_quarterly <- compustat_quarterly %>% arrange(gvkey, month)
compustat_quarterly <- compustat_quarterly %>% 
 group_by(gvkey, datadate) %>%
 mutate(index2 = cumsum(number))
 
sum(compustat_quarterly$index2 > 1, na.rm=TRUE)

compustat_quarterly <- compustat_quarterly %>% arrange(max_index, gvkey, month)

compustat_quarterly <- subset(compustat_quarterly , index == 1)                 # Drop duplicates   

compustat_quarterly <- compustat_quarterly %>% arrange(gvkey, fyearq, fqtr)
compustat_quarterly <- compustat_quarterly %>% 
 group_by(gvkey, fyearq, fqtr) %>%
 mutate(index3 = cumsum(number))

sum(compustat_quarterly$index3 > 1, na.rm=TRUE)

# Compute sub-variables 
compustat_quarterly <- compustat_quarterly %>%
 mutate(noaq = (atq - cheq - replace_na(ivaoq,0)) - (atq - replace_na(dlcq,0) - replace_na(dlttq,0) - replace_na(mibq,0) - replace_na(pstkq,0) - ceqq), 
 )
 
# Lag variables

compustat_quarterly_lag <- compustat_quarterly %>%
 select(gvkey, fyearq, fqtr, atq, noaq) %>%
 mutate(fqtr = fqtr + 1 ,
        fyearq = ifelse(fqtr ==5, fyearq +1, fyearq) ,
        fqtr = ifelse(fqtr ==5, 1, fqtr) 
        ) %>%
 rename_with(.cols = atq:noaq, ~ paste0(.x, "_lag")) 

# Remerge

compustat_quarterly <- compustat_quarterly %>%
 left_join(compustat_quarterly_lag, by = c("gvkey", "fyearq", "fqtr")) %>%
 arrange(gvkey, month) %>%
 select("gvkey", "month", "year", "mon", "fyearq", "fqtr", "atq", "atq_lag", everything())
 

# Compute variables

sorting_variables_comp_q <- compustat_quarterly %>%
 mutate(identifier_direction = 1,                                              # Identifier to merge later with direction of high minus low portfolio
        sv_roaq = (ibq) / atq_lag  ,                                           # Quarterly return on assets 
        beq = coalesce(seqq, ceqq + pstkq, atq - ltq) +                        # Quarterly book equity
          coalesce(txditcq, txdbq , 0) -
          coalesce(pstkrq, pstkq, 0),
        beq = if_else(beq <= 0, as.numeric(NA), beq),
        sv_roeq = ibq / beq,                                                   # Quarterly return on equity
        sv_olaq = (revtq - cogsq - xsgaq + replace_na(xrdq,0)) / atq_lag,      # Quarterly operating profits to assets 
        sv_atoq = saleq / noaq_lag                                             # Quarterly asset turnover 
        ) %>%
 select(gvkey, month, identifier_direction, starts_with("sv_"))

# Store variables ---------------------------------------------------------

# Store

sorting_variables_comp_q %>%
 dbWriteTable(data_nse, "sorting_variables_comp_q", ., overwrite = TRUE)



