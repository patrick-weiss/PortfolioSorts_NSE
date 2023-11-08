# Final result prep
## Collect results, cleaning, merge variables, and scale variables


# Packages ---------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)
library(kSamples)


# Data files -------------------------------------------------------
# Access regular database
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Data premium results 
data_premium_results <- dbReadTable(data_nse, "data_premium_results")


# Functions --------------------------------------------------------
# Compute AD test and output test statistic
ad_test <- function(data) {
  # Feedback
  cat(".")
  
  # Premiums
  ad_r <- data |> pull(mean)
  ad_c <- data |> pull(alpha_CAPM)
  ad_f <- data |> pull(alpha_FF5)
  ad_q <- data |> pull(alpha_q5)
 
  # Escape low levels
  if(min(length(ad_r), length(ad_c), length(ad_f), length(ad_q)) < 100) {
    return(tibble(ad_rc = NA_real_, ad_rc_t = NA_real_,
                  ad_cf = NA_real_, ad_cf_t = NA_real_,
                  ad_cq = NA_real_, ad_cq_t = NA_real_,
                  ad_fq = NA_real_, ad_fq_t = NA_real_))
  }
  
  # Demean
  ad_r <- ad_r - mean(ad_r)
  ad_c <- ad_c - mean(ad_c)
  ad_f <- ad_f - mean(ad_f)
  ad_q <- ad_q - mean(ad_q)
  
  # Compute AD test
  ad_rc_res <- ad.test(ad_r, ad_c, method = "asymptotic", dist = FALSE)
  ad_cf_res <- ad.test(ad_c, ad_f, method = "asymptotic", dist = FALSE)
  ad_cq_res <- ad.test(ad_c, ad_q, method = "asymptotic", dist = FALSE)
  ad_fq_res <- ad.test(ad_f, ad_q, method = "asymptotic", dist = FALSE)
  
  # Return tibble
  tibble(ad_rc = ad_rc_res$ad[1, 1], ad_rc_t = ad_rc_res$ad[1, 2],
         ad_cf = ad_cf_res$ad[1, 1], ad_cf_t = ad_cf_res$ad[1, 2],
         ad_cq = ad_cq_res$ad[1, 1], ad_cq_t = ad_cq_res$ad[1, 2],
         ad_fq = ad_fq_res$ad[1, 1], ad_fq_t = ad_fq_res$ad[1, 2])
}


# Computations -----------------------------------------------------
# AD tests across AP models
## Initialize table
data_ad_models <-  data_premium_results |> 
    group_by(sorting_variable) |> 
    summarise(ad_results = ad_test(data = pick(everything())),
              .groups = 'drop') |> 
    unnest(ad_results)
  
# Change sorting_variable
data_ad_models <- data_ad_models |> 
  mutate(sorting_variable = str_to_upper(substr(sorting_variable, 4, nchar(sorting_variable))),
         sorting_variable = str_replace(sorting_variable, "_", "")) |> 
  rename(SV = sorting_variable)


# Save results -----------------------------------------------------
# AD for models
data_ad_models |> 
  dbWriteTable(conn = data_nse,
               name = "data_ad_models", 
               value = _, 
               overwrite = TRUE)

# Disconnect
dbDisconnect(data_nse)