
# Packages ---------------------------------------------------------
library(tidyverse)
library(moments)
library(RSQLite)
library(xtable)


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")

# AD test between models
data_ad_models <- dbReadTable(data_nse, "data_ad_models")


# Table function ---------------------------------------------------
# IQR function
iqr <- function(premiums, quantiles = 0.25) {
  quantile(premiums, 1 - quantiles) - quantile(premiums, quantiles) |> 
    as.numeric()
}

# Function for columnnames
wrap_columnnames <- function(text) {
  for(i in 1:length(text)) {
    if(text[[i]] %in% c("Node", "Group", "SV")) {
      next
    } else {
      text[[i]] <- paste0("\\multicolumn{1}{l}{", text[[i]], "}") 
    }
  }
  
  return(text)
}

print_tex_table <- function(data, file = NA) {
  # Layout horizontal lines
  additional_layout <- list() 
  additional_layout$pos <- as.list(data |> nrow() - 1)
  additional_layout$command <- as.vector(rep("\\midrule ", length(additional_layout$pos)), mode = "character")
  if(nrow(data) == 1) additional_layout$command <- ""

  # Actual printing
  if(is.na(file)) {
    print(xtable(data, digits = c(0, 0, rep(2, 4), rep(0, 4))),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  } else {
    print(xtable(data, digits = c(0, 0, rep(2, 4), rep(0, 4))),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = paste0("Paper_Tables/", file, ".tex"),
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  }
}


# Table 3 ----------------------------------------------------------
# Table production
table_across_models <- data_premium_results |>
  drop_na(mean, alpha_CAPM, alpha_FF5, alpha_q5) |> 
  group_by(SV) |>
  summarize(Group = unique(group), 
            Raw = iqr(mean),
            CAPM = iqr(alpha_CAPM),
            FF5 = iqr(alpha_FF5),
            Q5 = iqr(alpha_q5),
            .groups = 'drop') |>
  arrange(Group, SV) |>
  left_join(data_ad_models |> select(!ends_with("_t")), by = "SV") |> 
  group_by(Group) |> 
  summarize(across(.cols = Raw:ad_fq, mean),
            .groups = 'drop')

# Average
table_across_models_mean <- data_premium_results |>
  drop_na(mean, alpha_CAPM, alpha_FF5, alpha_q5) |>
  group_by(SV) |>
  summarize(Raw = iqr(mean),
            CAPM = iqr(alpha_CAPM),
            FF5 = iqr(alpha_FF5),
            Q5 = iqr(alpha_q5),
            .groups = 'drop') |>
  left_join(data_ad_models |> 
              select(-ends_with("_t")) |> 
              group_by(SV) |> 
              summarise(across(everything(), mean),
                        .groups = 'drop'),
            by = "SV") |> 
  summarise(across(.cols = Raw:ad_fq, mean))

# Combine
table_across_models_all <- table_across_models |> 
  bind_rows(table_across_models_mean) |> 
  mutate(Group = replace_na(Group, "Mean")) |> 
  rename("R-C" = ad_rc,
         "C-F" = ad_cf,
         "C-Q" = ad_cq,
         "F-Q" = ad_fq)

# Print table
table_across_models_all |> print_tex_table(file = "IA05_NSE_across_models")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)