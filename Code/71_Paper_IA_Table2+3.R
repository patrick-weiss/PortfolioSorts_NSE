
# Packages ----------------------------------------------------------------
library(tidyverse)
library(moments)
library(DBI)
library(RSQLite)
library(xtable)

# Data --------------------------------------------------------------------

# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premium results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")


# Table function ----------------------------------------------------------
# Function for columnnames
wrap_columnnames <- function(text) {
  for(i in 1:length(text)) {
    if(text[[i]] %in% c("Node", "Group", "SV")) {
      next
    } else {
      text[[i]] <- paste0("\\multicolumn{1}{c}{", text[[i]], "}") 
    }
  }
  
  return(text)
}

print_tex_table <- function(data, file = NA) {
  additional_layout <- list() 
  additional_layout$pos <- as.list(data %>% 
                                     mutate(Group = as_factor(Group)) %>% 
                                     group_by(Group) %>% 
                                     summarize(freq = n()) %>% 
                                     mutate(freq = cumsum(freq)) %>% 
                                     slice(1:(n()-1)) %>%
                                     pull(freq))
  additional_layout$command <- as.vector(rep("\\midrule ", length(additional_layout$pos)), mode = "character")

  if(is.na(file)) {
    print(xtable(data),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = TRUE,
          add.to.row = additional_layout,
          sanitize.colnames.function = wrap_columnnames)
  } else {
    print(xtable(data),
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


# Table IA02 --------------------------------------------------------------
# Table production
table_across_sv <- data_premium_results %>%
  drop_na(mean) %>% 
  mutate(mean = alpha_CAPM,
         se = se_CAPM,
         t = t_CAPM) %>% 
  group_by(SV) %>%
  summarize(Group = paste0(substr(unique(group), 1, 3), "."),
            Mean = mean(mean),
            NSE = sd(mean),
            ASE = mean(se),
            Ratio = NSE/ASE,
            Skew. = skewness(mean),
            Kurt. = kurtosis(mean),
            Pos. = sum(mean > 0)/n(),
            Sig. = sum(t > qnorm(0.975))/n(),
            .groups = 'drop') %>%
  arrange(Group, SV) %>%
  select(Group, SV, Mean:Sig.)

# Add overall means at the bottom
mean_row <- table_across_sv %>%
  summarize(Group = "Mean",
            SV = "",
            across(.cols = Mean:Sig., mean))

table_across_sv <- table_across_sv %>%
  bind_rows(mean_row)

# Print table
table_across_sv %>%
  print_tex_table(file = "IA02_NSE_CAPM_acrosssvs")

# Table IA03 --------------------------------------------------------------
# Table production
table_across_sv <- data_premium_results %>%
  drop_na(mean) %>% 
  mutate(mean = alpha_FF3,
         se = se_FF3,
         t = t_FF3) %>% 
  group_by(SV) %>%
  summarize(Group = paste0(substr(unique(group), 1, 3), "."),
            Mean = mean(mean),
            NSE = sd(mean),
            ASE = mean(se),
            Ratio = NSE/ASE,
            Skew. = skewness(mean),
            Kurt. = kurtosis(mean),
            Pos. = sum(mean > 0)/n(),
            Sig. = sum(t > qnorm(0.975))/n(),
            .groups = 'drop') %>%
  arrange(Group, SV) %>%
  select(Group, SV, Mean:Sig.)

# Add overall means at the bottom
mean_row <- table_across_sv %>%
  summarize(Group = "Mean",
            SV = "",
            across(.cols = Mean:Sig., mean))

table_across_sv <- table_across_sv %>%
  bind_rows(mean_row)

# Print table
table_across_sv %>%
  print_tex_table(file = "IA03_NSE_FF3_acrosssvs")
