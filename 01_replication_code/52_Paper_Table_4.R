
# Packages ----------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(xtable)


# Data --------------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# TS premium
data_TS_all <- dbReadTable(data_nse, "data_TS_all")


# Table function ----------------------------------------------------------
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
  is_ad <- str_detect(file, "_ad_")
  
  if(is.na(file)) {
    print(xtable(data, digits = if_else(is_ad, 0, 2)),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          booktabs = TRUE,
          sanitize.colnames.function = wrap_columnnames)
  } else {
    print(xtable(data, digits = if_else(is_ad, 0, 2)),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = paste0("Paper_Tables/", file, ".tex"),
          booktabs = TRUE,
          sanitize.colnames.function = wrap_columnnames)
  }
}


# Table 3 Panel a ---------------------------------------------------------
# Mean absolute difference
## Overall
table_TS_mad_overall <- data_TS_all |>
  group_by(node_name) |>
  summarise(All = mean(mad, na.rm = T),
            .groups = 'drop') |>
  arrange(desc(All))

## Groups
table_TS_mad_groups <- data_TS_all |>
  mutate(group = paste0(substr(group, 1, 3), ".")) |>
  group_by(node_name, group) |>
  summarise(mean = mean(mad, na.rm = T),
            .groups = 'drop') |>
  pivot_wider(names_from = group, values_from = mean)

## Combine
table_TS_mad_overall |>
  left_join(table_TS_mad_groups, by = "node_name") |>
  rename(Node = node_name) |> 
  print_tex_table(file = "04a_mad_acrossnodes")


# Table 3 Panel b ---------------------------------------------------------
# Correlation
## Overall
table_TS_corr_overall <- data_TS_all |>
  group_by(node_name) |>
  summarise(All = mean(cor, na.rm = T),
            .groups = 'drop') |>
  arrange(All)

## Groups
table_TS_corr_groups <- data_TS_all |>
  mutate(group = paste0(substr(group, 1, 3), ".")) |>
  group_by(node_name, group) |>
  summarise(mean = mean(cor, na.rm = T),
            .groups = 'drop') |>
  pivot_wider(names_from = group, values_from = mean)

## Combine
table_TS_corr_overall |>
  left_join(table_TS_corr_groups, by = "node_name") |>
  rename(Node = node_name) |> 
  print_tex_table(file = "04b_corr_acrossnodes")


# Table 3 Panel c ---------------------------------------------------------
# AD tests
## Overall
table_TS_ad_overall <- data_TS_all |>
  group_by(node_name) |>
  summarise(All = mean(ad, na.rm = T),
            .groups = 'drop') |>
  arrange(desc(All))

## Groups
table_TS_ad_groups <- data_TS_all |>
  mutate(group = paste0(substr(group, 1, 3), ".")) |>
  group_by(node_name, group) |>
  summarise(mean = mean(ad, na.rm = T),
            .groups = 'drop') |>
  pivot_wider(names_from = group, values_from = mean)

## Combine
table_TS_ad_overall |>
  left_join(table_TS_ad_groups, by = "node_name") |>
  rename(Node = node_name) |>
  print_tex_table(file = "04c_ad_acrossnodes")


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)