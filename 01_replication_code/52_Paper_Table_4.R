
# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(xtable)


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# TS premium
data_TS_all <- dbReadTable(data_nse, "data_TS_all")

# Node Text 
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")

data_TS_all <- data_TS_all |>
  select(-node_name) |>
  left_join(mapping_nodes, by = c("node")) |>
  select(-node_order)


# Table function ---------------------------------------------------
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
          booktabs = TRUE)
  } else {
    print(xtable(data, digits = if_else(is_ad, 0, 2)),
          include.rownames = FALSE,
          only.contents	= TRUE,
          comment = FALSE,
          timestamp = FALSE,
          file = paste0("Paper_Tables/", file, ".tex"),
          booktabs = TRUE)
  }
}


# Table 4 ----------------------------------------------------------
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
  rename(Fork = node_name) |> 
  print_tex_table(file = "04_mad_acrossnodes")


# Close ------------------------------------------------------------
dbDisconnect(data_nse)