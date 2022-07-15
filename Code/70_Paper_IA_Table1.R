
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

# Sorting variables compustat annual
sorting_variables_COMP <- dbReadTable(data_nse, "sorting_variables_comp_y")

# Sorting variables CRSP
sorting_variables_CRSP <- dbReadTable(data_nse, "sorting_variables_CRSP")

# SV Groups 
sv_groups <- dbReadTable(data_nse, "sv_groups")


# Compute summary ---------------------------------------------------------
# Reshape data
## Compustat
sv_COMP <- sorting_variables_COMP %>%
  select(starts_with("sv_")) %>%
  pivot_longer(cols = everything(),
               names_to = "sorting_variable",
               values_to = "data") %>%
  drop_na()

## CRSP
sv_CRSP <- sorting_variables_CRSP %>%
  select(starts_with("sv_")) %>%
  mutate(sv_dtv = sv_dtv / 10^6) %>% 
  pivot_longer(cols = everything(),
               names_to = "sorting_variable",
               values_to = "data") %>%
  drop_na()

## Combine
svs <- sv_COMP %>%
  bind_rows(sv_CRSP) %>%
  left_join(sv_groups, by = c("sorting_variable" = "sv"))


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


# Table IA01 --------------------------------------------------------------
# Table production
# Compute summary
svs_table <- svs %>% 
  arrange(group, sorting_variable) %>%
  mutate(SV = str_to_upper(substr(sorting_variable, 4, nchar(sorting_variable)))) %>%
  group_by(SV) %>%
  mutate(data = case_when(data > quantile(data, 0.99) ~ quantile(data, 0.99),
                          data < quantile(data, 0.01) ~ quantile(data, 0.01),
                          TRUE ~ data)) %>%
  summarize(Group = paste0(substr(unique(group), 1, 3), "."), 
            Mean = mean(data),
            SD = sd(data),
            Minimum = min(data),
            Median = median(data),
            Maximum = max(data),
            Obs. = n()/1000) %>%
  arrange(Group, SV) %>%
  select(Group, SV, Mean:Obs.)

# Print table
svs_table %>%
  print_tex_table(file = "IA01_SV_summary")

