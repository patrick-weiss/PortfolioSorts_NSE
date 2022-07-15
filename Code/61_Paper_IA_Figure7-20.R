
# Packages ----------------------------------------------------------------
library(tidyverse)
library(moments)
library(DBI)
library(RSQLite)
library(scales)
library(gridExtra)
library(grid)
library(tikzDevice)


# Options -----------------------------------------------------------------

source("z_options_figures.R")

option_trim <- 0.005


# Data --------------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Mapping nodes
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")

# Demean results
data_premium_results_demeaned <- dbReadTable(data_nse, "data_premium_results_demeaned")



# Figure function ---------------------------------------------------------
plot_premiums_figure_grouped <- function(data, variable) {
  # Node as text
  Node <- mapping_nodes$node_name[mapping_nodes$node == variable]
  
  # Filter for nodes where certain sv are excluded
  ## Formation time and sv lag
  if(variable %in% c("formation_time", "sv_lag")) {
    data <- data %>%
      filter(sorting_variable != "sv_csi",
             group != "Momentum",
             group != "Size",
             group != "Trading Frictions")
  }
  
  ## Stock age
  if(variable %in% c("drop_stock_age_at")) {
    data <- data %>%
      filter(sorting_variable != "sv_csi")
  }
  
  ## Secondary portfolio
  if(variable %in% c("n_portfolios_secondary")) {
    data <- data %>%
      filter(!is.na(n_portfolios_secondary))
  }
  
  # Plot production
  data %>%
    filter(mean >= quantile(mean, option_trim),
           mean <= quantile(mean, 1 - option_trim)) %>%
    ggplot(aes(x  = mean, fill = as.factor(get(variable)), color = as.factor(get(variable)))) +
    geom_density(alpha = 0.2) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + 
    labs(x = "Average premium (demeaned, in \\%)",
         y = NULL,
         title = NULL,
         subtitle = NULL) + 
    guides(fill = guide_legend(title = Node),
           color = guide_legend(title = Node)) +
    facet_wrap(~ group, 
               ncol = 2)
}

# Figures -----------------------------------------------------------------
# Loop for figure production
the_variable_IA_counter <- 7

## Actual loop
for(the_variable in mapping_nodes$node) {
  # Produce the plot
  plot <- data_premium_results_demeaned %>%
    plot_premiums_figure_grouped(variable = the_variable)

  # Figure options
  plot_rows <- ceiling(length(unique(ggplot_build(plot)$data[[1]]$PANEL))/2)
  plot_name <- paste0("Paper_Plots/tex/IA", 
                      formatC(the_variable_IA_counter, width = 2, flag = "0"),
                      "_NodeComparison_",
                      the_variable,".tex")
  
  # Save the plot
  tikz(plot_name,
       standAlone = T, width = 6.5, height = 7.5)
  plot(plot)
  dev.off()
  
  # Run compilation
  ## Set WD
  setwd(paste0(getwd(), "/Paper_Plots"))
  
  ## Respecify names
  tex_name <- substr(plot_name, 13, nchar(plot_name))
  file_name <- substr(tex_name, 5, nchar(tex_name) - 4)
  
  ## Compile and housekeeping
  system(paste0("lualatex ", tex_name))
  system(paste0("rm ", file_name, ".log"))
  system(paste0("rm ", file_name, ".aux"))
  
  ## Reset WD
  setwd(dirname(getwd()))
  
  # Increase counter
  the_variable_IA_counter <- the_variable_IA_counter + 1 
}
