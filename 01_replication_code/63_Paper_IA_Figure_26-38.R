
# Packages ----------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(gridExtra)
library(grid)
library(tikzDevice)


# Options -----------------------------------------------------------------
source("z_options_figures.R")


# Data --------------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

mad_ts_data <- dbReadTable(data_nse, "data_TS_timeseries_all")

mapping_nodes <- dbReadTable(data_nse, "mapping_nodes")


# Prepare data ----------------------------------------------------------
# TS truncation
mad_ts_data <- mad_ts_data |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  rename("Unadjusted return" = mad_R, 
         "CAPM alpha" = mad_C, 
         "FF5 alpha" = mad_F, 
         "Q5 alpha" = mad_Q)

# Mapping nodes w/o value_weighted
mapping_nodes <- mapping_nodes |> 
  filter(node != "value_weighted")


# Functions -------------------------------------------------------------
# Printing function
plot_mad_ts <- function(data) {
  data |> 
    ggplot(aes(x = month, y = mad, group = return_type, colour = return_type,
               linetype = return_type)) +
    geom_line(linewidth = 0.8) +
    labs(x = "Month",
         y = "MAD (in \\%)",
         title = NULL,
         subtitle = NULL) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") + 
    theme(legend.title = element_blank()) 
}

# Data preparation
select_node <- function(data, the_node) {
  data_node <- data |> 
    filter(node == the_node) |> 
    select(-node, -node_name) |>
    pivot_longer(cols = !month, 
                 names_to = "return_type",
                 values_to = "mad") 
}



# Figure IA 26-38 -------------------------------------------------------
# Counter for table numbering
the_variable_IA_counter <- 26

# Loop through nodes
for(nodes in mapping_nodes$node) {
  # Naming
  plot_name <- paste0("Paper_Plots/tex/IA", 
                      formatC(the_variable_IA_counter, width = 2, flag = "0"),
                      "_MAD_over_time_",
                      nodes,".tex")
  
  # Plot generation
  the_plot <- mad_ts_data |> 
    select_node(the_node = nodes) |> 
    plot_mad_ts()
  
  # Plotting
  tikz(plot_name,
       standAlone = T, width = 6.5, height = 3)
  plot(the_plot) 
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


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)
