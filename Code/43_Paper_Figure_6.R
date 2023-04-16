
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


# Load Data ---------------------------------------------------------------
mad_ts_data <- dbReadTable(data_nse, "data_TS_timeseries_all")

# Keep only the decision node: weighting scheme
mad_ts_data <- mad_ts_data |>
  filter(node_name == "Weighting scheme") |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  select(-node, -node_name) |>
  rename("Unadjusted return" = mad_R, 
         "CAPM alpha" = mad_C, 
         "FF5 alpha" = mad_F, 
         "Q5 alpha" = mad_Q)


# Data from wide to long format for the graph ---------------------------------- 
mad_ts_data <- mad_ts_data |>
  pivot_longer(cols = `Unadjusted return` : `Q5 alpha`, 
               names_to = "return_type",
               values_to = "mad") 


# Figure 6 ---------------------------------------------------------------------
plot_mad_vw <- mad_ts_data |> 
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
  


# Output plot ------------------------------------------------------------------
tikz("Paper_Plots/tex/06_MAD_over_time_weighting_scheme.tex",
     standAlone = T, width = 6.5, height = 3)
plot_mad_vw 
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/06_MAD_over_time_weighting_scheme.tex"))
system(paste0("rm 06_MAD_over_time_weighting_scheme.log"))
system(paste0("rm 06_MAD_over_time_weighting_scheme.aux"))
setwd(dirname(getwd()))



