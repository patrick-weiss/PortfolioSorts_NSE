
# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(gridExtra)
library(grid)
library(tikzDevice)
library(slider)
library(dbplyr)

# Options ----------------------------------------------------------
source("z_options_figures.R")


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)


# Load Data --------------------------------------------------------
# CRSP data
crsp_monthly <- dbReadTable(data_nse, "crsp_monthly")

# MAD timeseries
mad_ts_data <- dbReadTable(data_nse, "data_TS_timeseries_all")

# Mapping nodes + ID
mapping_nodes <- dbReadTable(data_nse, "mapping_nodes") |> 
  rowid_to_column()

# NBER recessions
nber_recession <- dbReadTable(data_nse, "nber_recession")


# Transform recession indicator ------------------------------------
# Add change indicator
nber_recession <- nber_recession |> 
  mutate(change = rec_indicator - lag(rec_indicator)) |> 
  filter(month > as.Date("1972-01-01"))

# Collect starts and ends
nber_recession_periods <- tibble("start" = nber_recession |>  
                                   filter(change == 1) |> 
                                   mutate(month = month %m-% months(1)) |> 
                                   pull(month),
                                 "end" = nber_recession |> 
                                   filter(change == -1) |> 
                                   pull(month))


# MADs over time (by decision node) --------------------------------
# Keep only the decision node: Sorting variable lag
mad_ts_data <- mad_ts_data |>
  filter(node == "sv_lag") |>
  filter(month >= as.Date("1972-01-01") & month <= as.Date("2021-12-31")) |>
  select(month,
         "Unadjusted return" = mad_R, 
         "FF5 alpha" = mad_F)


# Data from wide to long format for the graph
mad_ts_data <- mad_ts_data |>
  pivot_longer(cols = `Unadjusted return` : `FF5 alpha`, 
               names_to = "return_type",
               values_to = "mad") 


# Figure 5 ---------------------------------------------------------
plot_mad_vw <- mad_ts_data |> 
  ggplot(aes(x = month, y = mad, group = return_type, colour = return_type,
             linetype = return_type)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c(`FF5 alpha` = "dotted", `Unadjusted return` = "solid")) +
  labs(x = "Month",
       y = "MAD (in \\%)",
       title = NULL,
       subtitle = NULL) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") + 
  theme(legend.title = element_blank()) +
  geom_rect(data = nber_recession_periods, inherit.aes = FALSE,
            aes(xmin = start,
                xmax = end, 
                ymin = -Inf,
                ymax = Inf), alpha = 0.2)


# Output plot ------------------------------------------------------
tikz("Paper_Plots/tex/05_MAD_over_time_sv_lag.tex",
     standAlone = T, width = 6, height = 3)
plot_mad_vw 
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/05_MAD_over_time_sv_lag.tex"))
system(paste0("rm 05_MAD_over_time_sv_lag.log"))
system(paste0("rm 05_MAD_over_time_sv_lag.aux"))
setwd(dirname(getwd()))


# Close ------------------------------------------------------------
dbDisconnect(data_nse)