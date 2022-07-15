
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


# Data --------------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")


# Figure function ---------------------------------------------------------
plot_premiums_boxplot <- function(data) {
  # Plot production
  data %>%
    ggplot(aes(x = reorder(SV, -SV_order), y  = alpha_FF3, fill = group)) +
    geom_boxplot(outlier.shape = NA) +
    labs(x = NULL,
         y = "FF3-adjusted premium (in \\%)",
         title = NULL,
         subtitle = NULL)  + 
    coord_flip()  + 
    scale_y_continuous(limits = c(-0.4, 1.5)) +
    guides(fill = guide_legend(title = "Group"),
           color = guide_legend(title = "Group")) +
    geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", size = 0.8)
}


# Wrap plot ---------------------------------------------------------------
# Produce plot
plot_box <- data_premium_results %>%
  plot_premiums_boxplot()

# Output plot
tikz("Paper_Plots/tex/06_NSE_F_box_acrossanomalies.tex",
     standAlone = T, width = 6, height = 7.5)
plot_box
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/06_NSE_F_box_acrossanomalies.tex"))
system(paste0("rm 06_NSE_F_box_acrossanomalies.log"))
system(paste0("rm 06_NSE_F_box_acrossanomalies.aux"))
setwd(dirname(getwd()))