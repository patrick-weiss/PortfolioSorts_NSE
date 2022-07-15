
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


# Figure 1 ----------------------------------------------------------------
# Conditional
plot_small <- data_premium_results %>%
  filter(sorting_variable == "sv_ag") %>%
  filter(value_weighted == "VW",
         exchanges == "NYSE",
         sorting_method == "Single", 
         n_portfolios_main == "10") %>%
  ggplot() +
  geom_density(aes(x  = mean), 
               alpha = 0.25, size = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = alpha_FF3), 
               alpha = 0.25, size = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.05, 0.55), breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "Premium (in \\%, p.m.)",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel B: Conditional distribution"))

# Overall
plot_all <- data_premium_results %>%
  filter(sorting_variable == "sv_ag") %>%
  ggplot() +
  geom_density(aes(x  = mean), 
               alpha = 0.25, size = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = alpha_FF3), 
               alpha = 0.25, size = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.05, 1.3), breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "Premium (in \\%, p.m.)",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel A: Overall distribution"))

# Output plot
tikz("Paper_Plots/tex/01_NSE_excessreturns.tex",
                    standAlone = T, width = 6.5, height = 3)
grid.arrange(plot_all, plot_small, ncol=2)
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/01_NSE_excessreturns.tex"))
system(paste0("rm 01_NSE_excessreturns.log"))
system(paste0("rm 01_NSE_excessreturns.aux"))
setwd(dirname(getwd()))


# Figure 2 ----------------------------------------------------------------
# Subset
plot_small <- data_premium_results %>%
  filter(sorting_variable == "sv_ag") %>%
  filter(value_weighted == "VW",
         exchanges == "NYSE",
         sorting_method == "Single", 
         n_portfolios_main == "10") %>%
  ggplot() +
  geom_density(aes(x  = t), 
               alpha = 0.25, size = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = t_FF3), 
               alpha = 0.25, size = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.5, 3.8), breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "$t$-statistic",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel B: Conditional distribution"))  +
  geom_vline(xintercept = qnorm(0.975), linetype = "dashed", size = 1) 

# Overall
plot_all <- data_premium_results %>%
  filter(sorting_variable == "sv_ag") %>%
  ggplot() +
  geom_density(aes(x  = t), 
               alpha = 0.25, size = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = t_FF3), 
               alpha = 0.25, size = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.5, 10), breaks = scales::pretty_breaks(n = 8)) + 
  labs(x = "$t$-statistic",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel A: Overall distribution")) +
  geom_vline(xintercept = qnorm(0.975), linetype = "dashed", size = 1) 

# Output plot
tikz("Paper_Plots/tex/02_Tstats_excessreturns.tex",
     standAlone = T, width = 6.5, height = 3)
grid.arrange(plot_all, plot_small, ncol=2)
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/02_Tstats_excessreturns.tex"))
system(paste0("rm 02_Tstats_excessreturns.log"))
system(paste0("rm 02_Tstats_excessreturns.aux"))
setwd(dirname(getwd()))