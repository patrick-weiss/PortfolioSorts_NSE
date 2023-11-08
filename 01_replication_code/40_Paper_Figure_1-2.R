
# Packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(gridExtra)
library(grid)
library(tikzDevice)


# Options ----------------------------------------------------------
source("z_options_figures.R")


# Data -------------------------------------------------------------
# Access Database 
data_nse <- dbConnect(SQLite(), 
                      "Data/data_nse.sqlite", 
                      extended_types = TRUE)

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results")


# Figure 1 ---------------------------------------------------------
# Conditional
plot_small <- data_premium_results |>
  filter(sorting_variable == "sv_ag") |>
  filter(value_weighted == "VW",
         exchanges == "NYSE",
         sorting_method == "Single", 
         n_portfolios_main == "10") |>
  ggplot() +
  geom_density(aes(x  = mean), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = alpha_CAPM), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.15, 0.70), breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "Premium (in \\%, p.m.)",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel B: Distribution for single sorts w/o HXZ"))

# Overall
plot_all <- data_premium_results |>
  filter(sorting_variable == "sv_ag") |>
  ggplot() +
  geom_density(aes(x  = mean), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = alpha_CAPM), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.1, 1.4), breaks = scales::pretty_breaks(n = 6)) + 
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


# Figure 2 ---------------------------------------------------------
# Subset
plot_small <- data_premium_results |>
  filter(sorting_variable == "sv_ag") |>
  filter(value_weighted == "VW",
         exchanges == "NYSE",
         sorting_method == "Single", 
         n_portfolios_main == "10") |>
  ggplot() +
  geom_density(aes(x  = t), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = t_CAPM), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.85, 4.2), breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "$t$-statistic",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel B: Distribution for single sorts w/o HXZ"))  +
  geom_vline(xintercept = qnorm(0.975), linetype = "dashed", linewidth = 1) 

# Overall
plot_all <- data_premium_results |>
  filter(sorting_variable == "sv_ag") |>
  ggplot() +
  geom_density(aes(x  = t), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = t_CAPM), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.75, 10), breaks = scales::pretty_breaks(n = 8)) + 
  labs(x = "$t$-statistic",
       y = NULL,
       title = NULL,
       subtitle = paste("Panel A: Overall distribution")) +
  geom_vline(xintercept = qnorm(0.975), linetype = "dashed", linewidth = 1) 

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


# Figure JPEG ------------------------------------------------------
# Overall
plot_r <- data_premium_results |>
  filter(sorting_variable == "sv_ag") |>
  ggplot() +
  geom_density(aes(x  = mean), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = alpha_CAPM), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.1, 1.4), breaks = scales::pretty_breaks(n = 6)) + 
  labs(x = "Premium (in %, p.m.)",
       y = NULL,
       title = NULL,
       subtitle = NULL) 


# Overall
plot_t <- data_premium_results |>
  filter(sorting_variable == "sv_ag") |>
  ggplot() +
  geom_density(aes(x  = t), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "red", colour = "red") +
  geom_density(aes(x  = t_CAPM), 
               alpha = 0.25, linewidth = 1.2, 
               fill = "blue", colour = "blue", linetype = 'dashed') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_continuous(limits = c(0.75, 10), breaks = scales::pretty_breaks(n = 8)) + 
  labs(x = "t-statistic",
       y = NULL,
       title = NULL,
       subtitle = NULL) +  
  geom_vline(xintercept = qnorm(0.975), linetype = "dashed", linewidth = 1) 

# Combine
plot_combined <- grid.arrange(plot_r, plot_t, ncol=2,
                              top = textGrob("Non-Standard Errors in Portfolio Sorts",
                                             gp = gpar(fontface = 2, fontlinewidth = 14)),
                              bottom = textGrob("Results for premiums (solid, red) and CAPM-alphas (dashed, blue) 
                                                for portfolio sorts on asset growth. Left: Premiums (in %, p.m.). Right: t-statistics.",
                                                gp = gpar(fontface = 3, fontlinewidth = 11),
                                                hjust = 1.01,
                                                x = 1))

# Save
ggsave(
  plot = plot_combined, width = 6.5, height = 4,
  filename = "Paper_Plots/JPG/NSE_plot_figure_1and2.jpg", bg = "white"
)


# Close ------------------------------------------------------------
dbDisconnect(data_nse)