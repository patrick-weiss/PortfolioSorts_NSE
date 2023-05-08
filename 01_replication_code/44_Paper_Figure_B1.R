
# Packages ----------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
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

# Significance in original paper
original_significance <- dbReadTable(data_nse, "significance_orig_paper")


# Figure function ---------------------------------------------------------
plot_premiums_boxplot <- function(data) {
  # Plot production
  data |>
    ggplot(aes(x = reorder(SV, -SV_order), y  = t, fill = group)) +
    geom_boxplot(outlier.shape = NA) +
    labs(x = NULL,
         y = "$t$-statistic",
         title = NULL,
         subtitle = NULL)  + 
    coord_flip()  + 
    scale_y_continuous(limits = c(-7, 12)) +
    guides(fill = guide_legend(title = "Group"),
           color = guide_legend(title = "Group")) +
    theme(axis.text.y = element_text(size = 8)) + 
    geom_hline(yintercept = qnorm(0.975), linetype = "dashed", color = "blue", linewidth = 1)
}


# Wrap plot ---------------------------------------------------------------
# Add star for insignificant premiums
data_premium_results <- data_premium_results |> 
  left_join(original_significance, by = join_by(sorting_variable == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(SV, "*"), SV))

# Produce plot
plot_box <- data_premium_results |>
  plot_premiums_boxplot()

# Output plot
tikz("Paper_Plots/tex/B01_box_acrossanomalies_t.tex",
     standAlone = T, width = 6, height = 8)
plot_box
dev.off()

setwd("./Paper_Plots")
system(paste0("lualatex tex/B01_box_acrossanomalies_t.tex"))
system(paste0("rm B01_box_acrossanomalies_t.log"))
system(paste0("rm B01_box_acrossanomalies_t.aux"))
setwd(dirname(getwd()))


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)