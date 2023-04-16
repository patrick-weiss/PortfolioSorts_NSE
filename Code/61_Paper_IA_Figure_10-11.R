
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

# Premiums results
data_premium_results <- dbReadTable(data_nse, "data_premium_results") |> 
  rename(alpha_Q5 = alpha_q5,
         se_Q5 = se_q5,
         t_Q5 = t_q5)

# Significance in original paper
original_significance <- dbReadTable(data_nse, "significance_orig_paper")

# Add star for insignificant premiums
data_premium_results <- data_premium_results |> 
  left_join(original_significance, by = join_by(sorting_variable == sv)) |> 
  mutate(SV = if_else(significance_orig_paper == 0, paste0(SV, "*"), SV)) |> 
  select(-significance_orig_paper)


# Figure function ---------------------------------------------------------
plot_boxplot <- function(data, 
                         var_spec = "mean", 
                         label_spec = "Premium (in \\%)",
                         limit_spec = c(-0.15, 1.5)) {

  # Plot production
  plot <- data |>
    mutate(var = get(var_spec)) |>
    ggplot(aes(x = reorder(SV, -SV_order), y  = var, fill = group)) +
    geom_boxplot(outlier.shape = NA) +
    labs(x = NULL,
         y = label_spec,
         title = NULL,
         subtitle = NULL)  + 
    coord_flip()  + 
    scale_y_continuous(limits = limit_spec) +
    guides(fill = guide_legend(title = "Group"),
           color = guide_legend(title = "Group")) +
    theme(axis.text.y = element_text(size = 8))
  
  # Add 0 line for means
  if(var_spec %in% c("mean", "alpha_CAPM", "alpha_FF5", "alpha_Q5"))  plot <- plot + 
    geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", linewidth = 0.8)
  
  # Add 1.96 line for t-stats
  if(substr(var_spec, 1, 1) == "t") plot <- plot + 
    geom_hline(yintercept = qnorm(0.975), linetype = "dashed", color = "blue", linewidth = 1)
  
  # Return
  return(plot)
}


# Boxplots ----------------------------------------------------------------
# Specifciations
figure_specs <- tibble(spec = c("mean", "t"),
                       label = c("Premium (in \\%)", "$t$-statistic"))

# Counter
the_variable_IA_counter <- 10

# Production loop
for(spec in 1:nrow(figure_specs)) {
  # Produce plot
  plot_box <- data_premium_results |> 
    filter(n_portfolios_main == 10) |>
    filter(sorting_method == "Single") |>
    filter(exchanges == "NYSE") |>
    filter(value_weighted == "VW") |>
    plot_boxplot(var_spec = figure_specs$spec[spec],
                 label_spec = figure_specs$label[spec],
                 limit_spec = NULL)
  
  # Figure options
  plot_name <- paste0("Paper_Plots/tex/IA", formatC(the_variable_IA_counter + spec - 1, width = 2, flag = "0"),
                      "_box_acrossanomalies_10nodes_",
                      figure_specs$spec[spec],".tex")
  
  # Save the plot
  tikz(plot_name,
       standAlone = T, width = 6, height = 8)
  plot(plot_box)
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
}


# Close -----------------------------------------------------------------
dbDisconnect(data_nse)
