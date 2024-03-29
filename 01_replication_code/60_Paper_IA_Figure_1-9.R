
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


# Figure function --------------------------------------------------
plot_boxplot <- function(data, 
                         var_spec = "mean", 
                         label_spec = "Premium (in \\%)") {
  
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
    guides(fill = guide_legend(title = "Group"),
           color = guide_legend(title = "Group")) +
    theme(axis.text.y = element_text(size = 8))
  
  # Add 0 line for means
  if(var_spec %in% c("mean", "alpha_CAPM", "alpha_FF5", "alpha_Q5"))  plot <- plot + 
      geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", linewidth = 0.8)
  
  # Limit y-axis for premia
  if(var_spec %in% c("mean"))  plot <- plot + 
      scale_y_continuous(limits = c(-0.6, 1.6)) 
  if(var_spec %in% c("alpha_CAPM", "alpha_FF5", "alpha_Q5"))  plot <- plot + 
      scale_y_continuous(limits = c(-1.15, 2.15)) 
  
  # Limit y-axis for t-stats
  if(var_spec %in% c("t_CAPM"))  plot <- plot + 
      scale_y_continuous(limits = c(-7.5, 13)) 
  if(var_spec %in% c("t_FF5"))  plot <- plot + 
      scale_y_continuous(limits = c(-6, 11)) 
  if(var_spec %in% c("t_Q5"))  plot <- plot + 
      scale_y_continuous(limits = c(-5.5, 10.5)) 
  
  # Limit y-axis for standard errors
  if(var_spec %in% c("se_CAPM"))  plot <- plot + 
      scale_y_continuous(limits = c(0, 0.38)) 
  if(var_spec %in% c("se_FF5"))  plot <- plot + 
      scale_y_continuous(limits = c(0, 0.52)) 
  if(var_spec %in% c("se_Q5"))  plot <- plot + 
      scale_y_continuous(limits = c(0, 0.59)) 
  
  # Add 1.96 line for t-stats
  if(substr(var_spec, 1, 1) == "t") plot <- plot + 
      geom_hline(yintercept = qnorm(0.975), linetype = "dashed", color = "blue", linewidth = 1)
  
  # Return
  return(plot)
}


# Boxplots ---------------------------------------------------------
# Specifciations
figure_specs <- tibble(spec = c("alpha_CAPM", "alpha_FF5", "alpha_Q5",
                                "t_CAPM", "t_FF5", "t_Q5",
                                "se_CAPM", "se_FF5", "se_Q5"),
                       label = c("Premium (in \\%)", "Premium (in \\%)", "Premium (in \\%)",
                                 "$t$-statistic", "$t$-statistic", "$t$-statistic",
                                 "Standard errors", "Standard errors", "Standard errors"))

# Counter
the_variable_IA_counter <- 1

# Production loop
for(spec in 1:nrow(figure_specs)) {
  # Produce plot
  plot_box <- data_premium_results |>
    plot_boxplot(var_spec = figure_specs$spec[spec],
                 label_spec = figure_specs$label[spec])
  
  # Figure options
  plot_name <- paste0("Paper_Plots/tex/IA", formatC(the_variable_IA_counter + spec - 1, width = 2, flag = "0"),
                      "_box_acrossanomalies_",
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


# Close ------------------------------------------------------------
dbDisconnect(data_nse)
