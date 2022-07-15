# Options for figures
## Contains the theme and layout options for figures

# Ggplot options
theme_set(theme_minimal() + theme(legend.position = "bottom"))

pal <- colorRampPalette(list("#DD8D29", "#DFB411", "#CBCC1C", "#72B68E", "#73A18F", "#CE8B1D", "#D0530E", "#B40F20")) # FantasticFox1

scale_colour_continuous <- function(...) scale_color_gradientn(colours = pal(20), ...)
scale_colour_discrete   <- function(...) discrete_scale("colour", scale_name = "pal", palette = pal, ...)

scale_fill_continuous <- function(...) scale_fill_gradientn(colours = pal(20), ...)
scale_fill_discrete   <- function(...) discrete_scale("fill", scale_name = "pal", palette = pal, ...)
