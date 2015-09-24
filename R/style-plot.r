#' Style plot
#' 
#' Functions to style plots
#' 
#' @rdname style-plot
#' @export
style_plot <- function(plot, colors = pm_colors(), base_size = 9) {
  plot + custom_theme(base_size = base_size) + scale_fill_manual(values=colors) + scale_colour_manual(values=colors)
}

#' @rdname style-plot
#' @export
custom_theme <- function (base_size = 9, base_family = "Neo Sans Pro") 
{
  theme(
    line = element_line(colour = "black", size = 0.25, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.25, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    title = element_text(),
    
    axis.title = element_text(size = rel(0.8)),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.text = element_text(size = rel(0.8)),
    axis.text.x = element_text(vjust = 1, hjust = 0.45),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),
    axis.line = element_line(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    
    legend.background = element_rect(colour = NA),
    legend.margin = unit(0, "cm"),
    legend.key = element_rect(fill = NA, colour = NA), 
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(0.6, "lines"),
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL, 
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = "top",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    
    panel.background = element_rect(fill = NA, colour = NA),
    panel.border = element_blank(),
    panel.margin = unit(0.5, "lines"),
    panel.grid = element_line(colour = rgb(222, 222, 222, max = 255)),
    panel.grid.major = element_line(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2)),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
    
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = rel(0.8)),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),
    
    complete = TRUE
  )
}

#' @rdname style-plot

