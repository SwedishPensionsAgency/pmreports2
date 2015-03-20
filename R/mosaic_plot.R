#' Funktion för mosaic plot
#' 
#' @export
mosaic_plot <- function(x, colors = pm_colors, legend.position = "right") {
  
  # Beräkna om till procent
  vars <- colnames(x)[!colnames(x) %in% c("variable", "(all)")]
  for (i in vars) {
    x[i] <- x[i] / x[["(all)"]]
    
  }
  x[["(all)"]] <- x[["(all)"]] / sum(x[["(all)"]])
  
  # Lägg till ytterligare variabler
  x$xmax <- cumsum(x[["(all)"]])
  x$xmin <- x$xmax - x[["(all)"]]
  x[["(all)"]] <- NULL
  
  x <- melt(x, id = c("variable", "xmin", "xmax"))
  x <- ddply(x, .(variable), transform, ymax = cumsum(value))
  x <- ddply(x, .(variable), transform, ymin = ymax - value)
  
  # Positioning of text
  x$xtext <- with(x, xmin + (xmax - xmin)/2)
  x$ytext <- with(x, ymin + (ymax - ymin)/2)
  
  # Lägg till label i första kolumnen
  #x$add_label <- ifelse(x$variable %in% x$variable[1], as.character(x$variable.1), "")
  x$add_label <- ifelse(x$variable %in% x$variable[1], "", "")
  
  p <- ggplot(x, aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable.1))
  
  p <- p + geom_rect() + 
    geom_rect(colour = I("white"), size = 0.25, show_guide=FALSE) + 
    guides(fill = guide_legend(reverse = TRUE))
  #   p <- p + geom_text(aes(
  #     x = xtext, 
  #     y = ytext,
  #     label = add_label, family = "Neo Sans Pro", fontface = 1))
  
  p <- p + geom_text(aes(x = xtext, y = 1.05, label = variable, size = 7.2, family = "Neo Sans Pro", fontface = 1), size = 2, fontface = 1)
  
  p <- style_plot(p, colors) + 
    xlab("") + 
    ylab("") + 
    scale_x_continuous(labels = percent_format(), breaks = seq(0,1,0.25)) + 
    scale_y_continuous(labels = percent_format(), breaks = seq(0,1,0.25)) + 
    theme(legend.position = legend.position, 
          legend.direction = "vertical", 
          panel.grid.major.y = element_blank(), 
          axis.ticks.y = element_line()) + 
    coord_cartesian(xlim = c(0,1.15), ylim = c(0,1.15))
  
  return(p)
}
