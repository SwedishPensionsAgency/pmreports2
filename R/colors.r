#' Get pm colors
#' 
#' Functions to get color series
#' @family color functions
#' @rdname pm-colors
#' @aliases pm_colors
#' @export
pm_colors_cmyk <- function() {
  c(
    cmyk2hex("0-35-100-0") # PMS 1235 C
     , cmyk2hex("0-55-100-0") # PMS 144 C
     , cmyk2hex("3-80-90-0") # PMS 1665 C
     , cmyk2hex("22-100-85-15") # PMS 187 C
     , cmyk2hex("25-100-0-0") # PMS 240 C
     , cmyk2hex("62-16-0-0") # PMS 292 C
     , cmyk2hex("95-65-20-10") # PMS 647 C
     , cmyk2hex("35-20-100-7") # PMS 398 C
     , cmyk2hex("65-40-100-35") # PMS 378 C
     , cmyk2hex("29-26-30-6") #PMS Warm Gray 4 C
     , cmyk2hex("0-17-34-62") #PMS Warm Gray 11 C
     , cmyk2hex("0-17-34-62") #PMS Warm Gray 11 C
     , cmyk2hex("0-0-0-100") #black
  )
}

#' @rdname pm-colors
#' @export
pm_colors_rgb <- function() {
  c(
    rgb(255, 183, 15,max = 255) # PMS 1235 C
    , rgb(239, 130, 0, max = 255) # PMS 144 C
    , rgb(227, 73, 18, max = 255) # PMS 1665 C
    , rgb(172, 26, 47, max = 255) # PMS 187 C
    , rgb(196, 38, 149, max = 255) # PMS 240 C
    , rgb(94, 176, 230, max = 255) # PMS 292 C
    , rgb(18, 86, 135, max = 255) # PMS 647 C
    , rgb(175, 165, 0, max = 255) # PMS 398 C
    , rgb(84, 95, 29, max = 255) # PMS 378 C
    , rgb(185, 177, 169, max = 255) #PMS Warm Gray 4 C
    , rgb(185, 177, 169, max = 255) #PMS Warm Gray 4 C
    , rgb(185, 177, 169, max = 255) #PMS Warm Gray 4 C
    , rgb(0, 0, 0, max = 255) #black
  )
}

#' Convert colors
#' 
#' Functions to convert between CMYK, RGB and HEX.
#' 
#' @family color functions
#' @rdname convert-colors
#' @export
rgb2cmyk <- function(rgb, max=255){
  cmy <- 1 - rgb/max
  names(cmy) <- c("c", "m", "y")
  k <- min(min(min(1, cmy[1]), cmy[2]), cmy[3])
  if(k == 1) c(c=0, m=0, y=0, k = 1)*100
  else c(c(cmy - k)/(1-k), k = k)
}

#' @rdname convert-colors
#' @export
cmyk2rgb <- function(cmyk, max=100){
  rgb <- cmyk[1:3]/max
  names(rgb) <- c("r", "g", "b")
  k <- cmyk[4]/max
  ((1-rgb) * (1-k) *255)
}

#' @rdname convert-colors
#' @export
cmyk2hex <- function(cmyk, alpha, max=100){
  this.rgb <- round(cmyk2rgb(as.numeric(unlist(strsplit(cmyk, "-"))), max))
  if (missing(alpha)){
    rgb(this.rgb[1], this.rgb[2], this.rgb[3], maxColorValue=255)
  }else{
    rgb(this.rgb[1], this.rgb[2], this.rgb[3], alpha=alpha/max*255, maxColorValue=255)
  }
}

#' Get pm colors
#' 
#' Functions to get color series
#' @family color functions
#' @rdname pm-colors
#' @export
pm_colors <- function(){pm_colors_cmyk()}



#' Plot pm colors 
#' 
#' @family color functions
#' @rdname pm-colors
#' @export
pm_colormap <- function () {
  color.df <- data.frame(x = letters[1:length(pm_colors())], 
                         y = rep(1, length(pm_colors())))
  rgb.colors <- as.data.frame(t(col2rgb(pm_colors_rgb())))
  rgb.names <- character(nrow(rgb.colors))
  for (i in 1:nrow(rgb.colors)) {
    rgb.names[i] <- paste("RGB:", paste(rgb.colors[i, ], collapse = "-"))
  }
    
  pms.names <- c("PMS 1235 C"
                 , "PMS 144 C"
                 , "PMS 1665 C"
                 , "PMS 187 C"
                 , "PMS 240 C"
                 , "PMS 292 C"
                 , "PMS 647 C"
                 , "PMS 398 C"
                 , "PMS 378 C"
                 , "PMS Warm Gray 4 C"
                 , "PMS Warm Gray 11 C"
                 , "PMS Warm Gray 11 C"
                 , "black")
  cmyk.names <- c("0-35-100-0" 
                  , "0-55-100-0" 
                  , "3-80-90-0" 
                  , "22-100-85-15" 
                  , "25-100-0-0" 
                  , "62-16-0-0" 
                  , "95-65-20-10" 
                  , "35-20-100-7" 
                  , "65-40-100-35" 
                  , "29-26-30-6" 
                  , "0-17-34-62" 
                  , "0-17-34-62" 
                  , "0-0-0-100")
    
  p <- ggplot(color.df, aes(fill = x)) + 
    geom_rect(aes(xmin = as.numeric(x)-1, xmax = as.numeric(x)-0.1, ymin = 0, ymax = 1)) +
    scale_x_continuous(breaks = seq(0.45, length(pm_colors()), 1), 
                       labels = paste(paste("Index:", 1:length(pm_colors())), pms.names, paste("CMYK:", cmyk.names), paste("HEX:", pm_colors()), rgb.names, sep="\n")) + 
    coord_cartesian(ylim = c(0, 1)) + 
    ggtitle("Colors Swedish Pensions Agency") + 
    scale_fill_manual(values = as.character(pm_colors()))
  
  p + custom_theme() + 
    theme(legend.position = "none", 
          axis.ticks = element_blank(), 
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1), 
          axis.text.y = element_blank(), 
          panel.grid = element_blank())
  
}
