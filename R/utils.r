#' @export
margin_width <- function(x){
  tmp.pdf <- tempfile("Rpdf")
  pdf(tmp.pdf)
  width <- max(strwidth(unique(x), units = "inches", family="Neo Sans Pro", cex=fontsize/12, font=1))
  dev.off()
  return(width)
} 

#' @export
margin_height <- function(x){
tmp.pdf <- tempfile("Rpdf")
pdf(tmp.pdf)
height <- max(strheight(unique(x), units = "inches", family="Neo Sans Pro", cex=fontsize/12, font=1))
dev.off()
return(height)
} 


#' @export
custom_dl <- function(method) geom_dl(method = list(method, fontfamily="Neo Sans Pro", fontface="plain", cex=fontsize/12))

#' @export
custom_vline <- function(x) geom_vline(xintercept = x, linetype = "longdash", colour = pm_colors()[10], size = 0.25)

#' @export
space <- function(x, ...) format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)

#' @export
custom_hline <- function(x) geom_hline(yintercept = x, linetype = "longdash", colour = pm_colors()[10], size = 0.25)

#' @export
.get_label <- function(x, data = .notes()) data[Id == x & year == .year]$label

#' @export
.get_table_note <- function(x, data = .notes()) data[Id == x & year == .year]$notes

#' @export
sum.aggr.na <- function(x) {
  if(length(x) == 1 && is.na(x)){
    sum(x, na.rm = FALSE)
  }else{
    sum(x, na.rm = TRUE)
  }
}

#' @export
fix_labels <- function(p) {
  p <- ggplot_gtable(ggplot_build(p))
  p$layout$clip[p$layout$name=="panel"] <- "off"
  return(p)
}

#' @export
import_csv <- function(file, lang) {
  x <- read.csv2(file, stringsAsFactors = FALSE)
  x <- as.data.table(melt(x))
  x$year <- substring(x$variable, 2, 5)
  x$label <- x[[paste("Label", lang, sep = "_")]]
  x$notes <- x[[paste("Notes", lang, sep = "_")]]
  return(x)
}

#' @export
custom_aggr_by <- function(data,
                           path = "Id", 
                           labels = c("label", "notes"), 
                           dims = c("year"), 
                           metrics = c("value"),
                           to_levels = TRUE,
                           grand_label = .grand_label,
                           ...) {
  
  aggr_by(data = data, 
          path = path,
          labels = labels,
          dims = dims,
          metrics = metrics,
          to_levels = to_levels,
          grand_label = grand_label,
          ...)
}

#' @export
by_child_table <- function(data, ..., margins = cast_col) {
  this_year <- custom_aggr_by(data[year == .year], by_child = TRUE, to_levels = FALSE, ...)
  colnames(this_year)[ncol(this_year)] <- paste(.grand_label, .year)
  sort_key <- 1:nrow(this_year)
  
  prev_year <- custom_aggr_by(data[year == .year - 1], by_child = TRUE, to_levels = FALSE, ...)
  colnames(prev_year)[ncol(prev_year)] <- paste(.grand_label, .year - 1)
  
  x <- merge(this_year, prev_year[, c(1:3, ncol(prev_year))], by.x = c("Id", "label", "notes"), all = FALSE, sort = FALSE)
  x$Id <- id_to_levels(x$Id)
  return(x)
}
