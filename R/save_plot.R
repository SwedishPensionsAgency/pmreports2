#' Save plot as pdf to ./figure directory
#' 
#' Save plot as pdf to ./figure directory and output latex code 
#' 
#' @param plot plot object
#' @param figurename name of the figuer without extension
#' @param width width of the figure in cm
#' @param height height of the figure in cm
#' @param caption caption string, used in latex output
#' @param subcaption subcaption string, used in latex output
#' @param tot.caption caption string for the table of tables, used in latex output
#' @param notes notes that should be printed below the figure, used in latex output
#' @param colormodel colormodel of the pdf, default for print is \code{cmyk}
#' @param extension file extension, default is "pdf"
#' @param path path to the directory where the figure should be saved, relative to the manuscript. \code{../} is not allowed: TeX does not handle points in paths
#' @param fix.labels should the margins for the labels be adjusted? Used for \code{\link{directlabels}} with \code{last.bumpup}
#' @param embed.fonts embed fonts in pdf? Default is TRUE
#' @param translate logical, should the plot be translated before saving?
#' @param ... arguments passed to \code{\link{pdf}}
#' 
#' @export 

save_plot <- function(
  plot, 
  figurename, 
  width = get_plot_dim("width"), 
  height = get_plot_dim("height"), 
  caption = NULL, 
  subcaption = NULL, 
  tot.caption = NULL, 
  notes = NULL, 
  colormodel = 'cmyk', 
  extension = "pdf", 
  path = "figure", 
  fix.labels = FALSE,
  embed.fonts = TRUE, 
  translate = TRUE, 
  ...){
  
  if (get_lang() != "sv") {
    path <- file.path(path, get_lang())
  }
  
  if (is.null(tot.caption) && !is.null(caption)) {
    tot.caption = caption
  }
  
  if (fix.labels){
    plot <- fix_labels(plot)
  }
  
  x.fp <- format.plot(plot = plot, 
                      name = figurename, 
                      path = path, 
                      data = list(caption = caption, 
                                  captionTableOfContents = tot.caption,
                                  subcaption = subcaption, 
                                  note = notes), 
                      devices = c("pdf", "png"), 
                      plot.options = list(colormodel = colormodel, 
                                          width = width/2.54, 
                                          height = height/2.54, 
                                          ...), 
                      gs = ghostscript_path(), 
                      fontpaths = fontpaths())
  
  if (translate && get_lang() != "sv") {
    x.fp <- translate_object(copy_object(x.fp), 
                             object.name = figurename, 
                             target.language = get_lang(), 
                             skip = c("objname", "default_stat", "default_aes", "default_pos", "required_aes", "default_geom", "colour", "color", "call", "aesthetics", "scale_name", "range", "trans", "guide", "lineend", "fill", "family", "face", "legend.position", "legend.justification", "coordinates", "xmin", "ymin", "xmax", "ymax", "gs", "fontpaths", "plot.options", "devices", "path", "name"))
  }
  
  cat(x.fp$render())

  
#   
#   full.path <- normalizePath(path, mustWork = FALSE)
#   
#   if (!file_test("-d", full.path))
#     dir.create(full.path, recursive = TRUE)
#   
#   
#   subcap.newline <- ""
#   if (caption != "" && subcaption != ""){
#     subcap.newline <- "\\nl{}"
#   }
#   if (subcaption != ""){
#     subcaption <- sprintf("{\\mdseries\\color{black} %1s}", latex_escape(subcaption))
#   }
#   
#   caption.out <- ""
#   
#   if (caption != "" | subcaption != ""){
#     caption.out <- paste(latex_escape(caption), subcap.newline, subcaption, sep="")
#   }
#   
#   tot.caption.out <- ""
#   # if caption for table of tables (tot.caption) is given then use it, else use caption if present
#   if (tot.caption != ""){
#     tot.caption.out <- latex_escape(tot.caption)
#   }else if (caption != ""){
#     tot.caption.out <- latex_escape(caption)
#   }
#   
#   file <- paste(figurename, extension, sep=".")
#   outfile <- normalizePath(file.path(full.path, file), mustWork=FALSE)
#   
#   pdf(file = outfile, width = width/2.54, height = height/2.54, colormodel = colormodel, ...)
#   #on.exit(capture.output(dev.off()))
#   
#   
#   if (fix.labels){
#     fix_labels(plot)
#     #temp <- capture.output(grid.draw(plot))
#   }else{
#     temp <- capture.output(print(plot))
#   }
#   
#   invisible()
#   
#   temp <- capture.output(dev.off())
#   
# 
#   # convert pdf to png
#   pdf.to.png(outfile, normalizePath(paste0(file_path_sans_ext(outfile), ".png"), mustWork=FALSE))
#   
#   
#   # embed fonts in pdf
#   if (embed.fonts && extension == "pdf") {
#     embedFonts(file = outfile, 
#                format = "pdfwrite", 
#                outfile = outfile, 
#                fontpaths = system.file("fonts", package = getPackageName()))
#   }
#   
#   
#   # output latex code
#   cat(sprintf("\\figurepm{%s}{%s}{%s}{%s}{%s}\n", figurename, file.path(path, file), caption.out, tot.caption.out, notes))
}
