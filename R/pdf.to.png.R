#' Convert PDF to PNG
#' 
#' Deprecated, use \code{format.plot::pdf_to_png} instead. Converts a PDf file to PNG with Ghostscript
#' 
#' @param file a character string giving the name of the original pdf file
#' @param outfile the name of the new png file including ".png" extension
#' @param fontpaths a character vector giving directories that Ghostscript will search for fonts
#' @param resolution target resolution of the png file, default is 600
#' @param options a character string containing further options to Ghostscript
#' 
#' @export
#' 
#' 
pdf.to.png <- function(
  file, 
  outfile, 
  fontpaths = system.file("fonts", package = getPackageName()), 
  resolution = 600, 
  options = "") {
  warning("The function 'pdf.to.png' is deprecated. Please use format.plot::pdf_to_png instead.")
  format.plot::pdf_to_png(file = file, outfile = outfile, fontpaths = fontpaths, resolution = resolution, options = options)
}
