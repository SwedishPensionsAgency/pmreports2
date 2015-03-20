#' Convert PDF to PNG
#' 
#' Converts a PDf file to PNG with Ghostscript
#' 
#' @param file a character string giving the name of the original pdf file
#' @param outfile the name of the new png file including ".png" extension
#' @param fontpaths a character vector giving directories that Ghostscript will search for fonts
#' @param resolution target resolution of the png file, default is 600
#' @param options a character string containing further options to Ghostscript
#' 
#' @export
pdf.to.png <- function(
  file, 
  outfile, 
  fontpaths = system.file("fonts", package = getPackageName()), 
  resolution = 600, 
  options = ""){
  tmpfile <- tempfile("Rpng")
  cmd <- paste0(system.file("gs", "bin", "gswin32.exe", package = getPackageName()), 
                " ", "-dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -r", resolution, " -dTextAlphaBits=4 -dGraphicsAlphaBits=4",
                " ", shQuote(paste0("-sFONTPATH=", paste(fontpaths, collapse = .Platform$path.sep))), 
                " ", options, 
                " ", "-sOutputFile=", tmpfile, 
                " ", shQuote(file))
  ret <- system(cmd)
  if (ret != 0) 
    stop(gettextf("status %d in running command '%s'", ret, 
                  cmd), domain = NA)
  file.copy(tmpfile, outfile, overwrite = TRUE)
}