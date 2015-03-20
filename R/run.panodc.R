#' Run pandoc with options
#' 
#' Pandoc, written by John MacFarlane, converts files from one markup format into another. For more information see pandoc's website: \url{http://johnmacfarlane.net/pandoc/}. 
#' 
#' @param file input file for pandoc
#' @param output ouput file
#' @param form input file format, default "markdown"
#' @param to output file format, default "latex"
#' @param options string with options for pandoc, default "--chapters"
#' @param pandoc.path path to your pandoc executable, used if pandoc is not in your \code{PATH}
#' 
#' @export
run.pandoc <- function(
  file, 
  output = stdout(), 
  from = "markdown", 
  to = "latex", 
  options = "--chapters", 
  pandoc.path = NULL
){
  
  if (!is.null(pandoc.path))
    pandoc.exe <- file.path(pandoc.path, "pandoc.exe")
  else
    pandoc.exe <- normalizePath(system.file("Pandoc", "pandoc.exe", package = getPackageName()))
  
  cmd <- sprintf("%s -f %s -t %s %s -o %s %s", 
                 pandoc.exe, 
                 from, 
                 to, 
                 options, 
                 output, 
                 file)
  system(cmd)
  message("Pandoc: ", cmd)
  
}
