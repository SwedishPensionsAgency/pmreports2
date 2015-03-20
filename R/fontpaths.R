#' Returns th fontpaths used for Ghostscript
#' 
#' @export
fontpaths <- function () {
  return(system.file("fonts", package = getPackageName()))
}
