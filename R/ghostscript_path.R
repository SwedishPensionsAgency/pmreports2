#' ghostscript path
#' 
#' Provide the path to the Ghostscript executable.
#' 
#' @export

ghostscript_path <- function() {
  if (require(pmbundle)) {
    return (pmbundle::ghostscript_path())
  } else {
    return(Sys.which("gs"))
  }
}