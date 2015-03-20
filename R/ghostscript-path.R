#' Returns the path to the Ghostscript executable
#' 
#' Returns the path to the Ghostscript executable bundled with the package on Windows, on other systems it returns the value of \code{Sys.which("gs")}.
#' 
#' @export
ghostscript_path <- function () {
  if (.Platform$OS.type == "windows" && .Platform$r_arch == "x64") {
    return(system.file("gs", "bin", "gswin64.exe", package = getPackageName()))
  } else if (.Platform$OS.type == "windows") {
    return(system.file("gs", "bin", "gswin32.exe", package = getPackageName()))
  } else {
    return(Sys.which("gs"))
  }
}