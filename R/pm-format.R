#' Format numeric vectors
#' 
#' Format numeric vectors with localized decimal and big marks
#' 
#' @param x numeric vector 
#' @param ... further arguments passed to \code{\link{format}}
#' @export
pm_format <- function (x, trim = TRUE, scientific = FALSE, ...) {
  format(x, 
         trim = trim, 
         scientific = scientific,
         big.mark = ft.opts.get("big.mark", "render"), 
         decimal.mark = ft.opts.get("decimal.mark", "render"), 
         ...)
}