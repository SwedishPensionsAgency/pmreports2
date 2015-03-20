#' Percent format
#' 
#' Convert number to percent format for labels in ggplot
#' 
#' @family format
#' @export
percent_format <- function () 
{
  function(x) {
    x <- round_any(x, precision(x)/100)
    str_c(format(x * 100, big.mark = .big.mark, decimal.mark = .decimal.mark, scientific = FALSE, trim = TRUE), " %")
  }
}

#' Precision function
#' @family format
precision <- function(x) {
  rng <- range(x, na.rm = TRUE)
  
  span <- if (zero_range(rng)) rng[1] else diff(rng)
  10 ^ floor(log10(span))
}
