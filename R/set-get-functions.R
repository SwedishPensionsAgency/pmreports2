#' Set font size
#' 
#' @param size font size in pt
#' @family set functions 
#' @export
set_fontsize <- function (size) {
  assign("fontsize", as.numeric(size), envir = .GlobalEnv)
}


#' Set year
#' 
#' @param year year
#' @family set-get-functions
#' @export
set_year <- function (year) {
  assign(".year", year, envir = .GlobalEnv)
}

#' Get year
#' 
#' @family set-get-functions
#' @export
get_year <- function () {
  return(get(".year", envir = .GlobalEnv))
}

#' Set Language and Dependent Variables
#' 
#' Set language and decimal mark, big mark, and global options for format.tables (read, write, render). 
#' 
#' @param lang language
#' @family set-get-functions
#' 
#' @export
set_lang <- function (lang = c("sv", "en")) {
  
  lang <- match.arg(lang)
  
  if (lang == "sv") {
    big.mark = " "
    decimal.mark = ","
    dec = ","
    sep = ";"
    plot.width = 13
    plot.height = 7.5
  } else if (lang == "en") {
    big.mark = ","
    decimal.mark = "."
    dec = "."
    sep = ","
    plot.width = 13.6
    plot.height = 7.5
  } else {
    big.mark = ","
    decimal.mark = "."
    dec = "."
    sep = ","
    plot.width = 13.6
    plot.height = 7.5
  }
  
  assign(".lang", lang, envir = .GlobalEnv)
  assign(".decimal.mark", decimal.mark, envir = .GlobalEnv)
  assign(".big.mark", big.mark, envir = .GlobalEnv)
  
  if (require(format.tables)) {
    table.template <- system.file("template", "ctable-pm.whisker", package = "format.tables")
    row.template <- system.file("template", "tex.rows-pm.yaml", package = "format.tables")
    
    ft.opts.set(read = list(sep = sep, dec = dec), 
                write = list(sep = sep, dec = dec), 
                render = list(table.template = table.template, 
                              row.template = row.template, 
                              decimal.mark = decimal.mark, 
                              big.mark = big.mark))
  } else {
    message("Please install the package format.tables devtools::install_github('SwedishPensionsAgency/format.tables') and rerun set_lang('", lang, "').")
  }
  
  
  set_plot_dim("width", plot.width)
  set_plot_dim("height", plot.height)
  
}


#' Get Language 
#'  
#' @family set-get-functions
#' 
#' @export
#' 
get_lang <- function () {
  return(get(".lang", envir = .GlobalEnv))
}


#' Set/get plot width and height
#' 
#' @param dim dimension of the plot, either "width" or "height"
#' @param value numeric, value to set the dimension to
#' 
#' @family plot dim
#' @rdname plot.dim
#' @export
set_plot_dim <- function (dim = c("width", "height"), value) {
  dim <- match.arg(dim)
  .pmreports.env$plot[[dim]] <- value
}


#' @family plot dim
#' @rdname plot.dim
#' @export
get_plot_dim <- function (dim = c("width", "height")) {
  dim <- match.arg(dim)
  return(.pmreports.env$plot[[dim]])
}

