#' Read format.table, translate it, and output latex/html code
#' 
#' @param file the name of the file which the data are to be read from or an object of class 'format.tables'
#' @param object.name 
#' @export
output_table <- function (file, object.name = NULL) {
  if ("format.tables" %in% class(file)) {
    x <- file
  } else {
    x <- format.tables(file = file)
  }
  
  if (!is.null(object.name) && is.null(x$header$label)) {
    x$header$label = paste0("tab:", object.name)
  }
  if (get_lang() == "en") {
    format.tables.skip <- c("styles", "names.style", "label")
    x <- translate_object(object = copy_object(x), object.name = object.name, skip = format.tables.skip)
  }
  return(x$render())
}